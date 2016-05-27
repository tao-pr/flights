package flights

// REVIEW: Following database dependencies should be disguised
import slick.driver.H2Driver.api._
import slick.lifted.Query
import scala.Either
import scala.collection.immutable.{ Iterable }
import scala.language.postfixOps
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Link between two airports and its operating airlines
 */
case class AirportLink(sourceAirport: Airport, destAirport: Airport, airlines: List[String]) {
  def distance(): Float = {
    Geo.distance(sourceAirport.lat, sourceAirport.lng, destAirport.lat, destAirport.lng)
  }

  def prettyPrint(prefix: String) {
    val airlines_ = airlines.mkString(",")
    println(
      prefix +
        sourceAirport.city + " (" + sourceAirport.code + ") → " +
        destAirport.city + " (" + destAirport.code + ")" +
        Console.GREEN + " ✈ via: " +
        Console.RESET + airlines_
    )
  }
}

case class ConnectedRoutes(routes: Seq[AirportLink]) {

  /**
   * Concatenate two connected routes
   */
  def ++(that: ConnectedRoutes): ConnectedRoutes = {
    ConnectedRoutes(routes ++ that.routes)
  }

  /**
   * Prepend an airport link to the underlying connected routes
   * @return {ConnectedRoutes} resultant connected routes
   */
  def prependLink(that: AirportLink): ConnectedRoutes = {
    ConnectedRoutes(that +: routes)
  }

  /**
   * Compute the aggregated travelling distance
   * of the entire connected routes
   */
  def totalDistance(): Float = routes match {
    case Seq() => 0
    case _ => {
      val rh = routes.head
      val r_ = routes.tail
      rh.distance() + ConnectedRoutes(r_).totalDistance()
    }
  }

  /**
   * Compute the distance from the beginning airport
   * to the final airport of the connected routes
   */
  def displacement() = routes.length >= 2 match {
    case true => {
      val a0 = routes.head
      val a1 = routes.last
      Geo.distance(
        a0.sourceAirport.lat,
        a0.sourceAirport.lng,
        a1.destAirport.lat,
        a1.destAirport.lng
      )
    }
    case false => 0
  }

  def prettyPrint() {
    if (routes.length > 0) {
      val chainedSources = routes.map(r =>
        Console.YELLOW +
          r.sourceAirport.city + " (" + r.sourceAirport.code + ")" +
          Console.RESET).mkString(" → ")

      println(
        Console.CYAN + s"[${routes.length} hops] " +
          chainedSources + " → " +
          Console.YELLOW + routes.last.destAirport.city + " (" + routes.last.destAirport.code + ")" +
          Console.RESET
      )
      routes.foreach { _.prettyPrint("   ") }
    }
  }
}

/**
 * Route mapper/finder utility
 */
object RouteMap {

  /**
   * Find all routes between two cities
   */
  def findCityRoutes(citySrc: String, cityDest: String): Future[Seq[Route]] = {

    // Execute queries and reap the collection of results
    val srcAirports = OpenFlightsDB.findAirports(citySrc)
    val dstAirports = OpenFlightsDB.findAirports(cityDest)

    // Expand all routes which connect from
    // the source airport to any of the destination airports
    for {
      sources <- srcAirports
      dests <- dstAirports
    } yield sources.flatMap { (src) =>
      dests.flatMap { (dst) =>
        Await.result(OpenFlightsDB.findAirportRoutes(src.code, dst.code), 20 seconds)
      }
    }
  }

  /**
   * Find all routes which connect from the specified airport
   * and end at any of the given list of the destination airports
   */
  def findAirportRoutes(srcAirport: Airport, dstAirports: Seq[Airport]) = Future[Seq[Route]] {
    dstAirports flatMap { (dstAirport) =>
      val routes = OpenFlightsDB.findAirportRoutes(
        srcAirport.code,
        dstAirport.code
      )
      Await.result(routes, 20 seconds)
    }
  }

  /**
   * Find all chained routes which connect two cities
   */
  def findCityIndirectRoutes(citySrc: String, cityDest: String, maxConnection: Int): Seq[ConnectedRoutes] = {

    // Expand all airports residing in the source city
    val srcAirports = Await.result(OpenFlightsDB.findAirports(citySrc), 20 seconds)

    // Expand all airports residing in destination city
    val destAirports = Await.result(OpenFlightsDB.findAirports(cityDest), 20 seconds)

    // Calculate a sample straight distance from the source to the destination
    val straightDistance = findDistance(srcAirports.head, destAirports.head)

    // Expand all connected routes recursively
    srcAirports flatMap ((srcAirport) => {
      val routes = findIndirectRoutesFromAirport(
        srcAirport,
        cityDest,
        Set(),
        maxConnection,
        straightDistance
      )
      routes
    })
  }

  /**
   * Calculate the distance between the source and the destination airport
   * in metre
   */
  def findDistance(srcAirport: Airport, dstAirport: Airport): Float = {
    Geo.distance(srcAirport.lat, srcAirport.lng, dstAirport.lat, dstAirport.lng)
  }

  /**
   * Find all connected routes which start at the specified airport
   * and end at the specified city
   * where each of them should not be longer than the given
   * number of connections
   * @param {Airport} The airport to start from
   * @param {String} The final destination city we're looking for
   * @param {Set[String]} The list of cities want to skip (usuall cities we departed)
   * @param {Int} Maximum number of connections
   * @param {Float} Straight distance from the source city to the destination city (in metre)
   */
  def findIndirectRoutesFromAirport(srcAirport: Airport, cityFinalDest: String, skipCities: Set[String], maxConnection: Int, straightDistance: Float): Iterable[ConnectedRoutes] = {

    if (maxConnection <= 0)
      List()
    else {
      // Expand all routes which start at the specified airport
      val departures = Await.result(
        OpenFlightsDB.findDepartureRoutes(srcAirport.code),
        30 seconds
      )

      // Group all departure routes by destination airport
      departures
        .groupBy(_.airportDestCode)
        .flatMap {
          case (destAirportCode, routes) => expandRoutes(
            srcAirport,
            destAirportCode,
            cityFinalDest,
            skipCities,
            routes,
            maxConnection,
            straightDistance
          )
        }
    }
  }

  /**
   * Expand further routes beginning from the specified airport
   */
  private def expandRoutes(srcAirport: Airport, destAirportCode: String, cityFinalDest: String, skipCities: Set[String], routes: Seq[Route], maxConnection: Int, straightDistance: Float): Iterable[ConnectedRoutes] = {
    // TAOTODO: Ignore if the route will be extended 
    // even farther to the final city 
    // if we choose this route

    // Treat this as a single common route
    // operated by multiple airlines
    val airlines = routes.map(_.airlineCode).toList
    val destAirports = Await.result(
      OpenFlightsDB.findAirportByCode(destAirportCode),
      30 seconds
    )

    destAirports.length match {
      case 0 => List() // Unable to locate the destination airport, skip it
      case _ => {
        val destAirport = destAirports.head
        val link = AirportLink(srcAirport, destAirport, airlines)

        // The expansion ends if all these routes 
        // end up at the final destination city
        if (destAirport.city == cityFinalDest) {
          List(ConnectedRoutes(Seq(link)))
        } // Skip the city we don't want to land at
        else if (skipCities contains destAirport.city) {
          List()
        } // Skip if the distance of the link exceeds total straight distance
        // (Some margin is added for relaxation)
        else if (link.distance * 1.1 > straightDistance) {
          List()
        } else {
          // Add the current terminal cities to the exclude list
          val skipCities_ = skipCities + srcAirport.city

          // Expand further routes from the current landed airport
          val nextRoutes = findIndirectRoutesFromAirport(
            destAirport,
            cityFinalDest,
            skipCities_,
            maxConnection - 1,
            straightDistance
          )

          // Assembly all the expanded links
          nextRoutes.map(r => r.prependLink(link))
        }
      }
    }
  }

  /**
   * Find the city where the given route arrives at
   */
  def getDestCityOfRoute(route: Route): String = {
    val (source, dest) = Await.result(
      OpenFlightsDB.findCitiesConnectedByRoute(route),
      30 seconds
    )
    dest
  }

}
