package starcolon.flights.routemap

// REVIEW: Following database dependencies should be disguised
import slick.driver.H2Driver.api._
import slick.lifted.Query
import scala.concurrent.ExecutionContext.Implicits.global

import starcolon.flights.database._
import starcolon.flights.geo._
import starcolon.flights.rawdata.RawDataset._

/**
 * Geospatial route which connects two airports.
 */
case class GeoRoute(route: Route, srcLat: Float, srcLng: Float, dstLat: Float, dstLng: Float) {
  // Distance in metres between the source airport
  // and the destination airport
  def distance() = Geo.distance(srcLat, srcLng, dstLat, dstLng)

  def prettyPrint(prefix: String = "") {
    println(prefix + Console.CYAN + route.airlineCode + " ✈️ " +
      Console.GREEN + route.airportSourceCode + " ➡️ " + route.airportDestCode + " " +
      Console.WHITE + route.numStops.toString + " stops " +
      Console.YELLOW + distance() / 1000 + " km" +
      Console.RESET)
  }
}

/**
 * Route mapper/finder utility
 */
object RouteMap {

  /**
   * Find all routes between two cities
   */
  def findCityRoutes(citySrc: String, cityDest: String): List[GeoRoute] = {

    // Execute queries and reap the collection of results
    val srcAirports = OpenFlightsDB.findAirports(citySrc)
    val dstAirports = OpenFlightsDB.findAirports(cityDest)

    for (src <- srcAirports)
      yield GeoRoute()

    // for (src <- srcAirports) {
    //   for (dst <- dstAirports) {
    //     val routes = findAirportRoutes(src, dst)
    //     for (r <- routes)
    //       yield r
    //   }
    // }
  }

  /**
   * Find indirect routes which connect two cities
   */
  // def findCityIndirectRoutes(citySrc: String, cityDest: String, maxDegree: Int): List[List[GeoRoute]] = {
  //   val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
  //   val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

  //   // Examine each pair of the src-dst airports
  //   srcAirports.foldLeft(List[List[GeoRoute]]()) { (route, src) =>
  //     route ++ dstAirports.foldLeft(List[List[GeoRoute]]()) { (_route, dst) =>
  //       val rs = findIndirectAirportRoutes(src, dst, maxDegree)
  //       _route ++ rs
  //     }
  //   }
  // }

  /**
   * Find all direct routes between two airports
   */
  def findAirportRoutes(airportSrc: Airports, airportDest: Airports): List[GeoRoute] = {

    // if (OpenFlights.routes.contains(airportSrc.code) &&
    //   OpenFlights.routes(airportSrc.code).contains(airportDest.code))
    //   return OpenFlights.routes(airportSrc.code)(airportDest.code)
    //     .map(r => GeoRoute(
    //       r,
    //       airportSrc.lat, airportSrc.lng,
    //       airportDest.lat, airportDest.lng
    //     ))
    // else
    return List[GeoRoute]()
  }

  /**
   * Find all indirect routes between two airports
   * There is at least one connecting airport between the two.
   */
  // def findIndirectAirportRoutes(airportSrc: Airport, airportDest: Airport, maxDegree: Int): List[List[GeoRoute]] = {

  //   // Perform a **greedy** depth-first-search
  //   return findConnectingRoutes(
  //     airportSrc,
  //     airportDest,
  //     maxDegree,
  //     List()
  //   )
  // }

  /**
   * Calculate a total distance in metre of the routes
   */
  private def totalRouteDistance(routes: List[GeoRoute]): Float = {
    routes.map(_.distance).sum
  }

  /**
   * Find routes which connect two airports.
   * The function may return multiple possible routes which it finds.
   */
  // private def findConnectingRoutes(airportSrc: Airport, airportDest: Airport, maxDegree: Int, prevRoute: List[GeoRoute]): List[List[GeoRoute]] = {
  //   // Stopping criterion
  //   if (maxDegree <= 0)
  //     return List()

  //   // TAOTODO: Other stopping criteria:
  //   // - Too large total distance
  //   // - Too large one single distance of a flight

  //   println(Console.CYAN + "==================================" + Console.RESET)
  //   println(Console.CYAN + s"[Max ${maxDegree} degrees left]..." + Console.RESET)
  //   println(Console.GREEN + "Expanding route from: " + prevRoute.map(_.route.airportSourceCode).mkString(" ➡️ ") +
  //     " " + airportSrc.code + Console.RESET)
  //   println(Console.CYAN + "==================================" + Console.RESET)

  //   // Expand all routes which start at the source airport
  //   // (Depth-first)
  //   val routesFromSrc = OpenFlights.routes(airportSrc.code)

  //   routesFromSrc.foldLeft(List[List[GeoRoute]]()) { (allRoutes, n) =>

  //     val (nextDest, routes) = n
  //     var allRoutes_ = List[List[GeoRoute]]()

  //     // Skip backward routes or invalid routes
  //     if (OpenFlights.airports.contains(nextDest) && nextDest != airportSrc.code) {
  //       val nextDestAirport = OpenFlights.airports(nextDest)

  //       println(airportSrc.code + " to " + nextDest + s" ${routes.length} routes")

  //       // These geolocations are useful for future implementation
  //       val (lat0, lng0) = (airportSrc.lat, airportSrc.lng)
  //       val (lat1, lng1) = (nextDestAirport.lat, nextDestAirport.lng)

  //       // Extend the previous routes with the expanded route list

  //       // ENHANCEMENT: List is not good at appending
  //       // Needs to find a better list-like structure
  //       val nextRoutes = routes.map(r =>
  //         prevRoute ++ List(GeoRoute(r, lat0, lng0, lat1, lng1)))

  //       // Finally reaches the final destination?        
  //       if (nextDest == airportDest.code) {
  //         println(Console.YELLOW + "Terminal found!" + Console.RESET)
  //         allRoutes_ = nextRoutes ++ allRoutes
  //       } else {
  //         // Find further routes starting from
  //         // the current landing airport (nextDest)
  //         val nextRoutes_ = nextRoutes.flatMap(rs => findConnectingRoutes(
  //           nextDestAirport,
  //           airportDest,
  //           maxDegree - 1,
  //           rs
  //         ))

  //         // Expanded routes could not be empty
  //         // otherwise, they never end at our final destination airport.
  //         if (nextRoutes.length > 0)
  //           allRoutes_ = nextRoutes_ ++ allRoutes
  //       }
  //     }

  //     allRoutes_
  //   }
  // }
}
