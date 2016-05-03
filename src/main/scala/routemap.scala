package starcolon.flights.routemap

import starcolon.flights.openflights._
import starcolon.flights.geo._

/**
 * Route mapper/finder utility
 */
object RouteMap {
  /**
   * Find all airports located in a particular city
   */
  def findAirports(city: String): List[Airport] = {
    if (OpenFlights.cityAirports.contains(city))
      return OpenFlights.cityAirports(city)
    else
      return List[Airport]()
  }

  /**
   * Find all routes between two cities
   */
  def findCityRoutes(citySrc: String, cityDest: String): List[GeoRoute] = {
    val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
    val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

    srcAirports.foldLeft(List[GeoRoute]()) { (route, src) =>
      route ++ dstAirports.foldLeft(List[GeoRoute]()) { (_route, dst) =>
        val r = findAirportRoutes(src, dst)
        _route ++ r
      }
    }
  }

  /**
   * Find indirect routes which connect two cities
   */
  def findCityIndirectRoutes(citySrc: String, cityDest: String, maxDegree: Int): List[List[GeoRoute]] = {
    val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
    val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

    // Examine each pair of the src-dst airports
    srcAirports.foldLeft(List[List[GeoRoute]]()) { (route, src) =>
      route ++ dstAirports.foldLeft(List[List[GeoRoute]]()) { (_route, dst) =>
        val rs = findIndirectAirportRoutes(src, dst, maxDegree)
        _route ++ rs
      }
    }
  }

  /**
   * Find all direct routes between two airports
   */
  def findAirportRoutes(airportSrc: Airport, airportDest: Airport): List[GeoRoute] = {

    if (OpenFlights.routes.contains(airportSrc.code) &&
      OpenFlights.routes(airportSrc.code).contains(airportDest.code))
      return OpenFlights.routes(airportSrc.code)(airportDest.code)
        .map(r => GeoRoute(
          r,
          airportSrc.lat, airportSrc.lng,
          airportDest.lat, airportDest.lng
        ))
    else
      return List[GeoRoute]()
  }

  /**
   * Find all indirect routes between two airports
   * There is at least one connecting airport between the two.
   */
  def findIndirectAirportRoutes(airportSrc: Airport, airportDest: Airport, maxDegree: Int): List[List[GeoRoute]] = {

    // Perform a **greedy** depth-first-search
    return findConnectingRoutes(
      airportSrc,
      airportDest,
      maxDegree,
      List()
    )
  }

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
  private def findConnectingRoutes(airportSrc: Airport, airportDest: Airport, maxDegree: Int, prevRoute: List[GeoRoute]): List[List[GeoRoute]] = {
    // Stopping criterion
    if (maxDegree <= 0)
      return List()

    // TAOTODO: Other stopping criteria:
    // - Too large total distance
    // - Too large one single distance of a flight

    println(Console.CYAN + "==================================" + Console.RESET)
    println(Console.CYAN + s"[Max ${maxDegree} degrees left]..." + Console.RESET)
    println(Console.GREEN + "Expanding route from: " + prevRoute.map(_.route.airportSourceCode).mkString(" ➡️ ") +
      " " + airportSrc.code + Console.RESET)
    println(Console.CYAN + "==================================" + Console.RESET)

    // Expand all routes which start at the source airport
    // (Depth-first)
    val routesFromSrc = OpenFlights.routes(airportSrc.code)

    routesFromSrc.foldLeft(List[List[GeoRoute]]()) { (allRoutes, n) =>

      val (nextDest, routes) = n
      var allRoutes_ = List[List[GeoRoute]]()

      // Skip backward routes or invalid routes
      if (OpenFlights.airports.contains(nextDest) && nextDest != airportSrc.code) {
        val nextDestAirport = OpenFlights.airports(nextDest)

        println(airportSrc.code + " to " + nextDest + s" ${routes.length} routes")

        // These geolocations are useful for future implementation
        val (lat0, lng0) = (airportSrc.lat, airportSrc.lng)
        val (lat1, lng1) = (nextDestAirport.lat, nextDestAirport.lng)

        // Extend the previous routes with the expanded route list

        // ENHANCEMENT: List is not good at appending
        // Needs to find a better list-like structure
        val nextRoutes = routes.map(r =>
          prevRoute ++ List(GeoRoute(r, lat0, lng0, lat1, lng1)))

        // Finally reaches the final destination?        
        if (nextDest == airportDest.code) {
          println(Console.YELLOW + "Terminal found!" + Console.RESET)
          allRoutes_ = nextRoutes ++ allRoutes
        } else {
          // Find further routes starting from
          // the current landing airport (nextDest)
          val nextRoutes_ = nextRoutes.flatMap(rs => findConnectingRoutes(
            nextDestAirport,
            airportDest,
            maxDegree - 1,
            rs
          ))

          // Expanded routes could not be empty
          // otherwise, they never end at our final destination airport.
          if (nextRoutes.length > 0)
            allRoutes_ = nextRoutes_ ++ allRoutes
        }
      }

      allRoutes_
    }
  }
}
