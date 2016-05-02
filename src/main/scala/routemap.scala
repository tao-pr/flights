package starcolon.flights.routemap

import starcolon.flights.openflights._
import starcolon.flights.geo._

/**
 * Route mapper/finder utility
 */
object RouteMap{
  /**
   * Find all airports located in a particular city
   */
  def findAirports(city: String): List[Airport] = OpenFlights.airports(city)

  /**
   * Find all routes between two cities
   */
  def findCityRoutes(citySrc: String, cityDest: String): List[Route] = {
    val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
    val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

    srcAirports.foldLeft(List[Route]()) { (route,src) =>
      route ++ dstAirports.foldLeft(List[Route]()) { (_route,dst) => 
        val r = findAirportRoutes(src.code, dst.code)
        
        // println(Console.YELLOW + src.code + Console.WHITE + " to " + Console.YELLOW + dst.code + Console.RESET)
        // println(r.length + " routes")
        // r foreach {n => n.prettyPrint}
        _route ++ r
      }
    }
  }

  /**
   * Find indirect routes which connect two cities
   */
  def findCityIndirectRoutes(citySrc: String, cityDest: String, maxDegree: Int): List[Route] = {
    val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
    val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

    // Examine each pair of the src-dst airports
    srcAirports.foldLeft(List[Route]()) { (route,src) =>
      route ++ dstAirports.foldLeft(List[Route]()) { (_route,dst) =>
        val r = findIndirectAirportRoutes(src.code, dst.code, maxDegree)

        // println(Console.YELLOW + src.code + Console.WHITE + " to " + Console.YELLOW + dst.code + Console.RESET)
        // println(r.length + " routes")
        // r foreach {n => n.prettyPrint}

        _route ++ r
      }
    }
  
    return List[Route]()
  } 

  /**
   * Find all direct routes between two airports
   */
  def findAirportRoutes(airportSrc: String, airportDest: String): List[Route] =
    OpenFlights.routes(RouteKey(airportSrc, airportDest))

  /**
   * Find all indirect routes between two airports
   * There is at least one connecting airport between the two.
   */
  def findIndirectAirportRoutes(airportSrc: String, airportDest: String, maxDegree: Int): List[Route] = {

    // TAOTODO:
    return List[Route]()
  }
}
