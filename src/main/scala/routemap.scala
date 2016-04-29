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
  def findAirports(city: String): List[Airport] = {
    ///OpenFlights.airports.filter(_.isIn(city))
    if (OpenFlights.airports.contains(city))
      return OpenFlights.airports(city)
    else
      return List[Airport]()
  }

  /**
   * Find all cities available to fly in a country
   */
  // def findCities(country: String): List[String] = {
  //  OpenFlights.airports
  //    .filter(_.isInCountry(country))
  //    .map(_.city)
  //    .toSet
  //    .toList
  // }

  /**
   * Find all routes between two cities
   */
  def findCityRoutes(citySrc: String, cityDest: String): List[Route] = {
    val srcAirports = findAirports(citySrc).filter(_.isValidAirport)
    val dstAirports = findAirports(cityDest).filter(_.isValidAirport)

    srcAirports.foldLeft(List[Route]()) { (route,src) =>
      route ++ dstAirports.foldLeft(List[Route]()) { (_route,dst) => 
        val r = findAirportRoutes(src.code,dst.code)
        
        println(Console.YELLOW + src.code + Console.WHITE + " to " + Console.YELLOW + dst.code + Console.RESET)
        println(r.length + " routes")
        r foreach {n => n.prettyPrint}

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
    
    // TAOTODO:
  
    return List[Route]()
  } 

  /**
   * Find all routes between two airports
   */
  def findAirportRoutes(airportSrc: String, airportDest: String): List[Route] = {
    //OpenFlights.routes.filter(_.isBetween(airportSrc,airportDest))
    val key = RouteKey(airportSrc,airportDest)
    if (OpenFlights.routes.contains(key))
      return OpenFlights.routes(key)
    else
      return List[Route]()
  }

  /**
   * Find all routes operated by an airway
   */
  // def findAirwayRoutes(airway: String): List[Route] = {
  //  OpenFlights.routes.filter(_.isOperatedBy(airway))
  // }
}