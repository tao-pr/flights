package starcolon.flights.openflights

import starcolon.flights.geo.Geo
import scala.collection.mutable.Map

case class Airline (id: Long, code: String, name: String, country: String){
  def prettyPrint(): Unit = {
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
}

case class Airport (code: String, name: String, city: String, country: String, lat: Float, lng: Float){
  def prettyPrint(): Unit = {
    println("🏠 " + Console.CYAN + name + " (" + code + ") " + 
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport() = code.length>0
  def isIn(_city: String) = city==_city
  def isInCountry(_cn: String) = country==_cn
}

case class Route (airlineCode: String, airportSourceCode: String, airportDestCode: String, numStops: Int){
  def prettyPrint(): Unit = {
    println(Console.CYAN + airlineCode + " ✈️ " + 
      Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " + 
      Console.WHITE + numStops.toString + " stops" + Console.RESET)
  }

  def startsAt(_airportCode: String) = airportSourceCode == _airportCode
  def endsAt(_airportCode: String) = airportDestCode == _airportCode
  def isBetween(_srcAirportCode: String, _dstAirportCode: String):Boolean = {
    airportSourceCode == _srcAirportCode && 
    airportDestCode == _dstAirportCode
  }
  def isOperatedBy(_airline: String) = airlineCode==_airline
}

case class RouteKey(airportSourceCode: String, airportDestCode: String)

/**
 * Another version of Route class with geolocations
 */
case class GeoRoute(route: Route, srcLat: Float, srcLng: Float, dstLat: Float, dstLng: Float){
  // Distance in metres between the source airport
  // and the destination airport
  def distance() = Geo.distance(srcLat, srcLng, dstLat, dstLng)
}

/**
 * OpenFlights data source handler
 */
object OpenFlights {
  /**
   * A mapping of two-letter airline codes to the airlines that have used that
   * code. It is possible for the codes of defunct airlines to be reassigned.
   */
  lazy val airlines: Map[String, List[Airline]] = loadAirlinesAsMap("/data/openflights/airlines.dat")

  /**
   * A mapping of city names to the [[Airport]]s located in that city.
   */
  lazy val airports: Map[String, List[Airport]] = loadAirportsAsMap("/data/openflights/airports.dat")

  /**
   * A mapping of [[Route]]s keyed by their (source, destination) pairs.
   */
  lazy val routes: Map[RouteKey, List[Route]] = loadRoutesAsMap("/data/openflights/routes.dat")

  private def loadCSVDataFile(path: String): List[Array[String]] = {
    val source = io.Source.fromURL(getClass.getResource(path))
    val records = source.getLines.map { line => line.split(",").map(_.trim) }.toList
    source.close()
    records
  }

  private def loadAirlinesAsMap(path: String): Map[String, List[Airline]] = {
    val records = loadCSVDataFile(path)
    val index = Map.empty[String, List[Airline]].withDefaultValue(List())

    records.foldLeft(index) { (index, n) =>
      // Airline code as a key
      val code    = n(4).replace("\"","")
      var airline = Airline(
        n(0).toLong,
        n(4).replace("\"",""),
        n(1).replace("\"",""),
        n(6).replace("\"","")
      )

      index(code) = airline +: index(code)
      index
    }
  }

  private def loadAirlines(path: String): List[Airline] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(List[Airline]()) { (list, n) =>
      list ++ List(Airline(
        n(0).toLong,
        n(4).replace("\"",""),
        n(1).replace("\"",""),
        n(6).replace("\"","")
      ))
    }
  }

  private def loadAirportsAsMap(path: String): Map[String, List[Airport]] = {
    val records = loadCSVDataFile(path)
    val index = Map.empty[String, List[Airport]].withDefaultValue(List())

    records.foldLeft(index) { (index, n) =>
      val name = n(1).replace("\"", "")

      // In case the city has a comma, it will be unintentionally split so we
      // have one extra element in the split array.
      val airport = if (n.length > 12) {
        val city    = s"${n(2)}, ${n(3)}".replace("\"", "")
        val country = n(4).replace("\"", "")
        val code    = n(5).replace("\"", "")
        val lat     = n(7).toFloat
        val lng     = n(8).toFloat

        Airport(code, name, city, country, lat, lng)
      } else {
        val city    = n(2).replace("\"", "")
        val country = n(3).replace("\"", "")
        val code    = n(4).replace("\"", "")
        val lat     = n(6).toFloat
        val lng     = n(7).toFloat

        Airport(code, name, city, country, lat, lng)
      }

      val city = airport.city
      index(city) = airport +: index(city)
      index
    }
  }

  private def loadAirports(path: String): List[Airport] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(List[Airport]()) { (list, n) =>
      if (n.length>12)
        // In case the city has a comma,
        // it will be unintentionally splitted so we have
        // one extra element in the splitted array.
        list ++ List(Airport(
          n(4).replace("\"",""),
          n(1).replace("\"",""),
          (n(2)+", "+n(3)).replace("\"",""),
          n(4).replace("\"",""),
          n(7).toFloat,
          n(8).toFloat
        ))
      else
        list ++ List(Airport(
          n(4).replace("\"",""),
          n(1).replace("\"",""),
          n(2).replace("\"",""),
          n(3).replace("\"",""),
          n(6).toFloat,
          n(7).toFloat
        ))
    }
  }

  private def loadRoutesAsMap(path: String): Map[RouteKey, List[Route]] = {
    val records = loadCSVDataFile(path)
    val index = Map.empty[RouteKey, List[Route]].withDefaultValue(List())

    records.foldLeft(index) { (index, n) =>
      var src      = n(2).replace("\"", "")
      var dst      = n(4).replace("\"", "")

      // Both src and dst airport codes as a key
      var routeKey = RouteKey(src, dst)
      var route    = Route(
        n(0).replace("\"", ""),
        src,
        dst,
        n(7).toInt
      )

      index(routeKey) = route +: index(routeKey)
      index
    }
  }

  private def loadRoutes(path: String): List[Route] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(List[Route]()) { (list, n) => 
      list ++ List(Route(
        n(0).replace("\"",""),
        n(2).replace("\"",""),
        n(4).replace("\"",""),
        n(7).toInt
      ))
    }
  }
}
