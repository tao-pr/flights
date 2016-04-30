package starcolon.flights.openflights

import java.io.InputStream

import starcolon.flights.geo.Geo
import scala.collection.mutable.Map

case class Airline (id: Long, code: String, name: String, country: String){
  def prettyPrint(){
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
}

case class Airport (code: String, name: String, city: String, country: String, lat: Float, lng: Float){
  def prettyPrint(){
    println("🏠 " + Console.CYAN + name + " (" + code + ") " + 
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport() = code.length>0
  def isIn(_city: String) = city==_city
  def isInCountry(_cn: String) = country==_cn
}

case class Route (airlineCode: String, airportSourceCode: String, airportDestCode: String, numStops: Int){
  def prettyPrint(){
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
object OpenFlights{
  def loadCSVDataFile(path: String): List[Array[String]] = {
    val stream: InputStream = getClass.getResourceAsStream(path)
    val lines = io.Source.fromInputStream(stream).getLines
    val records = lines.map { line => line.split(",").map(_.trim) }
    records.toList
  }

  def loadAirlinesAsMap(path: String): Map[String,List[Airline]] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(Map[String,List[Airline]]()){ (map, n) => 
      // Airline code as a key
      val code    = n(4).replace("\"","")
      var airline = Airline(
        n(0).toLong,
        n(4).replace("\"",""),
        n(1).replace("\"",""),
        n(6).replace("\"","")
      )

      if (map.contains(code))
        map(code) = map(code).+:(airline)
      else
        map(code) = List(airline)

      map
    }
  }

  def loadAirlines(path: String): List[Airline] = {
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

  def loadAirportsAsMap(path: String): Map[String,List[Airport]] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(Map[String,List[Airport]]()){ (map,n) =>
      
      // City as a key
      val code    = if (n.length > 12)
        (n(2)+", "+n(3)).replace("\"","")
        else n(2).replace("\"","")
    
      val airport = if (n.length > 12) 
        Airport(
          n(4).replace("\"",""),
          n(1).replace("\"",""),
          (n(2)+", "+n(3)).replace("\"",""),
          n(4).replace("\"",""),
          n(7).toFloat,
          n(8).toFloat
        ) 
        else Airport(
          n(4).replace("\"",""),
          n(1).replace("\"",""),
          n(2).replace("\"",""),
          n(3).replace("\"",""),
          n(6).toFloat,
          n(7).toFloat
        )

      if (map.contains(code))
        map(code) = map(code).+:(airport)
      else 
        map(code) = List(airport)

      map
    }
  }

  def loadAirports(path: String): List[Airport] = {
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

  def loadRoutesAsMap(path: String): Map[RouteKey,List[Route]] = {
    val records = loadCSVDataFile(path)
    records.foldLeft(Map[RouteKey,List[Route]]()) { (map, n) => 
      var src      = n(2).replace("\"","")
      var dst      = n(4).replace("\"","")
      
      // Both src and dst airport codes as a key
      var routeKey = RouteKey(src,dst)
      var route    = Route(
        n(0).replace("\"",""),
        n(2).replace("\"",""),
        n(4).replace("\"",""),
        n(7).toInt
      )

      if (map.contains(routeKey))
        map(routeKey) = map(routeKey).+:(route)
      else
        map(routeKey) = List(route)
      map
    }
  }

  def loadRoutes(path: String): List[Route] = {
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

  val airlines = loadAirlinesAsMap("/data/openflights/airlines.dat")
  val airports = loadAirportsAsMap("/data/openflights/airports.dat")
  val routes   = loadRoutesAsMap("/data/openflights/routes.dat")
}
