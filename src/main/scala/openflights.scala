package starcolon.flights.openflights

import starcolon.flights.geo.Geo
import scala.collection.mutable.Map

case class Airline(id: Long, code: String, name: String, country: String) {
  def prettyPrint(): Unit = {
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
}

case class Airport(code: String, name: String, city: String, country: String, lat: Float, lng: Float) {
  def prettyPrint(): Unit = {
    println("🏠 " + Console.CYAN + name + " (" + code + ") " +
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport() = code.length > 0
  def isIn(_city: String) = city == _city
  def isInCountry(_cn: String) = country == _cn
}

case class Route(airlineCode: String, airportSourceCode: String, airportDestCode: String, numStops: Int) {
  def prettyPrint(): Unit = {
    println(Console.CYAN + airlineCode + " ✈️ " +
      Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " +
      Console.WHITE + numStops.toString + " stops" + Console.RESET)
  }

  def startsAt(_airportCode: String) = airportSourceCode == _airportCode
  def endsAt(_airportCode: String) = airportDestCode == _airportCode
  def isBetween(_srcAirportCode: String, _dstAirportCode: String): Boolean = {
    airportSourceCode == _srcAirportCode &&
      airportDestCode == _dstAirportCode
  }
  def isOperatedBy(_airline: String) = airlineCode == _airline
}

case class RouteKey(airportSourceCode: String, airportDestCode: String)

/**
 * Another version of Route class with geolocations
 */
case class GeoRoute(route: Route, srcLat: Float, srcLng: Float, dstLat: Float, dstLng: Float) {
  // Distance in metres between the source airport
  // and the destination airport
  def distance() = Geo.distance(srcLat, srcLng, dstLat, dstLng)
}

/**
 * OpenFlights data source handler
 */
object OpenFlights {
  import kantan.csv._
  import kantan.csv.ops.CsvInputOps

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

  // Decoders that tell kantan.csv how to construct our case classes from the raw data
  private implicit val airportDecoder: RowDecoder[Airport] = RowDecoder.decoder6(Airport.apply)(4, 1, 2, 3, 6, 7)
  private implicit val airlineDecoder: RowDecoder[Airline] = RowDecoder.decoder4(Airline.apply)(0, 4, 1, 6)
  private implicit val routeDecoder: RowDecoder[Route] = RowDecoder.decoder4(Route.apply)(0, 2, 4, 7)

  /**
   * Load data from a CSV file, converting records to a [[List]] of the
   * parameterized type.
   *
   * Note: loads the entire file into memory eagerly; careful with huge files.
   *
   * @param path The path of the CSV file, as a resource on the classpath.
   * @tparam T The class represented by records in the CSV. Requires a
   *   [[kantan.csv.RowDecoder]] instance for the type available in scope.
   */
  private def loadCSVDataFile[T: RowDecoder](path: String): List[T] = {
    val url = getClass.getResource(path)
    // We have a static, well-formed data set -- unsafe causes no errors.
    url.unsafeReadCsv[List, T](',', false) // comma separator, no header
  }

  private def loadAirlinesAsMap(path: String): Map[String, List[Airline]] = {
    val records = loadAirlines(path)
    val index = Map.empty[String, List[Airline]].withDefaultValue(List())

    records.foldLeft(index) { (index, airline) =>
      val code = airline.code
      index(code) = airline +: index(code)
      index
    }
  }

  /** Load a list of [[Airline]]s, given a path to a CSV file. */
  private def loadAirlines(path: String): List[Airline] = loadCSVDataFile[Airline](path)

  private def loadAirportsAsMap(path: String): Map[String, List[Airport]] = {
    val records = loadAirports(path)
    val index = Map.empty[String, List[Airport]].withDefaultValue(List())

    records.foldLeft(index) { (index, airport) =>
      val city = airport.city
      index(city) = airport +: index(city)
      index
    }
  }

  /** Load a list of [[Airport]]s, given a path to a CSV file. */
  private def loadAirports(path: String): List[Airport] = loadCSVDataFile[Airport](path)

  private def loadRoutesAsMap(path: String): Map[RouteKey, List[Route]] = {
    val records = loadRoutes(path)
    val index = Map.empty[RouteKey, List[Route]].withDefaultValue(List())

    records.foldLeft(index) { (index, route) =>
      // Both src and dst airport codes as a key
      var routeKey = RouteKey(route.airportSourceCode, route.airportDestCode)
      index(routeKey) = route +: index(routeKey)
      index
    }
  }

  /** Load a list of [[Routes]]s, given a path to a CSV file. */
  private def loadRoutes(path: String): List[Route] = loadCSVDataFile[Route](path)
}
