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
  lazy val airlines: Map[String, List[Airline]] =
    asMultiMap(loadCSV[Airline]("/data/openflights/airlines.dat"), airline => airline.code)

  /**
   * A mapping of city names to the [[Airport]]s located in that city.
   */
  lazy val airports: Map[String, List[Airport]] =
    asMultiMap(loadCSV[Airport]("/data/openflights/airports.dat"), airport => airport.city)

  /**
   * A mapping of [[Route]]s keyed by their (source, destination) pairs.
   */
  lazy val routes: Map[RouteKey, List[Route]] = {
    val records = loadCSV[Route]("/data/openflights/routes.dat")
    asMultiMap(records, route => RouteKey(route.airportSourceCode, route.airportDestCode))
  }

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
  private def loadCSV[T: RowDecoder](path: String): List[T] = {
    val url = getClass.getResource(path)
    // We have a static, well-formed data set -- unsafe causes no errors.
    url.unsafeReadCsv[List, T](',', false) // comma separator, no header
  }

  /**
   * Creates a multi-Map from a list, using the given keyfunc to derive a map
   * key for each list value. The Map is defined to return an empty List for
   * keys with missing values.
   */
  private def asMultiMap[K, V](records: List[V], keyfunc: V => K): Map[K, List[V]] = {
    val index = Map.empty[K, List[V]].withDefaultValue(List())

    records.foldLeft(index) { (index, record) =>
      val key = keyfunc(record)
      index(key) = record +: index(key)
      index
    }
  }
}
