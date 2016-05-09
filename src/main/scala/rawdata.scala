package starcolon.flights.rawdata

object RawDataset {

  import kantan.csv._
  import kantan.csv.ops.CsvInputOps

  /**
   * Representative tuples of CSV rows
   */
  type AirlineTuple = (Long, String, String, String)
  type AirportTuple = (String, String, String, String, Float, Float)
  type RouteTuple = (String, String, String, Int)

  /**
   * Representative classes of the CSV rows
   */
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
    def isOperatedBy(_airline: String) = airlineCode == _airline
  }

  // Decoders that tell kantan.csv how to construct our case classes from the raw data
  private implicit val airportDecoder: RowDecoder[Airport] = RowDecoder.decoder6(Airport.apply)(4, 1, 2, 3, 6, 7)
  private implicit val airlineDecoder: RowDecoder[Airline] = RowDecoder.decoder4(Airline.apply)(0, 4, 1, 6)
  private implicit val routeDecoder: RowDecoder[Route] = RowDecoder.decoder4(Route.apply)(0, 2, 4, 7)

  /**
   * A plain list of airline tuple records read from the CSV dataset.
   */
  lazy val airlines: List[Airline] = loadCSV[Airline](
    "/data/openflights/airlines.dat"
  )

  /**
   * A plain of airport tuple records read from the CSV dataset
   */
  lazy val airports: List[Airport] = loadCSV[Airport](
    "/data/openflights/airports.dat"
  )

  /**
   * A plain of route tuple records read from the CSV dataset
   */
  lazy val routes: List[Route] = loadCSV[Route](
    "/data/openflights/routes.dat"
  )

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
  // private def asMultiMap[K, V](records: List[V], keyfunc: V => K): Map[K, List[V]] = {
  //   val index = Map.empty[K, List[V]].withDefaultValue(List())

  //   records.foldLeft(index) { (index, record) =>
  //     val key = keyfunc(record)
  //     index(key) = record +: index(key)
  //     index
  //   }
  // }
}