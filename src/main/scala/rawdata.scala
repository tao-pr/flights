package starcolon.flights.rawdata

import kantan.csv._
import kantan.csv.ops.CsvInputOps

object RawDataset {

  /** 
   * Representative tuples of CSV rows
   */
  type AirlineTuple = (Long, String, String, String)
  type AirportTuple = (String, String, String, String, Float, Float)
  type RouteTuple   = (String, String, String, Int)

  /**
   * Representative classes of the CSV rows
   */
  case class AirlineCsvRow(id:Long, code:String, name:String, country:String){
    def toTuple:AirlineTuple = (id,code,name,country)
  }

  case class AirportCsvRow(code:String, name:String, city:String, country:String, lat:Float, lng:Float){
    def toTuple:AirportTuple = (code,name,city,country,lat,lng)  
  }

  case class RouteCsvRow(airlineCode: String, airportSourceCode: String, airportDestCode: String, numStops: Int){
    def toTuple:RouteTuple = (airlineCode,airportSourceCode,airportDestCode,numStops)
  }


  // Decoders that tell kantan.csv how to construct our case classes from the raw data
  private implicit val airportDecoder: RowDecoder[AirportCsvRow] = RowDecoder.decoder6(AirportCsvRow.apply)(4, 1, 2, 3, 6, 7)
  private implicit val airlineDecoder: RowDecoder[AirlineCsvRow] = RowDecoder.decoder4(AirlineCsvRow.apply)(0, 4, 1, 6)
  private implicit val routeDecoder: RowDecoder[RouteCsvRow] = RowDecoder.decoder4(RouteCsvRow.apply)(0, 2, 4, 7)


  /**
   * A plain list of airline tuple records read from the CSV dataset.
   */
  lazy val airlines: List[AirlineCsvRow] = loadCSV[AirlineCsvRow](
    "/data/openflights/airlines.dat"
  )

  /**
   * A plain of airport tuple records read from the CSV dataset
   */
  lazy val airports: List[AirportCsvRow] = loadCSV[AirportCsvRow](
    "/data/openflights/airports.dat"
  )

  /**
   * A plain of route tuple records read from the CSV dataset
   */
  lazy val routes: List[RouteCsvRow] = loadCSV[RouteCsvRow](
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
  private def asMultiMap[K, V](records: List[V], keyfunc: V => K): Map[K, List[V]] = {
    val index = Map.empty[K, List[V]].withDefaultValue(List())

    records.foldLeft(index) { (index, record) =>
      val key = keyfunc(record)
      index(key) = record +: index(key)
      index
    }
  }
}