package starcolon.flights.rawdata

import kantan.csv._
import kantan.csv.ops.CsvInputOps

object RawDataset {

  /**
   * Representative types of the CSV rows
   */
  type AirlineCsvRow = (Long, String, String, String)
  type AirportCsvRow = (String, String, String, String, Float, Float)
  type RouteCsvRow   = (String, String, String, Int)

  /**
   * A plain list of airline tuple records read from the CSV dataset.
   */
  lazy val airlines: List[AirlineCsvRow] = loadCSV[](
    "/data/openflights/airlines.dat"
  )

  /**
   * A plain of airport tuple records read from the CSV dataset
   */
  lazy val airports: List[AirportCsvRow] = loadCSV[](
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