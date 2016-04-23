package starcolon.flights.openflights

import scala.collection.immutable.List

class Airline (_id: Long, _name: String, _country: String){
	var id: Long = _id
	var name: String = _name
	var country: String = _country
}

class Airport (_id: Long, _name: String, _city: String, _country: String, _lat: Float, _lng: Float){
	var id: Long = _id
	var name: String = _name
	var city: String = _city
	var country: String = _country
	var lat: Float = _lat
	var lng: Float = _lng
}
/**
 * OpenFlights data source handler
 */
object OpenFlights{
	def loadCSVDataFile(path: String): List[Array[String]] = {
		val lines   = io.Source.fromFile(path).getLines
		val records = lines.map { line => line.split(",").map(_.trim) }
		return records.toList
	}

	def loadAirlines(path: String): List[Airline] = {
		val records = loadCSVDataFile(path)
		records.foldLeft(List[Airline]()) { (list, n) =>
			list ++ List(new Airline(n(0).toLong,n(1),n(6)))
		}
	}

	def loadAirports(): List[Airport] = {
		// TAOTODO: Load airports data from file
		List[Airport]()
	}

	val airlines = loadAirlines("../../../data/openflights/airlines.dat")
	val airports = loadAirports()

}
