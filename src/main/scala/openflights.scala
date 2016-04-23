package starcolon.flights.openflights

import scala.collection.immutable.List

class Airline (_id: Long, _name: String, _alias: String, _country: String){
	var id: Long = _id
	var name: String = _name
	var alias: String = _alias
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
	def loadAirlines(): List[Airline] = {
		// TAOTODO: Load airlines data from file
		List[Airline]()
	}

	def loadAirports(): List[Airport] = {
		// TAOTODO: Load airports data from file
		List[Airport]()
	}

	val airlines = loadAirlines()
	val airports = loadAirports()

}
