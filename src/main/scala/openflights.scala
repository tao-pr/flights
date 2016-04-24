package starcolon.flights.openflights

import scala.collection.immutable.List

class Airline (_id: Long, _code: String, _name: String, _country: String){
	var id: Long = _id
	var code: String = _code
	var name: String = _name
	var country: String = _country

	def prettyPrint(){
		println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
	}
}

class Airport (_code: String, _name: String, _city: String, _country: String, _lat: Float, _lng: Float){
	var code: String = _code
	var name: String = _name
	var city: String = _city
	var country: String = _country
	var lat: Float = _lat
	var lng: Float = _lng

	def prettyPrint(){
		println("🏠 " + Console.CYAN + name + " (" + code + ") " + 
			Console.WHITE + city + "/" + country + Console.RESET)
	}
}

class Route (_airlineCode: String, _srcCode: String, _destCode: String, _stops: Int){
	var airlineCode: String = _airlineCode
	var airportSourceCode: String = _srcCode
	var airportDestCode: String = _destCode
	var numStops: Int = _stops

	def prettyPrint(){
		println(Console.CYAN + airlineCode + " ✈️ " + 
			Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " + 
			Console.WHITE + numStops.toString + " stops" + Console.RESET)
	}
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
			list ++ List(new Airline(n(0).toLong,n(4),n(1),n(6)))
		}
	}

	def loadAirports(path: String): List[Airport] = {
		val records = loadCSVDataFile(path)
		records.foldLeft(List[Airport]()) { (list, n) =>
			if (n.length>12)
				// In case the city has a comma,
				// it will be unintentionally splitted so we have
				// one extra element in the splitted array.
				list ++ List(new Airport(n(4),n(1),n(2)+", "+n(3),n(4),n(7).toFloat,n(8).toFloat))
			else
				list ++ List(new Airport(n(4),n(1),n(2),n(3),n(6).toFloat,n(7).toFloat))
		}
	}

	def loadRoutes(path: String): List[Route] = {
		val records = loadCSVDataFile(path)
		records.foldLeft(List[Route]()) { (list, n) => 
			list ++ List(new Route(n(0),n(2),n(4),n(7).toInt))
		}
	}

	val airlines = loadAirlines("./data/openflights/airlines.dat")
	val airports = loadAirports("./data/openflights/airports.dat")
	var routes   = loadRoutes("./data/openflights/routes.dat")
}
