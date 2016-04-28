package starcolon.flights.openflights

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
			list ++ List(Airline(
				n(0).toLong,
				n(4).replace("\"",""),
				n(1).replace("\"",""),
				n(6).replace("\"","")
			))
		}
	}

	def loadAirportsAsMap(path: String): Map[String,Airport] = {
		val records = loadCSVDataFile(path)
		records.foldLeft(Map[String,Airport]()){ (map,n) =>
			val code    = n(4).replace("\"","")
			val airport = Airport(
					n(4).replace("\"",""),
					n(1).replace("\"",""),
					(n(2)+", "+n(3)).replace("\"",""),
					n(4).replace("\"",""),
					n(7).toFloat,
					n(8).toFloat
				)
			map(code) = airport
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

	val airlines = loadAirlines("./data/openflights/airlines.dat")
	val airports = loadAirports("./data/openflights/airports.dat")
	var routes   = loadRoutes("./data/openflights/routes.dat")
}
