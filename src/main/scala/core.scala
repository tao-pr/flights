package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core {
	import starcolon.flights.openflights._
	import starcolon.flights.routemap._

  def main(args: Array[String]) {
  	// Test finding route between two cities
  	println("Airports in Bangkok")
  	RouteMap.findAirports("Bangkok") foreach (n => n.prettyPrint)
  }
}