package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core {
	import starcolon.flights.openflights._
	import starcolon.flights.routemap._

  def main(args: Array[String]) {
  	// Test finding route between two cities
  	println("Routes Bangkok -> Sydney")
  	RouteMap.findCityRoutes("Bangkok","Sydney") foreach (r => r.prettyPrint)
  }
}