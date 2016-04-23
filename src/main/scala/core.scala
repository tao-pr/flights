package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core {
	import starcolon.flights.openflights._
  def main(args: Array[String]) {
  	val airlines = OpenFlights.airlines

		airlines foreach { a => a.prettyPrint() } 
  }
}