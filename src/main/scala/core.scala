package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core {
	import starcolon.flights.openflights._
	import starcolon.flights.routemap._
	import scala.io.StdIn.{readLine,readInt}

  def main(args: Array[String]) {
  	// Run an REPL
  	repl()
  }

  def repl(){
  	// Prompt the user for inputs
  	println("Let's find best routes!")
		val citySource = readLine(Console.CYAN + "Source city: " + Console.RESET)
		val cityDest   = readLine(Console.CYAN + "Destination city: " + Console.RESET)

		println(citySource + " ✈ ️ " + cityDest)
  }
}