package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core {
	import starcolon.flights.openflights._
	import starcolon.flights.routemap._
	import scala.io.StdIn.{readLine,readInt}

  def main(args: Array[String]) {
  	repl()
  }

  def repl(){
  	// Prompt the user for inputs
  	println("Let's find best routes!")
		val citySource = readLine(Console.CYAN + "Source city: " + Console.RESET)
		val cityDest   = readLine(Console.CYAN + "Destination city: " + Console.RESET)
		print(Console.CYAN + "Max connections: " + Console.RESET)
		var maxDegree  = readInt()

		println(citySource + " ✈ ️ " + cityDest)

		// Find routes between two cities
		val routesDirect = RouteMap.findCityRoutes(citySource,cityDest)
		val routesIndirect = RouteMap.findCityIndirectRoutes(citySource,cityDest,maxDegree)

		// Print outputs
		println(Console.MAGENTA + "[Direct flights]" + Console.RESET)
		routesDirect foreach {r => r.prettyPrint}

		println(Console.MAGENTA + "[Indirect flights]" + Console.RESET)
		routesIndirect foreach {r => r.prettyPrint}
  }	
}