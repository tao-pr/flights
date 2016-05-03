package starcolon.flights.core

/**
 * Core module of Flights
 */
object Core extends App {
  import starcolon.flights.openflights._
  import starcolon.flights.routemap._
  import starcolon.flights.geo._
  import scala.io.StdIn.{ readLine, readInt }

  // Prompt the user for inputs
  println("Let's find best routes!")
  val citySource = readLine(Console.CYAN + "Source city: " + Console.RESET)
  val cityDest = readLine(Console.CYAN + "Destination city: " + Console.RESET)
  print(Console.CYAN + "Max connections: " + Console.RESET)
  var maxDegree = readInt()

  println(citySource + " ✈ ️ " + cityDest)

  // Find direct flights between two cities
  val routesDirect = RouteMap.findCityRoutes(citySource, cityDest)
  var routesIndirect = List[List[GeoRoute]]()

  if (maxDegree > 1) {
    // Find indirect flights between two cities
    routesIndirect = RouteMap.findCityIndirectRoutes(citySource, cityDest, maxDegree)
  }

  // Report time!
  println(Console.MAGENTA + "[Indirect flights]" + Console.RESET)
  routesIndirect foreach { routes =>
    println(Console.CYAN + "[ROUTE]" + Console.RESET)
    routes.foreach { _.prettyPrint("   ") }
  }

  println(Console.MAGENTA + "[Direct flights]" + Console.RESET)
  routesDirect foreach { _.prettyPrint() }

}
