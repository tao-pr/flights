package flights.core

/**
 * Core module of Flights
 */
object Core extends App {
  import flights.routemap._
  import flights.geo._
  import flights.rawdata._
  import flights.database._
  import scala.io.StdIn.{ readLine, readInt }
  import scala.concurrent.ExecutionContext.Implicits.global

  // Import airports, airlines and routes
  // from .dat data files to H2 database
  println(Console.CYAN + "Preparing database..." + Console.RESET)
  OpenFlightsDB.populateAirlines(RawDataset.airlines)
  OpenFlightsDB.populateAirports(RawDataset.airports)
  OpenFlightsDB.populateRoutes(RawDataset.routes)

  // Show the database summary
  println(Console.CYAN + "========================" + Console.RESET)
  println(Console.CYAN + "Database ready" + Console.RESET)
  OpenFlightsDB.summariseRecords()
  println(Console.CYAN + "========================" + Console.RESET)

  // Prompt the user for inputs
  println("Let's find best routes!")
  val citySource = readLine(Console.CYAN + "Source city: " + Console.RESET)
  val cityDest = readLine(Console.CYAN + "Destination city: " + Console.RESET)
  print(Console.CYAN + "Max connections: " + Console.RESET)
  var maxDegree = readInt()

  println(citySource + " ✈ ️ " + cityDest)

  // Find direct flights between two cities
  val routesDirect = RouteMap.findCityRoutes(citySource, cityDest)
  for { routes <- routesDirect }
    routes foreach { _.prettyPrint() }

  // var routesIndirect = List[List[GeoRoute]]()
  // println(Console.MAGENTA + "[Direct flights]" + Console.RESET)
  // routesDirect foreach { _.prettyPrint() }

  /*
  // Report time!
  println(Console.MAGENTA + "[Indirect flights]" + Console.RESET)
  routesIndirect foreach { routes =>
    println(Console.CYAN + "[ROUTE]" + Console.RESET)
    routes.foreach { _.prettyPrint("   ") }
  }

  
  */
}
