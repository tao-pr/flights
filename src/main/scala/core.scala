package flights

/**
 * Core module of Flights
 */
object Core extends App {
  import scala.io.StdIn.{ readLine, readInt }
  import scala.concurrent.{ Future, Await }
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  // Import airports, airlines and routes
  // from .dat data files to H2 database
  println(Console.CYAN + "Preparing database..." + Console.RESET)
  val importJobs = for {
    job1 <- OpenFlightsDB.populateAirlines(RawDataset.airlines)
    job2 <- OpenFlightsDB.populateAirports(RawDataset.airports)
    job3 <- OpenFlightsDB.populateRoutes(RawDataset.routes)
  } yield (job1, job2, job3)

  // Make sure all data has been imported
  Await.result(importJobs, 100.seconds)

  // Show the database summary
  println(Console.CYAN + "========================" + Console.RESET)
  println(Console.CYAN + "Database ready" + Console.RESET)
  OpenFlightsDB.summariseRecords()
  println(Console.CYAN + "========================" + Console.RESET)

  // Prompt the user for inputs
  println("Let's find best routes!")

  while (true) {
    println()
    val citySource = readLine(Console.CYAN + "Source city: " + Console.RESET)
    val cityDest = readLine(Console.CYAN + "Destination city: " + Console.RESET)

    println(Console.CYAN + "=========================")
    println(Console.CYAN + "Max number of connections")
    print(Console.CYAN + "(1 for direct flights only): " + Console.RESET)
    var maxDegree = readInt()

    println(citySource + " ✈ ️ " + cityDest)

    // Find direct flights between two cities
    // val routesDirect = Await.result(
    //   RouteMap.findCityRoutes(citySource, cityDest),
    //   100.seconds
    // )
    // routesDirect foreach { _.prettyPrint() }

    // Find indirect flights between two cities
    if (maxDegree > 0) {
      val routesIndirect = RouteMap.findCityIndirectRoutes(citySource, cityDest, maxDegree)

      routesIndirect foreach { _.prettyPrint() }
    }

    println(Console.GREEN + "Try again, or Ctrl + C to quit" + Console.RESET)
  }
}
