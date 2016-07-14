package flights

/**
 * Core module of Flights
 */
object Core extends App {
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.io.StdIn.{ readLine, readInt }

  def prepareDatabase() {
    // Import airports, airlines and routes
    // from .dat data files to H2 database
    println(Console.CYAN + "Preparing database..." + Console.RESET)
    val importJobs = for {
      job1 <- OpenFlightsDB.populateAirlines(RawDataset.airlines)
      job2 <- OpenFlightsDB.populateAirports(RawDataset.airports)
      job3 <- OpenFlightsDB.populateRoutes(RawDataset.routes)
    } yield (job1, job2, job3)
    // TAOTODO: Above, better to use Future.sequence ?

    // Make sure all data has been imported
    Await.result(importJobs, 100.seconds)
    val summary = Await.result(OpenFlightsDB.recordSummary, 10.seconds)

    // Show the database summary
    println(Console.CYAN + "========================" + Console.RESET)
    println(Console.CYAN + "Database ready" + Console.RESET)
    println(summary)
    println(Console.CYAN + "========================" + Console.RESET)
  }

  def findBestRoutes() {
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
        val results = Await.result(routesIndirect, 30.seconds)

        results foreach { _.prettyPrint() }
      }

      println(Console.GREEN + "Try again, or Ctrl + C to quit" + Console.RESET)
    }
  }

  def findSpanningRoutes() {
    println("Let's find spanning routes")
    while (true) {
      // Prompt the user for 3~5 cities
      println(Console.CYAN + "Enter 3~5 city names, separated by commas" + Console.RESET)
      val input = readLine(Console.CYAN + ":" + Console.RESET)
      val cities = input.split(",").map(_.trim).toList

      // Generate a spanning tree
      val tree = TreeSpanner.minSpanningTree(cities)
      tree.prettyPrint()

      println(Console.GREEN + "Try again, or Ctrl + C to quit" + Console.RESET)
    }
  }

  prepareDatabase()
  println()
  println(Console.MAGENTA + " a) " + Console.RESET + "Finding best routes")
  println(Console.MAGENTA + " b) " + Console.RESET + "Finding spanning routes (tree)")
  val option = readLine("Which one to play? (a/b) :")

  option match {
    case "a" => findBestRoutes()
    case "b" => findSpanningRoutes()
    case _ => {
      println("Unknown option, go for best route finding then...")
      findBestRoutes()
    }
  }

}
