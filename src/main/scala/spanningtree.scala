package flights

import slick.driver.H2Driver.api._
import slick.lifted.Query
import scala.Either
import scala.collection.immutable.{ Iterable }
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Map
import scala.language.postfixOps
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.Ordered.orderingToOrdered

/**
 * A spanning tree consists of nodes and edges which:
 *   [Nodes] = list of cities
 *   [Edges] = list of routes
 * However, for simplicity, the list of `Airport`s
 * is employed as nodes but will be referenced as cities.
 * This means one city node may be represented by one or more Airports.
 */
case class SpanningTree(cities: Set[String], airports: Seq[Airport], links: List[Route]) {

  /**
   * Check whether the tree has a loop.
   */
  def isLooped(): Boolean = {

    // TAOTODO:

    false
  }

  /**
   * List all cities in the tree which don't have
   * any route connected.
   */
  def orphanCities(): Set[String] = {
    links.foldLeft(cities) { (orphans, lnk) =>
      var sourceCity = cityOfAirport(lnk.airportSourceCode)
      var destCity = cityOfAirport(lnk.airportDestCode)
      // Remove the two terminal cities from the list of orphans.
      // Iterate until all routes are tested.
      orphans -- Set(sourceCity, destCity)
    }
  }

  /**
   * Find which city the given airport resides in
   */
  private def cityOfAirport(airportCode: String): String = {
    airports
      .filter(_.code == airportCode)
      .head
      .city
  }

}

/**
 * Definition of links between two particular cities
 */
case class CityLink(citySrc: String, cityDest: String, airports: Map[String, Airport], routes: Future[Seq[Route]], distance: Float) extends Ordered[CityLink] {

  def compare(that: CityLink): Int = {
    distance.compareTo(that.distance)
  }

}

object FindCityLink {

  /**
   * Make a city link
   * @param {String} source city
   * @param {String} destination city
   * @return {CityLink} resultant city link
   */
  def apply(citySrc: String, cityDest: String): CityLink = {
    // Find all routes which connect both cities
    val routes = RouteMap.findCityRoutes(citySrc, cityDest)
    val airportsSrc = Await.result(OpenFlightsDB.findAirports(citySrc), 20 seconds)
    val airportsDst = Await.result(OpenFlightsDB.findAirports(cityDest), 20 seconds)

    // Combine all airports altogether as a hash map
    var airports = Map.empty[String, Airport]
    for (ap <- airportsSrc ++ airportsDst) {
      airports += ap.code -> ap
    }

    // Calculate the shortest distance among routes
    val shortestDist = shortestDistance(routes, airports)
    CityLink(citySrc, cityDest, airports, routes, shortestDist);
  }

  /**
   * Compute the shortest distance among the sequence of routes.
   * The list of associated airports are required as supplementary info.
   */
  private def shortestDistance(routes: Future[Seq[Route]], airports: Map[String, Airport]): Float = {
    val allRoutes = Await.result(routes, 10 seconds)
    val sortedRoutes = allRoutes.sortWith((left, right) => {
      routeDistance(left, airports) < routeDistance(right, airports)
    })

    routeDistance(
      sortedRoutes.head,
      airports
    )
  }

  /**
   * Compute a distance of a route.
   * The list of associated airports are required as supplementary info.
   */
  private def routeDistance(route: Route, airports: Map[String, Airport]): Float = {
    val srcAirport = airports get route.airportSourceCode
    val dstAirport = airports get route.airportDestCode

    if (srcAirport.isDefined && dstAirport.isDefined)
      srcAirport.get.distanceTo(dstAirport.get)
    else
      0
  }
}

object TreeSpanner {

  /**
   * Given a list of cities, create a minimum spanning tree
   * which is a subtree (with minimal total distance)
   * of the routes connecting those cities
   */
  def minSpanningTree(cities: List[String]): SpanningTree = {

    // Find all airports residing in the given set of cities
    val airports = cities.flatMap {
      (city) => Await.result(OpenFlightsDB.findAirports(city), 20 seconds)
    }

    // Find all possible city links which connect
    // the given set of cities altogether.
    var q = PriorityQueue.empty[CityLink](implicitly[Ordering[CityLink]])
    cities.foreach { (src) =>
      cities.filter(_ != src).foreach { (dst) =>
        // Add a city link to the priority queue
        // which sorts its elements by shortest distance.
        q.enqueue(FindCityLink(src, dst))
      }
    }

    // Perform Kruskal's algorithm
    // TAOTODO:

    SpanningTree(cities.toSet, airports, List[Route]())
  }
}

