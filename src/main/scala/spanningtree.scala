package flights.spanningtree

import slick.driver.H2Driver.api._
import slick.lifted.Query
import scala.Either
import scala.collection.immutable.{ Iterable }
import scala.collection.mutable.PriorityQueue
import scala.language.postfixOps
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.Ordered.orderingToOrdered
import flights.database._
import flights.geo._
import flights.rawdata.RawDataset._
import flights.routemap._


/**
 * A spanning tree consists of nodes and edges which:
 *   [Nodes] = list of cities
 *   [Edges] = list of routes
 * However, for simplicity, the list of `Airport`s 
 * is employed as nodes but will be referenced as cities.
 * This means one city node may be represented by one or more Airports.
 */
case class SpanningTree(cities: Set[String], airports: List[Airport], links: List[Route]) {

  /**
   * Check whether the tree has a loop.
   * The algorithm takes merely O(N^2).
   */
  def isLooped(): Boolean = {

    // TAOTODO:

    false
  }
}

object TreeSpanner {

  /**
   * Given a list of cities, create a minimum spanning tree
   * which is a subtree (with minimal total distance)
   * of the routes connecting those cities
   */
  def makeSpanningTree(cities: List[String]): SpanningTree = {

    var airports = cities.flatMap((c) => OpenFlightsDB.findAirports(c))

    // List all straight routes which connect all cities altogether.
    // Store them in a priority queue, order by the distance.
    // NOTE: Ordering is automatically handled by `AirportLink` class itself.
    var q = new PriorityQueue[Route]()

    cities.foreach((src) => {
      cities.filter(_ != src).foreach((dst) => {
          val routes   = RouteMap.findCityRoutes(src, dst)
          val airlines = routes.map(_.airlineCode).toSet().toList()
          val 


        }
      })

    // Kruskal's algorithm:
    //-------------------------
    // - Iterate through the queue, take the route with minimal distance.


  }
}

