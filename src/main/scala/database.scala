package flights

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import slick.driver.H2Driver.api._

/**
 * Airline table for Slick interface
 */
class Airlines(tag: Tag) extends Table[Airline](tag, "AIRLINES") {
  def id = column[Long]("id")
  def code = column[String]("code")
  def name = column[String]("name")
  def country = column[String]("country")

  def * = (id, code, name, country) <> (Airline.tupled, Airline.unapply)
}

/**
 * Airport table for Slick interface
 */
class Airports(tag: Tag) extends Table[Airport](tag, "AIRPORTS") {
  def code = column[String]("code")
  def name = column[String]("name")
  def city = column[String]("city")
  def country = column[String]("country")
  def lat = column[Float]("lat")
  def lng = column[Float]("lng")

  def * = (code, name, city, country, lat, lng) <> (Airport.tupled, Airport.unapply)
}

/**
 * Route table for Slick interface
 */
class Routes(tag: Tag) extends Table[Route](tag, "ROUTES") {
  def airlineCode = column[String]("airline")
  def airportSourceCode = column[String]("src")
  def airportDestCode = column[String]("dst")
  def numStops = column[Int]("numstops")

  def * = (airlineCode, airportSourceCode, airportDestCode, numStops) <> (Route.tupled, Route.unapply)
}

object OpenFlightsDB {

  /**
   * FlightDB database handler
   */
  private val db = Database.forConfig("flightDB")

  /**
   * Data table handlers
   */
  val airlines = TableQuery[Airlines]
  val airports = TableQuery[Airports]
  val routes = TableQuery[Routes]

  /**
   * Populate airline records to the underlying database
   */
  def populateAirlines(records: Seq[Airline]) = {
    println(s"Airlines: ${records.length} records")
    val actions = DBIO.seq(
      airlines.schema.create,
      // Bulk insert
      airlines ++= records
    )
    db.run(actions)
  }

  /**
   * Poluate airport records to the underlying database
   */
  def populateAirports(records: Seq[Airport]) = {
    println(s"Airports: ${records.length} records")
    val actions = DBIO.seq(
      airports.schema.create,
      // Bulk insert
      airports ++= records
    )
    db.run(actions)
  }

  /**
   * Populate route records to the underlying database
   */
  def populateRoutes(records: Seq[Route]) = {
    println(s"Routes: ${records.length} records")
    val actions = DBIO.seq(
      routes.schema.create,
      // Bulk insert
      routes ++= records
    )
    db.run(actions)
  }

  /**
   * A summary of record counts in the database.
   */
  def recordSummary: Future[String] = {
    for {
      numAirports <- db.run(airports.size.result)
      numAirlines <- db.run(airlines.size.result)
      numRoutes <- db.run(routes.size.result)
    } yield s"${numAirports} airports, ${numAirlines} airlines, ${numRoutes} routes"
  }

  def findAirports(city: String): Future[Seq[Airport]] = {
    val query = airports
      .filter(a => a.code.length > 0 && a.city === city)
      .result

    // Compile and run the query
    return db.run(query)
  }

  def findAirportByCode(airportCode: String): Future[Seq[Airport]] = {
    val query = airports
      .filter(a => a.code === airportCode)
      .result

    // Compile and run the query
    return db.run(query)
  }

  def findAirportRoutes(srcAirport: String, dstAirport: String): Future[Seq[Route]] = {
    val query = routes
      .filter(a =>
        a.airportSourceCode === srcAirport &&
          a.airportDestCode === dstAirport)
      .result

    // Compile and run the query
    return db.run(query)
  }

  /**
   * Find all routes which depart from the specified airport
   */
  def findDepartureRoutes(srcAirport: String): Future[Seq[Route]] = {
    val query = routes
      .filter(a => a.airportSourceCode === srcAirport)
      .result

    // Compile and run the query
    return db.run(query)
  }

  /**
   * Find cities at both ends of the route
   */
  def findCitiesConnectedByRoute(route: Route): Future[(String, String)] = {
    val querySrc = airports
      .filter(a => a.code === route.airportSourceCode)
      .result

    val queryDst = airports
      .filter(a => a.code === route.airportDestCode)
      .result

    for {
      src <- db.run(querySrc)
      dst <- db.run(queryDst)
    } yield (src.head.city, dst.head.city)
  }

}

