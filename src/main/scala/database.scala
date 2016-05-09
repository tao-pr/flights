package starcolon.flights.database

import slick.driver.H2Driver.api._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import starcolon.flights.rawdata._
import starcolon.flights.rawdata.RawDataset._

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
   * Database objects
   */
  private val db = Database.forConfig("flightDB")

  val airlines = TableQuery[Airlines]
  val airports = TableQuery[Airports]
  val routes = TableQuery[Routes]

  /**
   * Populate airline records to the underlying database
   */
  def populateAirlines(records: Seq[Airline]) {
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
  def populateAirports(records: Seq[Airport]) {
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
  def populateRoutes(records: Seq[Route]) {
    val actions = DBIO.seq(
      routes.schema.create,
      // Bulk insert
      routes ++= records
    )
    db.run(actions)
  }

  def findAirports(city: String): Future[Seq[Airport]] = {
    val query = airports
      .filter(a => a.code.length > 0 && a.city === city)
      .result

    // Compile and run the query
    return db.run(query)
  }
}

