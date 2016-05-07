package starcolon.flights.data

import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global

class Airlines(tag: Tag) extends Table[(Long, String, String, String)](tag, "AIRLINES") {
  def id = column[Long]("id")
  def code = column[String]("code")
  def name = column[String]("name")
  def country = column[String]("country")

  def * = (id, code, name, country)
}

class Airports(tag: Tag) extends Table[(String, String, String, String, Float, Float)](tag, "AIRPORTS") {
  def code = column[String]("code")
  def name = column[String]("name")
  def city = column[String]("city")
  def country = column[String]("country")
  def lat = column[Float]("lat")
  def lng = column[Float]("lng")

  def * = (code, name, city, country, lat, lng)
}

class Routes(tag: Tag) extends Table[(String, String, String, Int)](tag, "ROUTES") {
  def airlineCode = column[String]("airline")
  def airportSourceCode = column[String]("src")
  def airportDestCode = column[String]("dst")
  def numStops = column[Int]("numstops")

  def * = (airlineCode, airportSourceCode, airportDestCode, numStops)
}

object OpenFlightsData {

  /**
   * Database objects
   */
  val db = Database.forConfig("flightDB")

  val airlines = TableQuery[Airlines]
  val airports = TableQuery[Airports]
  val routes = TableQuery[Routes]

  /**
   * Populate airline records to the underlying database
   */
  def populateAirlines(records: Seq[(Long, String, String, String)]) {
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
  def populateAirports(records: Seq[(String, String, String, String, Float, Float)]) {
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
  def populateRoutes(records: Seq[(String, String, String, Int)]) {
    val actions = DBIO.seq(
      routes.schema.create,
      // Bulk insert
      routes ++= records
    )
    db.run(actions)
  }

  /**
   * Flush occupied resources
   */
  def dispose() {
    db.close
  }
}

