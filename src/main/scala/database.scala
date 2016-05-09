package starcolon.flights.database

import slick.driver.H2Driver.api._
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

  def prettyPrint(): Unit = {
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
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

  def prettyPrint(): Unit = {
    println("🏠 " + Console.CYAN + name + " (" + code + ") " +
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport() = code.length > 0
  def isIn(_city: String) = city === _city
  def isInCountry(_cn: String) = country === _cn
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

  def prettyPrint(): Unit = {
    println(Console.CYAN + airlineCode + " ✈️ " +
      Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " +
      Console.WHITE + numStops.toString + " stops" + Console.RESET)
  }

  def startsAt(_airportCode: String) = airportSourceCode === _airportCode
  def endsAt(_airportCode: String) = airportDestCode === _airportCode
  def isOperatedBy(_airline: String) = airlineCode === _airline
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

}

