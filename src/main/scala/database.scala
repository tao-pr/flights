package starcolon.flights.database

import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import starcolon.flights.rawdata._

class Airlines(tag: Tag) extends Table[AirlineTuple](tag, "AIRLINES") {
  def id = column[Long]("id")
  def code = column[String]("code")
  def name = column[String]("name")
  def country = column[String]("country")

  def * = (id, code, name, country)

  def prettyPrint(): Unit = {
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
}

class Airports(tag: Tag) extends Table[AirportTuple](tag, "AIRPORTS") {
  def code = column[String]("code")
  def name = column[String]("name")
  def city = column[String]("city")
  def country = column[String]("country")
  def lat = column[Float]("lat")
  def lng = column[Float]("lng")

  def * = (code, name, city, country, lat, lng)

  def prettyPrint(): Unit = {
    println("🏠 " + Console.CYAN + name + " (" + code + ") " +
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport() = code.length > 0
  def isIn(_city: String) = city === _city
  def isInCountry(_cn: String) = country === _cn
}

class Routes(tag: Tag) extends Table[RouteTuple](tag, "ROUTES") {
  def airlineCode = column[String]("airline")
  def airportSourceCode = column[String]("src")
  def airportDestCode = column[String]("dst")
  def numStops = column[Int]("numstops")

  def * = (airlineCode, airportSourceCode, airportDestCode, numStops)

  def prettyPrint(): Unit = {
    println(Console.CYAN + airlineCode + " ✈️ " +
      Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " +
      Console.WHITE + numStops.toString + " stops" + Console.RESET)
  }

  def startsAt(_airportCode: String) = airportSourceCode === _airportCode
  def endsAt(_airportCode: String) = airportDestCode === _airportCode
  def isOperatedBy(_airline: String) = airlineCode === _airline
}

/**
 * Another version of Route class with geolocations
 */
case class GeoRoute(route: Routes, srcLat: Float, srcLng: Float, dstLat: Float, dstLng: Float) {
  // Distance in metres between the source airport
  // and the destination airport
  def distance() = Geo.distance(srcLat, srcLng, dstLat, dstLng)

  def prettyPrint(prefix: String = "") {
    println(prefix + Console.CYAN + route.airlineCode + " ✈️ " +
      Console.GREEN + route.airportSourceCode + " ➡️ " + route.airportDestCode + " " +
      Console.WHITE + route.numStops.toString + " stops " +
      Console.YELLOW + distance() / 1000 + " km" +
      Console.RESET)
  }
}


object OpenFlightsDatabase {

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
  def populateAirlines(records: Seq[AirlineCsvRow]) {
    val actions = DBIO.seq(
      airlines.schema.create,
      // Bulk insert
      airlines ++= records.map(_.toTuple)
    )
    db.run(actions)
  }

  /**
   * Poluate airport records to the underlying database
   */
  def populateAirports(records: Seq[AirportCsvRow]) {
    val actions = DBIO.seq(
      airports.schema.create,
      // Bulk insert
      airports ++= records.map(_.toTuple)
    )
    db.run(actions)
  }

  /**
   * Populate route records to the underlying database
   */
  def populateRoutes(records: Seq[RouteCsvRow]) {
    val actions = DBIO.seq(
      routes.schema.create,
      // Bulk insert
      routes ++= records.map(_.toTuple)
    )
    db.run(actions)
  }

}

