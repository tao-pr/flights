package flights

/*
 * Primary domain model types.
 */

case class Airline(id: Long, code: String, name: String, country: String) {
  def prettyPrint(): Unit = {
    println("✈️ " + Console.CYAN + name + " (" + code + ") " + Console.WHITE + country + Console.RESET)
  }
}

case class Airport(code: String, name: String, city: String, country: String, lat: Float, lng: Float) {
  def prettyPrint(): Unit = {
    println("🏠 " + Console.CYAN + name + " (" + code + ") " +
      Console.WHITE + city + "/" + country + Console.RESET)
  }

  def isValidAirport: Boolean = code.length > 0
  def isIn(city: String): Boolean = city == this.city
  def isInCountry(country: String): Boolean = country == this.country
}

case class Route(airlineCode: String, airportSourceCode: String, airportDestCode: String, numStops: Int) {
  def prettyPrint(): Unit = {
    println(Console.CYAN + airlineCode + " ✈️ " +
      Console.GREEN + airportSourceCode + " ➡️ " + airportDestCode + " " +
      Console.WHITE + numStops.toString + " stops" + Console.RESET)
  }

  def startsAt(airportCode: String): Boolean = airportCode == airportSourceCode
  def endsAt(airportCode: String): Boolean = airportCode == airportDestCode
  def isOperatedBy(airlineCode: String): Boolean = airlineCode == this.airlineCode
}
