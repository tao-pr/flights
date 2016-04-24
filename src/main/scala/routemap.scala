package starcolon.flights.routemap

import starcolon.flights.openflights

/**
 * Route mapper/finder utility
 */
object RouteMap{
	def findAirports(city: String): List[Airport] = {
		OpenFlights.airports.filter(_.isIn(city))
	}

	def findCities(country: String): List[String] = {
		OpenFlights.airports
			.filter(_.isInCountry(country))
			.map(_.city)
			.toSet
			.toList
	}

	def findCityRoutes(citySrc: String, cityDest: String): List[Route]{}
	def findAirportRoutes(airportSrc: String, airportDest: String): List[Route]{}
	def findAirwayRoutes(airway: String): List[Route]{}
}