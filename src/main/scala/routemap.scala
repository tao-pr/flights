package starcolon.flights.routemap

import starcolon.flights.openflights._

/**
 * Route mapper/finder utility
 */
object RouteMap{
	/**
	 * Find all airports located in a particular city
	 */
	def findAirports(city: String): List[Airport] = {
		OpenFlights.airports.filter(_.isIn(city))
	}

	/**
	 * Find all cities available to fly in a country
	 */
	def findCities(country: String): List[String] = {
		OpenFlights.airports
			.filter(_.isInCountry(country))
			.map(_.city)
			.toSet
			.toList
	}

	/**
	 * Find all routes between two cities
	 */
	def findCityRoutes(citySrc: String, cityDest: String): List[Route] = {
		val srcAirports = findAirports(citySrc)
		val dstAirports = findAirports(cityDest)
		for {src <- srcAirports}
			for {dst <- dstAirports}
				for {r <- findAirportRoutes(src.code,dst.code)}
					yield r
	}

	/**
	 * Find all routes between two airports
	 */
	def findAirportRoutes(airportSrc: String, airportDest: String): List[Route] = {
		OpenFlights.routes.filter(_.isBetween(airportSrc,airportDest))
	}

	/**
	 * Find all routes operated by an airway
	 */
	def findAirwayRoutes(airway: String): List[Route] = {
		OpenFlights.routes.filter(_.isOperatedBy(airway))
	}
}