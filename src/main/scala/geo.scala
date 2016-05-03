package starcolon.flights.geo

object Geo {
  /**
   * Calculates a distance between two locations
   * returns distance in metres
   */
  def distance(lat1: Float, lng1: Float, lat2: Float, lng2: Float): Float = {
    val dlat = 111540 * (lat1 - lat2)
    val dlng = 111320 * Math.cos(lat1) * (lng2 - lng1)

    val flat = dlat.toFloat
    val flng = dlng.toFloat

    Math.sqrt(flat * flat + flng * flng).toFloat
  }

  /**
   * Convert a distance in metre from a location to delta lattitude
   */
  def metreToDeltaLat(lat0: Float, lng0: Float, dist: Float): Float = {
    (0.707 * dist / 111540).toFloat
  }

  /**
   * Convert a distance in metre from a location to delta longitude
   */
  def metreToDeltaLng(lat0: Float, lng0: Float, dist: Float): Float = {
    (0.707 * dist / 111320).toFloat
  }
}