package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val latLong = (for (lat <- -89 to 90; long <- -180 to 179) yield {
      (lat, long)
    })

    val tMap = latLong.par.map { case (lat, long) => ((lat, long) -> Visualization.predictTemperature(temperatures,
      Location(lat, long)))
    }.toMap

    (a, b) => {
      tMap((a, b))
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {

    ???
//    val grids = temperaturess.map(makeGrid(_))
//    val values = ((a: Int, b: Int) => grids.map(v => v(a, b)).map)
//    //    values.sum / values.le
//    values
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    ???
  }


}

