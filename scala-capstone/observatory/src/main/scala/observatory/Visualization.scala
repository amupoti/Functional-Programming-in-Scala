package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val spark = Extraction.spark
  val earthRadius = 6371
  val p = 6
  val delta = 0.01

  def roundAt(p: Int, n: Double): Double = {
    val s = math pow(10, p);
    (math round n * s) / s
  }

  def computeDistance(l1: Location, l2: Location) = {
    //use great-circle distance
    val ds = acos(sin(l1.lat) * sin(l2.lat) + cos(l1.lat) * cos(l2.lat) * cos(abs(l2.lon - l1.lon)))
    ds * earthRadius
  }

  def isCloseLocation(loc1: Location, loc2: Location): Boolean =
    abs(loc1.lon - loc2.lon) < delta && abs(loc1.lat - loc2.lat) < delta

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //use Inverse distance weighting
    val closeTemp = temperatures.find(loc_temp => isCloseLocation(location, loc_temp._1)).headOption

    if (closeTemp.isDefined) closeTemp.get._2
    else {

      val numDenumVals = temperatures.par.map(loc_temp => {
        val dist = 1 / pow(computeDistance(loc_temp._1, location), p)
        (loc_temp._2 * dist, dist)
      })
      val numDenum = numDenumVals.aggregate((0.0, 0.0))((acc, value) => (acc._1 + value._1, acc._2 + value._2),
        (acc, value) => (acc._1 + value._1, acc._2 + value._2))

      numDenum._1 / numDenum._2
    }

  }

  def computeTemp(x0: Int, x1: Int, p: Double) = round(x1 * p + x0 * (1 - p)).toInt

  def linearInterpolation(x0: (Double, Color), x1: (Double, Color), value: Double): Color = {
    val (r0, g0, b0) = (x0._2.red, x0._2.green, x0._2.blue)
    val (r1, g1, b1) = (x1._2.red, x1._2.green, x1._2.blue)
    val percentage = roundAt(3, (value - x0._1) / (x1._1 - x0._1))
    val tR = computeTemp(r0, r1, percentage)
    val tG = computeTemp(g0, g1, percentage)
    val tB = computeTemp(b0, b1, percentage)
    Color(tR, tG, tB)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    // use linear interpolation

    //Find if temp is in mapping to avoid interpolation
    val valueMapped = points.find(_._2 == value).headOption
    if (valueMapped.isDefined) valueMapped.get._2
    else {
      //Interpolate
      val pointsSortedByTemp = points.toArray.sortBy(_._1)

      if (value > pointsSortedByTemp.last._1) pointsSortedByTemp.last._2
      else if (value < pointsSortedByTemp.head._1) pointsSortedByTemp.head._2
      else {
        val index = (1 until pointsSortedByTemp.size).
          find(i => value <= pointsSortedByTemp(i)._1 && value >= pointsSortedByTemp(i - 1)._1).headOption
        if (index.isEmpty) pointsSortedByTemp.last._2
        else {
          val i = index.get
          linearInterpolation(pointsSortedByTemp(i - 1), pointsSortedByTemp(i), value)
        }
      }
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    val locations = for (lat <- 90 until -90 by -1; lon <- -180 until 180) yield {
      Location(lat, lon)
    }

    val pixels = locations.par.
      map(l => interpolateColor(colors, predictTemperature(temperatures, l))).
      map(c => Pixel(c.red, c.green, c.blue, 255)).toArray
    Image(360, 180, pixels)

  }
}

