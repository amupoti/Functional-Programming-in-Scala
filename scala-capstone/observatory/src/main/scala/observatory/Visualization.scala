package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val spark = Extraction.spark
  val earthRadius = 6371
  val p = 2

  def computeDistance(l1: Location, l2: Location) = {
    //use great-circle distance
    val ds = acos(sin(l1.lat) * sin(l2.lat) + cos(l1.lat) * cos(l2.lat) * cos(abs(l2.lon - l1.lon)))
    ds * earthRadius
  }


  def seqop(acc: (Location, Double), v: (Location, Double), location: Location): (Location, Double) = {
    val w = pow(1 / computeDistance(v._1, location), p)
    (acc._1, acc._2)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //use Inverse distance weighting

    val numDenumVals = temperatures.par.map(loc_temp => {
      val dist = 1 / pow(computeDistance(loc_temp._1, location), p)
      (loc_temp._2 * dist, dist)
    })
    val numDenum = numDenumVals.aggregate((0.0, 0.0))((acc, value) => (acc._1 + value._1, acc._2 + value._2),
      (acc, value) => (acc._1 + value._1, acc._2 + value._2))

    numDenum._1 / numDenum._2

  }

  def diff(x0: Int, x1: Int, percentage: Double) = {
    ((x0 - x1) * percentage)
  }


  def linearInterpolation(x0: (Double, Color), x1: (Double, Color), value: Double): Color = {
    val (r0, g0, b0) = (x0._2.red, x0._2.green, x0._2.blue)
    val (r1, g1, b1) = (x1._2.red, x1._2.green, x1._2.blue)
    val percentage = (value - x0._1) / (x1._1 - x0._1)
    val diffR = diff(r0, r1, percentage)
    val diffG = diff(g0, g1, percentage)
    val diffB = diff(b0, b1, percentage)
    Color(round(r0 - diffR).toInt, round(g0 - diffG).toInt, round(b0 - diffB).toInt)
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

      val index = (1 until pointsSortedByTemp.size).
        find(i => value <= pointsSortedByTemp(i)._1 && value >= pointsSortedByTemp(i - 1)._1).headOption
      if (index.isEmpty) pointsSortedByTemp.last._2
      else {
        val i = index.get
        linearInterpolation(pointsSortedByTemp(i - 1), pointsSortedByTemp(i), value)
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
    //    val locRdd = spark.sparkContext.parallelize(locations)
    val pixels = locations.par.
      map(l => interpolateColor(colors, predictTemperature(temperatures, l))).
      map(c => Pixel(c.red, c.green, c.blue, 255)).toArray
    val img = Image(360, 180, pixels)
    img.output(new java.io.File("/tmp/some-image.png"))
    img
  }
}

