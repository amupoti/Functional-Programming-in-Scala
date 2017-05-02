package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val spark = Extraction.spark
  val earthRadius = 6371

  def computeDistance(l1: Location, l2: Location) = {
    //use great-circle distance
    val ds = acos(sin(l1.lat) * sin(l2.lat) + cos(l1.lat) * cos(l2.lat) * cos(abs(l2.lon - l1.lon)))
    ds * earthRadius
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //use Inverse distance weighting

    val tempRdd = spark.sparkContext.parallelize(temperatures.toSeq)
    val p = 2
    val numDem = tempRdd.map(u => (
      u._2 / pow(computeDistance(u._1, location), p),
      (1 / pow(computeDistance(u._1, location), 2))))
    numDem.map(_._1).sum() / numDem.map(_._2).sum()
  }

  def linearInterpolation(x0: (Double, Color), x1: (Double, Color), value: Double): Color = {

    val percentage = value - x0._1 / x1._1 - x0._1
    val diff = (x0._2.red - x1._2.red) * percentage
    //TODO: complete method

  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    // use linear interpolation
    val pointMap = points.toMap
    val valueColor = pointMap.get(value)
    val pointsSortedByTemp = points.toArray.sortBy(_._1)

    //TODO: edge cases
    val index = (1 until pointsSortedByTemp.size).
      find(i => value < pointsSortedByTemp(i + 1)._1 && value > pointsSortedByTemp(i)._1).headOption
    if (index.isEmpty) pointsSortedByTemp.last._2
    else {
      val i = index.get
      linearInterpolation(pointsSortedByTemp(i - 1), pointsSortedByTemp(i), value)
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

