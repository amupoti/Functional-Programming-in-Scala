package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocationD(zoom: Double, x: Double, y: Double): Location = {
    //    n = 2 ^ zoom
    //    lon_deg = xtile / n * 360.0 - 180.0
    //    lat_rad = arctan(sinh(π * (1 - 2 * ytile / n)))
    //    lat_deg = lat_rad * 180.0 / π

    val n = pow(2, zoom)
    val lonDeg = x / n * 360.0 - 180.0
    val latRad = atan(sinh(PI * (1 - 2 * y / n)))
    val latDeg = latRad * 180.0 / PI
    Location(latDeg, lonDeg)

  }

  def tileLocation(zoom: Int, x: Int, y: Int): Location = tileLocationD(zoom, x, y)

  def nextTileCoord(x: Int, y: Int) = {
    ((2 * x, 2 * y), (2 * x + 1, 2 * y), (2 * x, 2 * y + 1), (2 * x + 1, 2 * y + 1))
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    System.err.println("temp: " + temperatures.mkString(","))
    System.err.println("colors:" + colors.mkString(","))
    System.err.println("zoom:" + zoom + "x:" + x + "y:" + y)
    val levels = zoom + 8
    val size = 256

    //TODO: sort temperatures
    val locations = for (x <- 0 until size; y <- 0 until size) yield {
      (x, y)
    }
    val pixels = locations.par.
      map { case (x, y) => tileLocationD(levels, x.toFloat / size + x, y.toFloat / size + y) }.
      map(loc => interpolateColor(colors, predictTemperature(temperatures, loc))).
      map(c => Pixel(c.red, c.green, c.blue, 127)).toArray
    Image(size, size, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {

    val input = for (year <- yearlyData;
                     zoom <- 0 to 3;
                     x <- 0 to pow(2, zoom).toInt - 1;
                     y <- 0 to pow(2, zoom).toInt - 1
    ) yield {
      (year._1, zoom, x, y, year._2)
    }
    input.par.foreach(d => generateImage(d._1, d._2, d._3, d._4, d._5))
  }

}
