package observatory


import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val colors = Seq((60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)), (12.0, Color(255, 255, 0)), (0.0,
    Color(0, 255, 255)), (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)), (-50.0, Color(33, 0, 107))
    , (-60.0, Color(0, 0, 0)))

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

  import org.scalactic.Tolerance._

  test("Predicted temperature should be between provided known values") {
    val t1 = (Location(0.0, 0.0), 10.0)
    val t2 = (Location(2.0, 2.0), 15.0)
    val predTemp = Visualization.predictTemperature(List(t1, t2), Location(1.0, 1.0))
    assert(predTemp > 10.0 && predTemp < 15.0)
  }
  test("Predicted temperature at origin with same location in list") {
    val t1 = (Location(0.0, 0.0), 10.0)
    val t2 = (Location(2.0, 2.0), 15.0)
    val predTemp = Visualization.predictTemperature(List(t1, t2), Location(0.0, 0.0))
    assert(predTemp === 10.0)
  }

  test("Predicted temperature at origin with very close location") {
    val t1 = (Location(0.00000000001, 0.00000000001), 10.0)
    val t2 = (Location(2.0, 2.0), 15.0)
    val predTemp = Visualization.predictTemperature(List(t1, t2), Location(0.0, 0.0))
    assert(predTemp === 10.0)
  }

  test("interpolateColor must provide colors for well known boundaries") {
    assert(Visualization.interpolateColor(colors, 12380.0) === Color(255, 255, 255))
    assert(Visualization.interpolateColor(colors, 60.0) === Color(255, 255, 255))
    assert(Visualization.interpolateColor(colors, 32.0) === Color(255, 0, 0))
    assert(Visualization.interpolateColor(colors, 12.0) === Color(255, 255, 0))
    assert(Visualization.interpolateColor(colors, 0.0) === Color(0, 255, 255))
    assert(Visualization.interpolateColor(colors, -15.0) === Color(0, 0, 255))
    assert(Visualization.interpolateColor(colors, -27.0) === Color(255, 0, 255))
    assert(Visualization.interpolateColor(colors, -50.0) === Color(33, 0, 107))
    assert(Visualization.interpolateColor(colors, -60.0) === Color(0, 0, 0))
    assert(Visualization.interpolateColor(colors, -2360.0) === Color(0, 0, 0))
  }
  test("interpolateColor must provide colors for liniar interpolated values") {
    val value = Visualization.interpolateColor(colors, 59.0)
    assert(value.green === 246)
  }

  test("interpolate color in mid range") {
    assert(Visualization.linearInterpolation((-2.147483648E9, Color(255, 0, 0)), (0.0, Color(0, 0, 255)), -1.073741824E9)
      === Color(128, 0, 128))
  }

  test("interpolate color in mid range for 1.0") {
    assert(Visualization.linearInterpolation((-2.147483648E9, Color(255, 0, 0)), (1.0, Color(0, 0, 255)), -1.073741824E9)
      === Color(128, 0, 128))
  }

  test("linear interpolation for an equidistant temperature should get mean value for a color") {
    val x0 = (10.0, Color(255, 255, 255))
    val x1 = (20.0, Color(255, 255, 251))
    assert(Visualization.linearInterpolation(x0, x1, 15.0) === Color(255, 255, 253))
  }

  test("linear interpolation for a color should generate a color closer to the closest temperature") {
    val x0 = (10.0, Color(255, 255, 255))
    val x1 = (20.0, Color(255, 255, 251))
    assert(Visualization.linearInterpolation(x0, x1, 18.0) === Color(255, 255, 252))
  }


  test("linear interpolation for a color should generate a color closer to the closest temperature with inverted " +
    "values") {
    val x0 = (20.0, Color(255, 255, 255))
    val x1 = (10.0, Color(255, 255, 251))
    assert(Visualization.linearInterpolation(x0, x1, 18.0) === Color(255, 255, 254))
  }


  ignore("visualize must return an image") {
    val t1 = (Location(0.0, 0.0), 10.0)
    val t2 = (Location(2.0, 2.0), 15.0)
    val temps = Seq(t1, t2)
    val image = Visualization.visualize(temps, colors)
    assert(image.width === 360)
    assert(image.height === 180)
  }

  test("predictTemperature: some point closer") {
    val location1 = Location(1, 1)
    val temp1 = 10d
    val location2 = Location(-10, -10)
    val temp2 = 50d
    val list = List((location1, temp1), (location2, temp2))
    val result = Visualization.predictTemperature(list, Location(0, 0))
    assert(temp1 - result < temp2 - result)
  }

  test("predictTemperature: some point closer with 3 values") {
    val location1 = Location(1, 1)
    val temp1 = 10d
    val location2 = Location(-10, -10)
    val temp2 = 40d
    val location3 = Location(10, 10)
    val temp3 = 40d
    val list = List((location1, temp1), (location2, temp2), (location3, temp3))
    val result = Visualization.predictTemperature(list, Location(0, 0))
    assert(temp1 - result < temp2 - result && temp1 - result < temp3 - result)
  }

  ignore("visualize by writing image with minimal data (2021)") {
    val temperatures = locateTemperatures(2021, "/reduced/stations.csv", "/reduced/2021.csv")
    val averages = locationYearlyAverageRecords(temperatures)
    val image = Visualization.visualize(averages, colors)
    image.output(new java.io.File("/tmp/2021.png"))
  }

  ignore("visualize by writing image with small dataset (1975 reduced)") {
    val temperatures = locateTemperatures(1975, "/stations.csv", "/1975_reduced.csv")
    val averages = locationYearlyAverageRecords(temperatures)
    val image = Visualization.visualize(averages, colors)
    image.output(new java.io.File("/tmp/1975_reduced.png"))
  }


  test("predictTemperature small sets") {
    assert(Visualization.predictTemperature(List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0)), Location(0.0, -45.0)) === 15.0)
    assert(Visualization.predictTemperature(List((Location(0.0, 0.0), 10.0)), Location(0.0, 0.0)) === 10.0)
    assert(Visualization.predictTemperature(List((Location(45.0, -90.0), 0.0), (Location(-45.0, 0.0), 59.028308521858634)), Location(0.0, 0.0)).round === 52)
  }

  test("interpolateColor - 2 elements") {
    assert(Visualization.interpolateColor(List((10.0, Color(0, 0, 0)), (40.0, Color(60, 60, 60))), 30) === Color(40, 40, 40))
  }

  test("interpolateColor - 2 elements inversed") {
    assert(Visualization.interpolateColor(List((40.0, Color(60, 60, 60)), (10.0, Color(0, 0, 0))), 30) === Color(40, 40, 40))
  }

  test("interpolateColor - 2 elements, from red to blue") {
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), 0.25) === Color(191, 0, 64))
  }

  test("interpolateColor - 2 elements, from red to blue, argument before first interval") {
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), -0.1) === Color(255, 0, 0))
  }

  test("interpolateColor - 2 elements, from red to blue, argument after the last interval") {
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), 1.1) === Color(0, 0, 255))
  }

  test("interpolateColor - 2 elements, middle between red and blue") {
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1445.0,Color(0,0,255))), 722.5) === Color(128,0,128))
  }

  test("interpolateColor - 2 elements, middle between red and blue, big values") {
    assert(Visualization.interpolateColor(List((-353.0,Color(255,0,0)), (163810.0, Color(0,0,255))), 81728.5) === Color(128,0,128))
  }

  test("interpolateColor - 3 elements") {
    assert(Visualization.interpolateColor(List((0.0, Color(13, 17, 19)), (10.0, Color(0, 0, 0)), (40.0, Color(60, 60, 60))), 30) === Color(40, 40, 40))
  }

  test("interpolateColor - 3 unsorted elements 2nd interval") {
    assert(Visualization.interpolateColor(List((10.0, Color(0, 0, 0)), (0.0, Color(13, 17, 19)), (40.0, Color(60, 60, 60))), 30) === Color(40, 40, 40))
  }

  test("interpolateColor - 3 unsorted elements 1st interval") {
    assert(Visualization.interpolateColor(List((10.0, Color(8, 8, 8)), (0.0, Color(0, 0, 0)), (40.0, Color(60, 60, 60))), 7.5) === Color(6, 6, 6))
  }

}