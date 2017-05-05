package observatory


import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val points = Seq((60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)), (12.0, Color(255, 255, 0)), (0.0,
    Color(0, 255, 255)), (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)), (-50.0, Color(33, 0, 107))
    , (-60.0, Color(0, 0, 0)))


  test("Predicted temperature should be between provided known values") {
    val t1 = (Location(0.0, 0.0), 10.0)
    val t2 = (Location(2.0, 2.0), 15.0)
    val predTemp = Visualization.predictTemperature(List(t1, t2), Location(1.0, 1.0))
    println(s"Predicted temperature is $predTemp")
    assert(predTemp > 10.0 && predTemp < 15.0)
  }

  test("interpolateColor must provide colors for well known boundaries") {
    assert(Visualization.interpolateColor(points, 60.0) === Color(255, 255, 255))
    assert(Visualization.interpolateColor(points, 32.0) === Color(255, 0, 0))
    assert(Visualization.interpolateColor(points, 12.0) === Color(255, 255, 0))
    assert(Visualization.interpolateColor(points, 0.0) === Color(0, 255, 255))
    assert(Visualization.interpolateColor(points, -15.0) === Color(0, 0, 255))
    assert(Visualization.interpolateColor(points, -27.0) === Color(255, 0, 255))
    assert(Visualization.interpolateColor(points, -50.0) === Color(33, 0, 107))
    assert(Visualization.interpolateColor(points, -60.0) === Color(0, 0, 0))
  }
  test("interpolateColor must provide colors for liniar interpolated values") {
    val value = Visualization.interpolateColor(points, 59.0)
    assert(value.green === 246)
  }

  test("interpolate color in mid range") {
    assert(Visualization.linearInterpolation((-2.147483648E9, Color(255, 0, 0)), (0.0, Color(0, 0, 255)), -1.073741824E9)
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
    val image = Visualization.visualize(temps, points)
    assert(image.width === 360)
    assert(image.height === 180)
  }

}