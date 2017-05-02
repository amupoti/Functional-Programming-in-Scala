package observatory


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
    assert(value.green === 255)
  }

}
