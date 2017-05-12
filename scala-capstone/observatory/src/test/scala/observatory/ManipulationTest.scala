package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("Make grid must return a function which computes temperature from location") {
    val temps = Seq((Location(0.0, 0.0), 10.0), (Location(2.0, 2.0), 15.0))
    val func = Manipulation.makeGrid(temps)
    assert(func(0, 0) === 10)
  }

}