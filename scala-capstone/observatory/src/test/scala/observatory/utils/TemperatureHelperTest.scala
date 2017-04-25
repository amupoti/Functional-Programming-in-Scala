package observatory.utils

import org.scalatest.FunSuite

class TemperatureHelperTest extends FunSuite {

  test("32 F should return 0 C") {
    assert(0 == TemperatureHelper.toCelsius(32))
  }

  test("70 F should return 21.11 C") {
    assert(21 == TemperatureHelper.toCelsius(70).toInt)

  }

}
