package observatory.utils

object TemperatureHelper {

  def toCelsius(temperature: Double) = (temperature - 32) * 5 / 9

}
