package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: String, lat: Double, lon: Double)

case class Temperature(stationId: String, month: Int, day: Int, temperature: Double)

