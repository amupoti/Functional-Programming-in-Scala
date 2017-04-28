package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: StationId, lat: Double, lon: Double)

case class Temperature(stationId: StationId, month: Int, day: Int, temperature: Double)

case class StationId(stn:String, wban:String)