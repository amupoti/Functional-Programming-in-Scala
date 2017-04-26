package observatory

import java.io.FileInputStream
import java.time.LocalDate

import observatory.utils.TemperatureHelper
import observatory.utils.TemperatureHelper._
import org.apache.spark.sql.{SparkSession, functions}
import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source._


/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  @transient lazy val spark = SparkSession.
    builder().
    appName("extraction").
    config("spark.master", "local").
    getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._


  def toOptionDouble(s: String): Option[Double] = if (s.isEmpty) None else Some(s.toDouble)

  def rowToStation(row: Array[String]): Option[Station] =
    (row(0), row(1), row(2), row(3)) match {
      //ignore data coming from stations that have no GPS coordinates
      case (_, _, lat, lon) if (lat.isEmpty || lon.isEmpty) => None
      case (stn, wban, lat, lon) => Some(Station(stn.concat(wban), lat.toDouble, lon.toDouble))
      case _ => None
    }

  def rowToTemperature(row: Array[String]): Temperature = {
    Temperature(row(0).concat(row(1)), row(2).toInt, row(3).toInt, row(4).toDouble)
  }


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stations = spark.read.textFile(loadFile(stationsFile)).map(_.split(",", -1)).flatMap(row => rowToStation(row))
    val temperatures = spark.read.textFile(loadFile(temperaturesFile)).map(_.split(",", -1)).map(row => rowToTemperature(row))
    println(s"Files loaded successfully. Stations: ${stations.count()}, Temperatures: ${temperatures.count()}")

    val join = stations.joinWith(temperatures, $"id" === $"stationId")
    val triplets = join.collect().map {
      case (st, temp) => (LocalDate.of(year, temp.month, temp.day), Location(st.lat, st.lon), toCelsius(temp.temperature))
    }
    triplets
  }


  def loadFile(classpathResource: String): String = {
    val path = getClass.getResource(classpathResource).toURI.getPath
    println(s"Path for $classpathResource is $path")
    path
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    import org.apache.spark.sql.expressions.scalalang.typed
    val ds = spark.sparkContext.parallelize(records.toSeq).map(t => (t._2, t._3)).toDS()
    val gbk = ds.groupByKey(t => t._1).agg(typed.avg(_._2)).collect()
    gbk.toList
  }

}
