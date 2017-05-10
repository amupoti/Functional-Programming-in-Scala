package observatory

import java.io.File
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.FileAttribute

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap
import scala.tools.nsc.classpath.FileUtils

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

  import org.scalactic.Tolerance._

  val bound = 85.0511

  test("tileLocation should provide top left location for 1 tile") {
    val actualLoc = Interaction.tileLocation(0, 0, 0)
    val expectedLoc = Location(bound, -180)
    assert(actualLoc.lon === expectedLoc.lon)
    assert(actualLoc.lat === expectedLoc.lat)
  }
  test("tileLocation should provide top bottom location for right upper tile") {
    assert(Interaction.tileLocation(1, 0, 1) === Location(0, -180.0))
  }
  test("tileLocation should provide center location for right bottom tile in a 2x2 tile map") {
    assert(Interaction.tileLocation(1, 1, 1) === Location(0, 0))
  }

  val temps: Iterable[(Location, Double)] = Seq((Location(0.0, 0.0), 10.0)
    , (Location(1.0, 1.0), 15.0), (Location(2.0, 2.0), 20.0))

//  val colors = Seq((60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)), (12.0, Color(255, 255, 0)), (0.0,
//    Color(0, 255, 255)), (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)), (-50.0, Color(33, 0, 107))
//    , (-60.0, Color(0, 0, 0)))

  val colors = Seq((32.0, Color(255, 0, 0)),(-32.0, Color(0, 255, 0)))

  test("tile returns an image of 256x256 without zoom") {
    val img = Interaction.tile(temps, colors, 0, 0, 0)
    assert(img.width === 256)
    assert(img.height === 256)
  }

  test("generateTiles should generate tiles for reduced dataset") {

    val temperatures = locateTemperatures(2021, "/reduced/stations.csv", "/reduced/2021.csv")
    val averages = locationYearlyAverageRecords(temperatures)
    Interaction.generateTiles[Iterable[(Location, Double)]](Seq((2021, averages)), (year, zoom, x, y, averages) => {
      val image = Interaction.tile(averages, colors, zoom, x, y)
      new File(s"/tmp/$year/$zoom/").mkdirs()
      image.output(new java.io.File(s"/tmp/$year/$zoom/$x-$y.png"))
    })
  }
}