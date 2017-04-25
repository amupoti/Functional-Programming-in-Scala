package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("Init extraction must load station and temperatures file") {
    val triplets = Extraction.locateTemperatures(1975, "/stations.csv", "/1975_reduced.csv")
    //as many elements as temperatures
    assert(triplets.size == 2716)

  }
  //Files loaded successfully. Stations: 28128, Temperatures: 2716

}