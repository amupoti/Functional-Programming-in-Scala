package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("dfSchema returns a StructType with one string and the rest with doubles"){
    val struct = TimeUsage.dfSchema(List("name", "1", "2"))
    assert(struct.size === 3)
    assert(struct.head.dataType === DataTypes.StringType)
    assert(struct.tail.forall(_.dataType.equals(DataTypes.DoubleType)) === true)
    assert(struct.forall(_.nullable == false) === true)

  }

  test("row must return a spark row from the given strings in the line") {
    val row = TimeUsage.row(List("name", "1", "2"))
    assert(row.size===3)
  }

  test("classified columns must return columnNames in 3 categories") {
    val columnList = TimeUsage.classifiedColumns(List("t01", "t03", "t11", "t1801", "t1803", "t05", "t1805", "t02",
      "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16", "t18"))
    assert(columnList._1.size === 5)
    assert(columnList._2.size === 2)
    assert(columnList._3.size === 13)
  }
}
