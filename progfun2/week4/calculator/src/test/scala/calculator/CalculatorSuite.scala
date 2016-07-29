package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a changing signal") {
    val signalVar = Var(52)
    val resultChangeColor = TweetLength.colorForRemainingCharsCount(signalVar)
    assert(resultChangeColor() == "green")
    signalVar.update(10)
    assert(resultChangeColor() == "orange")
    signalVar.update(-10)
    assert(resultChangeColor() == "red")
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("find roots with 2 solutions") {
    val a: Signal[Double] = Signal(2)
    val b: Signal[Double] = Signal(12)
    val c: Signal[Double] = Signal(10)
    assert(Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))() == Set(-1.0, -5.0))
  }

  test("Simple expressions") {
    val map = Calculator.computeValues(Map("a" -> Signal(Literal(2.0)), ("b" -> Signal(Plus(Literal(3), Literal(1))))))
    map mapValues (v => v()) foreach ((p) => println("Name is " + p._1 + " and value is " + p._2))
    assert(true)
  }

}
