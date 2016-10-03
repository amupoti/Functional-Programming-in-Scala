package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import reductions.ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("combine should return balance for a simple open/close") {
    def check(input: (Int, Int)) =
      assert(input ==(0, 0))

    check(combine((1, 0), (0, 1)))
    check(combine((2, 0), (0, 2)))
    check(combine((1,0),combine((2, 1), (0, 2))))
    check(combine((1, 0), (0, 1)))
  }

  test("Test long simple launch of parallel balancing") {
    assert(parBalance("(((())))(((())))(((())))".toArray,8)==true)
  }

  test("Test short simple launch of parallel balancing") {
    assert(parBalance("((((()))))".toArray, 8) == true)
  }

  test("Test short complex launch of parallel balancing") {
    assert(parBalance("(()(()(())))".toArray, 8) == true)
  }

  test("Test unbalanced") {
    assert(parBalance("((((()))))(".toArray, 8) == false)
  }
  

}