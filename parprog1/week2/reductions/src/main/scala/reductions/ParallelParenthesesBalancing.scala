package reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def loop(charList: Array[Char], open: Int): Boolean = {
      if (charList.isEmpty) open == 0
      else if (charList.head == '(') loop(charList.tail, open + 1)
      else if (charList.head == ')' && open > 0) loop(charList.tail, open - 1)
      else if (charList.head == ')' && open == 0) false
      else loop(charList.tail, open)
    }
    loop(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {

      var i = idx
      while (i < until) {

        i += 1
      }
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      if (until - from < threshold) {
        traverse()
      }
      else {
        //We go parallel by computing the chunks
        val mid = from + (until - from) / 2
        reduce(from, mid) && reduce(mid, until)
      }
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
