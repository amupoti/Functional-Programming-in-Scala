package reductions

import common.parallel
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
    def loop(index:Int, open: Int): Boolean = {
      if (index==chars.length) open == 0
      else if (chars(index) == '(') loop(index +1, open + 1)
      else if (chars(index) == ')' && open > 0) loop(index +1, open - 1)
      else if (chars(index) == ')' && open == 0) false
      else loop(index +1, open)
    }
    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var (l, r) = (arg1, arg2)
      while (i < until) {
        if (chars(i) == '(') l = l + 1
        else if (chars(i) == ')' && l > 0) l = l - 1
        else if (chars(i) == ')' && l == 0) r = r + 1
        i += 1
      }
      (l, r)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold || threshold==1) {
        traverse(from, until, 0, 0)
      }
      else {
        //We go parallel by computing the chunks
        val mid = from + (until - from) / 2
        combine(parallel(reduce(from, mid), reduce(mid, until)))
      }
    }

    reduce(0, chars.length) ==(0, 0)

  }

  def combine(p:( (Int, Int),(Int, Int))): (Int, Int) = {

    val left = p._1
    val right = p._2

    var (l,r) = (0,0)
    val a = left._1-right._2
    if (a<0) r = Math.abs(a)
    else l = Math.abs(a)
    (l + right._1, r + left._2)

  }
}
