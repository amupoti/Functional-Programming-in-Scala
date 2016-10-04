package reductions

import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(moneyLoop: Int, coinsLoop: List[Int], count: Int): Int = {
      if (moneyLoop == 0) 1
      else if (moneyLoop < 0) 0
      else if (coinsLoop.isEmpty) 0
      else loop(moneyLoop - coinsLoop.head, coinsLoop, count) + loop(moneyLoop, coinsLoop.tail, count)
    }
    loop(money, coins, 0)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {

    def loop(moneyLoop: Int, coinsLoop: List[Int], count: Int, threshold: Threshold): Int = {
      if (moneyLoop == 0) 1
      else if (moneyLoop < 0) 0
      else if (coinsLoop.isEmpty) 0
      else if (threshold(moneyLoop, coinsLoop)) countChange(moneyLoop,coinsLoop)
      else {
        val (r, l) = common.parallel(loop(moneyLoop - coinsLoop.head, coinsLoop, count, threshold), loop(moneyLoop, coinsLoop.tail, count, threshold))
        r + l
      }
    }
    loop(money, coins, 0, threshold)
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, list: List[Int]) => money <= (startingMoney * 2) / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (money: Int, list: List[Int]) => list.size <= (totalCoins * 2) / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (money: Int, list: List[Int]) => list.size * money <= (allCoins.size * startingMoney) / 2
  }
}
