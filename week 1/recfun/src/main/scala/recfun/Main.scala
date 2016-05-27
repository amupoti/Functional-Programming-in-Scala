package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {

    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(charList: List[Char], open: Int): Boolean = {
      if (charList.isEmpty) open == 0
      else if (charList.head == '(') loop(charList.tail, open + 1)
      else if (charList.head == ')' && open > 0) loop(charList.tail, open - 1)
      else if (charList.head == ')' && open == 0) false
      else loop(charList.tail, open)

    }

    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(moneyLoop: Int, coinsLoop: List[Int], count: Int): Int = {

      if (moneyLoop == 0) 1
      else if (moneyLoop < 0) 0
      else if (coinsLoop.isEmpty) 0
      else loop(moneyLoop - coinsLoop.head, coinsLoop, count) + loop(moneyLoop,
        coinsLoop.tail, count)
    }

    loop(money, coins, 0)
  }
}
