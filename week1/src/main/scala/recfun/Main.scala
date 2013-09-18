package recfun
import common._

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
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(acc: Long, chars: List[Char]): Boolean = {
      if (acc < 0)
        false
      else {
        if (chars.isEmpty)
          acc == 0
        else {
          if (chars.head == '(')
            balanceIter(acc + 1, chars.tail)
          else {
            if (chars.head == ')')
              balanceIter(acc - 1, chars.tail)
            else
              balanceIter(acc, chars.tail)
          }
        }
      }
    }
    balanceIter(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) return 0
    if (money < 0) return 0
    if (money == 0) return 1
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)  
    }
  }
}
