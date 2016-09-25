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
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case `r` => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balanceNr(chars, 0)

  def balanceNr(chars: List[Char], count: Int): Boolean = chars.isEmpty match {
    case true => (count == 0)
    case false => chars.head match {
      case '(' => balanceNr(chars.tail, count + 1)
      case ')' => if(count <= 0) false else balanceNr(chars.tail, count - 1)
      case _   => balanceNr(chars.tail, count)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (_, Nil) => 0
    case (v, x::xs) =>
      if(v - x >= 0)
        countChange(v - x, x::xs) + countChange(v, xs)
      else
        countChange(v, xs)
  }
}
