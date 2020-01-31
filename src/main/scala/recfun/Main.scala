package recfun
import java.util

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 3) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x)) I told him (that it’s not yet) done). (But he wasn’t listening)".toList))

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = c match {
      case 0 => 1
      case c if c >=r => 1
      case _ => pascal(c-1,r-1)+pascal(c,r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], left: Int): Boolean ={
      if(chars.isEmpty) left == 0
      else
      if (chars.head == ')') {left > 0 && balance(chars.tail, left - 1)}
      else if (chars.head == '(') {balance(chars.tail, left + 1)}
      else {balance(chars.tail, left)}
    }
    balance(chars, 0)
  }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    money match {
      case 0  => 1
      case x if x < 0 => 0
      case x if x>=1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)

    }

  }


}
