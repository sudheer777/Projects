package aryanb1239

import scala.annotation.tailrec

object Q4 {
  def power(x: Int, n: Int): Int = {
    if (n == 0) {
      1
    } else {
      power(x, n-1) * x
    }
  }

  def powerIterative(x: Int, n: Int): Int = {
    var res: Int = 1
    for (i <- 0 until n) {
      res *= x
    }
    res
  }

  def main(args: Array[String]): Unit = {
    println(power(4, 4))
    println(powerIterative(4, 4))
  }
}
