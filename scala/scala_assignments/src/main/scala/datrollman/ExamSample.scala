package datrollman

import scala.annotation.tailrec

object ExamSample {

  def fun3(x: Int, y: Int, z: Int): Int = {
    if (z<=1) x-y
    else {
      if (x==y) fun3(x, y-z, z)
      else 5 * fun3(x-2, y-2, 4)
    }
  }

  def f(x: Int): Int = {
    if (x <= 100) 1 else f(x + f(x-10))
  }

  def main(args: Array[String]): Unit = {
    println(fun3(3,4,5))
  }
}
