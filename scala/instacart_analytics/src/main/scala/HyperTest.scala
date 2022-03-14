/*
The Rules
·       Do NOT use any functions other than the ones in this file. In other words, avoid Scala's arithmetic operators +, *, pow, etc.

·       All of the functions in Hyper should be recursive but NOT tail recursive.

·       All of the functions in TailHyper should use tail recursion.(Tail recursive helper functions are okay.)

*/

import scala.annotation.tailrec

class Base {
  def inc(x: BigInt): BigInt = x + 1
  def dec(x: BigInt): BigInt = x - 1
  def isZero(x: BigInt): Boolean = x == 0
}

class Hyper extends Base {

  def add(n: BigInt, m: BigInt): BigInt = {
    if (isZero(m)) {
      n
    } else {
      inc(add(n, dec(m)))
    }
  }

  def mul(n: BigInt, m: BigInt): BigInt = {
    if (isZero(m)) {
      0
    } else {
      add(n, mul(n, dec(m)))
    }
  }

  def exp(n: BigInt): BigInt = {
    val n1 = dec(n)
    if (isZero(n1)) {
      2
    } else {
      mul(2,  exp(dec(n)))
    }
  }

  def hyperExp(n: BigInt): BigInt = {
    if (isZero(n)) {
      2
    } else {
      exp(hyperExp(dec(n)))
    }
  }
}

class TailHyper extends Base {

  @tailrec
  final def add(n: BigInt, m: BigInt): BigInt = {
    if (isZero(m)) {
      n
    } else {
      add(inc(n), dec(m))
    }
  }

  def mul(n: BigInt, m: BigInt): BigInt = {
    @tailrec
    def mulHelper(n1: BigInt, m1: BigInt): BigInt = {
      if (isZero(m1)) {
        n1
      } else {
        mulHelper(add(n, n1), dec(m1))
      }
    }
    mulHelper(0, m)
  }

  def exp(n: BigInt): BigInt = {
    @tailrec
    def expHelper(b: BigInt, n: BigInt): BigInt = {
      if (isZero(n)) {
        b
      } else {
        expHelper(mul(2, b), dec(n))
      }
    }
    expHelper(1, n)
  }

  def hyperExp(n: BigInt): BigInt = {
    @tailrec
    def hyperExpHelper(b: BigInt, n: BigInt): BigInt = {
      if (isZero(n)) {
        b
      } else {
        hyperExpHelper(exp(b), dec(n))
      }
    }
    hyperExpHelper(2, n)
  }


}

object HyperTest extends TailHyper with App {

  println("exp(10) = " + exp(10))           // 1024
  println("hyperExp(2) = " + hyperExp(2))   // 16
  println("hyperExp(3) = " + hyperExp(3))   // 65536
  println("hyperExp(4) = " + hyperExp(4))   // still waiting

}