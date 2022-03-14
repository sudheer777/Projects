
/*
Notes

1. We are representing the puzzle as an array of 3 stacks of integers:

val numTowers = 3
val numRings = 3
val towers: Array[mutable.Stack[Int]] = Array.ofDim[mutable.Stack[Int]](numTowers)
// initialize towers
for(t <- 0 until numTowers) towers(t) = mutable.Stack[Int]()
// push ..., 3, 2, 1 onto tower 0
for(r <- numRings to 1 by -1) towers(0).push(r) // towers = [Stack(1, 2, 3) Stack() Stack()]

2. Stacks are one of many mutable collections in:

import scala.collection.mutable

3. Stacks have the usual push, pop, and top operations. A stack is empty if:

stack.length == 0

4. All of this and more can be found in Scala Docs.

5. Be careful, though. Throw an exception if an attempt is made to push n on top of m when n > m. In other words, don't allow a big ring to get pushed on top of a small one.
*/
import scala.collection.mutable

class Hanoi {
  val numRings = 3
  val numTowers = 3
  val towers: Array[mutable.Stack[Int]] = Array.ofDim[mutable.Stack[Int]](numTowers)
  // initialize towers
  for(t <- 0 until numTowers) towers(t) = mutable.Stack[Int]()
  // push ..., 3, 2, 1 onto tower 0
  for(r <- numRings to 1 by -1) towers(0).push(r) // towers = [Stack(1, 2, 3) Stack() Stack()]

  override def toString = {
    var result = "["
    for(t <- 0 until numTowers) {
      result += towers(t).toString + " "
    }
    result + "]"
  }

  // legally move num rings from fromTower to toTower
  def move(num: Int, fromTower: Int, toTower: Int): Unit = {
    if (num > 0) {
      val mtower = 3 - fromTower - toTower
      move(num-1, fromTower, mtower)
      val a = towers(fromTower).pop()
      towers(toTower).push(a)
      println(toString)
      move(num-1, mtower, toTower)
    }
  }

}

object Tower extends App {
  val game = new Hanoi()
  println(game.toString)
  // move 3 rings from tower 0 to tower 1
  game.move(3, 0, 2)
}

/*
Output:
[Stack(1, 2, 3) Stack() Stack() ]
[Stack(2, 3) Stack() Stack(1) ]
[Stack(3) Stack(2) Stack(1) ]
[Stack(3) Stack(1, 2) Stack() ]
[Stack() Stack(1, 2) Stack(3) ]
[Stack(1) Stack(2) Stack(3) ]
[Stack(1) Stack() Stack(2, 3) ]
[Stack() Stack() Stack(1, 2, 3) ]
*/