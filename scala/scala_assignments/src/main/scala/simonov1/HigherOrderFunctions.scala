package simonov1

import java.util.NoSuchElementException

object HigherOrderFunctions {
  /**
   * EXERCISE:
   * Define your own version of map! Loops are prohibited, so use recursion.
   */
  def myMap[A,B](xs: List[A], f: (A => B)): List[B] = xs match {
    case Nil          => Nil
    case head :: tail => f(head) :: myMap(tail, f)
  }


  /**
   * EXERCISE:
   * Take a look at the takeWhile function:
   * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#takeWhile(p:A=%3EBoolean):List[A]
   *
   * Define the function takeWhileSmallerThanFive, it should take a list and return the first n
   * elements, until the next element (n+1) is greater or equal to 5.
   *
   * Use the takeWhile function!
   */
  def takeWhileSmallerThanFive(xs: List[Int]): List[Int] = {
   xs.takeWhile(_ < 5)
  }

  /**
   * EXERCISE:
   * Define the function dropWhileSmallerThanFive, it should take a list and discard the first n
   * elements, until the next element (n+1) is greater or equal to 5.
   *
   * Again, use one of Scala's built-in list functions.
   */
  def dropWhileSmallerThanFive(xs: List[Int]): List[Int] = {
    xs.dropWhile(_ < 5)
  }

  /**
   * Take a look at the zip function:
   * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#zip[B](that:scala.collection.GenIterable[B]):List[(A,B)]
   *
   * This function connects two lists by 'zipping' elements into tuples:
   */
  List(1,2,3,4).zip(List(2,4,6,8)) // = List( (1,2), (2,4), (3,6), (4,8) )

  /**
   * EXERCISE:
   * Define zipWith. It should zip two lists, but instead of zipping elements into a tuple,
   * it should use a function to combine two elements.
   *
   * Example: zipWith(List(1, 2, 3),
   *                  List(10, 11, 12),
   *                  (x: Int, y: Int) => x+y)
   * Should return: List(11,13,15)
   *
   * Hint: use map and zip.
   */
  def zipWith[A,B,C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] = {
    xs.zip(ys).map(x => f(x._1, x._2))
  }

  def main(args: Array[String]): Unit = {
    def a(k: Int): Int = k+1
    println(myMap(List(1,2,3,4,5,6), a))

    println(takeWhileSmallerThanFive(List(1,2,3,4,5,6,7,8)))
    println(dropWhileSmallerThanFive(List(1,2,3,4,5,6,7,8)))
    println(zipWith(List(1, 2, 3), List(10, 11, 12), (x: Int, y: Int) => x+y))
  }
}

