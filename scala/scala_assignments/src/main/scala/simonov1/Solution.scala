package simonov1

import java.util.NoSuchElementException

object Solution {

  //sealed abstract class List[A]
  //case class Nill[A]() extends List[A]
  //case class Cons[A](head: A, tail: List[A]) extends List[A]

  /**
   * There are several ways to create a List:
   */
  val as = List(1,2,3)
  val bs = 1 :: 2 :: 3 :: Nil
  /**      ^1   ^^^^^^^^^^^^^2
   * This second line uses the :: function to prepend its left argument to its right argument.
   * (1) is the element to be prepended,
   * (2) is an instance of a List
   *
   * Nil is the constructor for an empty list.
   *
   * Therefore we could also do the following:
   */
  val cs = 1 :: List(2,3,4)

  /**
   * Watch out, the following two expressions do not create one list,
   * but rather a list containing integers and lists!
   */
  val ds = List(1, 2) :: List(3, 4) // = List(List(1,2),3,4)
  val es = 1 :: List(2, 3) :: 4 :: List(5, 6) :: List(7) // = List(1,List(2,3),4,List(5,6),7)

  /**
   * If you wish to concat two (or more) lists, use the ::: operator
   */
  val fs = List(1, 2) ::: List(3, 4) // = List(1, 2, 3, 4)

  /**
   * To pattern match on a list, we can use the :: function as illustrated above.
   * Note that any part of the pattern match that we do not use can be ignored with an underscore '_'
   * This way it is not required to give each part of a match a name.
   *
   * Note: The [E] in the function signature introduces a type variable for the generic type of List.
   * This means the head function works for any list, not just List[Int] or List[String].
   */
  def head[E](xs: List[E]): E = xs match {
    case Nil       => throw new NoSuchElementException
    case head :: _ => head
  }

  /**
   * It's also possible to pattern match on concrete values.
   * This function matches any list that starts with 1, 2 and 3 as it's first elements:
   */
  def headIs123(xs: List[Int]): Boolean = xs match {
    case 1 :: 2 :: 3 :: _ => true
    case _                => false
  }

  /**
   * EXERCISE:
   * Define a function sum(xs).
   * It should sum every second and third element in a list, ignoring all other elements.
   * Only add to the sum when both the second and third element are present (see: second example).
   * Use pattern matching and recursion!
   *
   * So:
   *   sum(List(1,2,3,4,5,6)) == 2+3 + 5+6
   *   sum(List(1,2,3,4,5)) == 2+3
   *   sum(List(1,2)) == 0
   */
  def sum(xs: List[Int]): Int = xs match {
    case a1 :: a2 :: a3 :: a4 =>
      a2 + a3 + sum(a4)
    case _ => 0
  }
}