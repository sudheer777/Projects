package eric45

import scala.util.Try

object March14 {
  // Problem 1
  def problem1[T1, T2](f: T1 => T2): (T1 => Option[T2]) = {
    def k(t: T1): Option[T2] = {
      Try(f(t)).toOption
    }
    k
  }

  // Problem 2 which computes sum of all elements in list
  def problem2(l: List[Int]): Int = {
    l match {
      case Nil => 0
      case head :: tail => head + problem2(tail)
    }
  }

  // Problem 3
  def problem3[T1, T2](l: List[T1], transformer: T1 => T2): List[T2] = {
    l match {
      case Nil => List()
      case head :: tail => transformer(head) :: problem3(tail, transformer)
    }
  }

  // Problem 4. Factorial computation
  def problem4(baseValue: Int, combiner: (Int, Int) => Int): Int => Int = {
    def res(k: Int): Int = {
      if (k == baseValue) return 1
      else {
        combiner(k, res(k-1))
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {

    // Problem 1 Testing
    def t1(a: String): Int = a.toInt
    val t2 = problem1(t1)
    println(t2("12"))
    println(t2("12a"))

    // problem2 testing
    println(problem2(List(1,2,3,4,5,6)))

    // problem3 testing
    println(problem3(List("1", "2", "0", "pl", "l", "100"), t2))


    // problem4 testing
    val f = problem4(1, _ * _)
    println(f(5))
  }
}
