package simonov1

import java.util.NoSuchElementException


object Assignment2 {

  /**
   * Define the function lengths(xs), which returns a list of the lengths of the strings in the list xs.
   * Example: if the input is List("I", "took", "a", "quick", "glimpsing", "at", "bright", "stars"),
   * the output should be List(1, 4, 1, 5, 9, 2, 6, 5).
   */
  def lengths(xs: List[String]): List[Int] = {
    xs
      .map(_.length)
  }

  /**
   * Define the function longWords(words), which returns the words that have more than 5 characters.
   */
  def longWords(words: List[String]): List[String] = {
    words
      .filter(_.length > 5)
  }

  /**
   * Define the function maximum(numbers), which uses foldLeft to return the highest number
   * in the given list of non-negative numbers, and -1 if the list is empty.
   */
  def maximum(numbers: List[Int]): Int = {
    numbers
      .foldLeft(-1)((x, y) => {
        if (x > y)
          x
        else
          y
      })
  }

  def main(args: Array[String]): Unit = {
    println(lengths(List("I", "took", "a", "quick", "glimpsing", "at", "bright", "stars")))

    println(longWords(List("I", "took", "a", "quick", "glimpsing", "at", "bright", "stars")))

    println(maximum(List(3,4,10,9,6,9)))
  }
}