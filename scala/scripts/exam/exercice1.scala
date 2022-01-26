package com.gh.json

import scala.io.StdIn._

object exercice1 {
  def code(mot: String): String = {
    /*
    - YOUR CODE MUST START RIGHT AFTER THIS COMMENT -
    Do not delete or modify anything above this comment
    */
    /*
    Copy/paste just below this comment the declaration of the two arrays that was given to you in the statement
    You must use these variables to solve the exercise
    You cannot modify their declaration
    */

    /*
    The message array represents the result
    You must adapt its declaration for the exercise
    */
    var message = Array("") // //TO ADAPT
    val morse = Array(".-", "-...", "-.-.", "-..", ".","..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--",
      "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..")
    val alphabet = Array('A', 'B', 'C', 'D', 'E', 'F','G','H','I','J','K','L','M','N','O','P',
      'Q','R','S','T','U','V','W','X','Y','Z')
    val motArr = mot.toCharArray
    message = new Array[String](motArr.length)
    var i = 0
    while (i < motArr.length) {
      val ind = alphabet.indexOf(motArr(i))
      message(i) = morse(ind)
      i += 1
    }

    /*
    - YOUR CODE MUST END JUST BEFORE THIS COMMENT
    Do not delete or modify anything after this comment
     */
    return message.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    /*
    For the exam, you MUST NOT write your code here
    */
    var mot = readLine("Mot ? > ")
    print("code :" + code(mot))
  }
}
