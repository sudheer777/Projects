import scala.io.StdIn._

object exercice2 {
  /*
  - YOUR CODE MUST START RIGHT AFTER THIS COMMENT -
  Do not delete or modify anything above this comment
  */

  /*
  Write the declaration of the class requested in the statement
  and use it in the game method to make the game.
  BEGIN declaration of the class >>
  */

  class Pawn(var vcolor: String = "black", var vstrength: Int = 0) {
    def display(): Unit = {
      println("Color: " + vcolor + ", force: " + vstrength.toString)
    }

    def choiceColor(color: String): Unit = {
      this.vcolor = color
    }

    def change(turn: Int): Unit = {
      if (vcolor == "black") {
        if (turn % 2 == 0) {
          vcolor = "white"
        }
      } else {
        if (turn % 2 == 1) {
          vcolor = "black"
        }
      }
    }

    def eat(pawn: Pawn): Unit = {
      if (vcolor == "black" && pawn.vcolor == "black") {
        vcolor = "white"
        vstrength += 1
      } else if (vcolor == "white" && pawn.vcolor == "white") {
        vcolor = "black"
        vstrength -= 1
      }
    }
  }

  /*
  << END of the declaration of the class
  */
  def game(): Unit = {
    /*
    CODE THE GAME INTO THE game METHOD :
    */
    val programPawn = new Pawn()
    val playerPawn = new Pawn()
    val gameColor = readLine("What is the initial color of the game pawn?  ")
    programPawn.vcolor = gameColor
    val userColor = readLine("What is the initial color of your pawn?  ")
    playerPawn.vcolor = userColor

    var turn = 1
    var hasGameFinished: Boolean = false

    while (turn <= 5 && !hasGameFinished) {
      println("Turn " + turn.toString)
      val ch = readLine("Do you want to eat the game piece? (y/n) ")
      if (ch == "y") {
        playerPawn.eat(programPawn)

        print("Situation of your pawn;")
        playerPawn.display()
      }

      if (playerPawn.vstrength == 2) {
        println("You have won in " + turn.toString + " turns")
        hasGameFinished = true
      } else {
        val colorChange = readLine("Do you want to change the color of your pawn? (y/n)  ")
        if (colorChange == "y") {
          // color change for player pawn
          playerPawn.change(turn)
        }
        // color change for program pawn
        programPawn.change(turn)

        turn += 1
      }
    }

    if (turn == 6) {
      println("You have lost!")
    }
  }
  /*
   - YOUR CODE MUST END JUST BEFORE THIS COMMENT
   Do not delete or modify anything after this comment
    */
  def main(args: Array[String]): Unit = {
    /*
      For the exam, you MUST NOT write your code here
      */
    game()
  }
}
