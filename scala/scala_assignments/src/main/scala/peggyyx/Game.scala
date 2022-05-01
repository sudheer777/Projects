package peggyyx
// package cw
import scala.annotation.tailrec

/**
 * This class holds an instance of a simple game where
 * a player moves on a field and collects coins.
 * See the explanation sheet and comments in this file for details. The constructor builds an
 * instance of a game using the accepted parameters.
 *
 * @param wall A list of coordinates (as tuples) where walls exist. Example: The parameter List((0,0),(0,1)) puts two wall elements in the upper left corner and the position below.
 * @param coin A list of coins, each is a position and a value (i.e. a 3 value tuple). Example: List((0,0,50)) puts a coin in the upper left corner which adds 50 to the score.
 * @param initialX The initial x position of the player.
 * @param initialY The initial y position of the player. If initialX and initialY are 0, the player starts in the upper left corner.
 */
class Game(wall: List[(Int, Int)], coin: List[(Int, Int, Int)], initialX: Int, initialY: Int) {

  //the current grid, a 10x10 field, where -1=empty, 0=wall, any positive number=coin
  private var field: Array[Array[Int]] = Array.ofDim[Int](10, 10)

  /* Please note - to align with the overall case study (see explanation sheet), both of the above two-dimensional arrays
   * should be accessed in the format field(col)(row) so field(2)(0) would retrieve the 3rd column and the 1st row (as indexing starts at zero),
   * equivalent to an (x,y) coordinate of (2,0). You may therefore visualise each inner array as representing a column of data.
   */

  //the current score, initially 0
  private var score: Int = 0
  //the current player position. As the player moves these positions update.
  private var positionX: Int = initialX
  private var positionY: Int = initialY
  //the current X and Y save position, initially -1
  private var saveX: Int = -1
  private var saveY: Int = -1

  /* This code is executed as part of the constructor. It firstly initialises all cells to -1 (i.e. empty).
   * It uses the list of walls provided to initialise the walls in the field array by setting given coordinates to 0.
   * It then uses the list of coins to initialise the coins in the field array by setting given coordinates to the provided number.
   */
  for (i <- 0 until 10; k <- 0 until 10) field(i)(k) = -1
  wall.foreach(w => field(w._1)(w._2) = 0)
  coin.foreach(w => field(w._1)(w._2) = w._3)

  /**
   * Repeatedly run a sequence of commands. For example:
   *    for(i <- 1 to 5) println("Hello")
   * can be replaced by
   *    rpt(5)(println("Hello"))
   */
  def rpt(n: Int)(commands: => Unit) {
    for (i <- 1 to n) { commands }
  }

  /**
   * Utilised for GameApp.scala
   */
  def printField(): Unit ={
    for(k<-0 until 10){
      for(i<-0 until 10){
        if(positionX==i && positionY==k){
          print("p")
        }else if(saveX==i && saveY==k){
          print("s")
        }else if(field(i)(k)== 0){
          print("w")
        }else if(field(i)(k)== -1){
          print(".")
        }else{
          print(field(i)(k))
        }
      }
      println()
    }
  }

  /********************************************************************************
   * COURSEWORK STARTS HERE - COMPLETE THE DEFINITIONS OF EACH OF THE OPERATIONS
   * WE SUGGEST YOU RUN THE GameTest SUITE AFTER EVERY CHANGE YOU MAKE TO THESE
   * SO YOU CAN SEE PROGRESS AND CHECK THAT YOU'VE NOT BROKEN ANYTHING THAT USED
   * TO WORK.
   *******************************************************************************/

  /**
   * Returns the current position of the player as a tuple, in (x,y) order.
   */
  def getPlayerPos(): (Int, Int) = {
    (positionX, positionY)
  }

  /**
   * Returns the current score.
   */
  def getScore(): Int = {
    score
  }

  /**
   * Move the player one place to the left.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft(): Unit = {
    if (positionX <= 0) {
      return
    }
    val nPossibleX = positionX - 1
    val nPossibleValue = field(nPossibleX)(positionY)
    val isValidMove = nPossibleValue != 0
    if (isValidMove) {
      positionX = nPossibleX
      checkCoins()
      checkCoin()
    }
  }

  /**
   * Move the player one place to the right.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight(): Unit = {
    if (positionX >= 9) {
      return
    }
    val nPossibleX = positionX + 1
    val nPossibleValue = field(nPossibleX)(positionY)
    val isValidMove = nPossibleValue != 0
    if (isValidMove) {
      positionX = nPossibleX
      checkCoins()
      checkCoin()
    }
  }

  /**
   * Move the player one place up.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp(): Unit = {
    if (positionY <= 0) {
      return
    }
    val nPossibleY = positionY - 1
    val nPossibleValue = field(positionX)(nPossibleY)
    val isValidMove = nPossibleValue != 0
    if (isValidMove) {
      positionY = nPossibleY
      checkCoins()
      checkCoin()
    }
  }

  /**
   * Move the player one place down.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown(): Unit = {
    if (positionY >= 9) {
      return
    }
    val nPossibleY = positionY + 1
    val nPossibleValue = field(positionX)(nPossibleY)
    val isValidMove = nPossibleValue != 0
    if (isValidMove) {
      positionY = nPossibleY
      checkCoins()
      checkCoin()
    }
  }

  /**
   * Move the player n places to the left. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft(n: Int): Unit = {
    @tailrec
    def mLeft(ind: Int): Unit = {
      if (ind < n) {
        moveLeft()
        mLeft(ind + 1)
      }
    }

    mLeft(0)
  }

  /**
   * Move the player n places to the right. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight(n: Int): Unit = {
    @tailrec
    def mRight(ind: Int): Unit = {
      if (ind < n) {
        moveRight()
        mRight(ind + 1)
      }
    }

    mRight(0)
  }

  /**
   * Move the player n places up. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp(n: Int): Unit = {
    @tailrec
    def mUp(ind: Int): Unit = {
      if (ind < n) {
        moveUp()
        mUp(ind + 1)
      }
    }

    mUp(0)
  }

  /**
   * Move the player n places down. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown(n: Int): Unit = {
    @tailrec
    def mDown(ind: Int): Unit = {
      if (ind < n) {
        moveDown()
        mDown(ind + 1)
      }
    }

    mDown(0)
  }

  /**
   * Checks if the current position is a coin. A coin exists if the cell
   * has a value larger than 0. If a coin does exist, increase the score,
   * and then erase the coin, i.e. set it to -1.
   */
  def checkCoin(): Unit = {
    val currField = field(positionX)(positionY)
    if (currField > 0) {
      score = score + currField
      field(positionX)(positionY) = -1
    }
  }

  //The methods beyond this point (aside to those in GameBuilder which is a separate task) are more complex than those above.

  /**
   * This moves the player according to a string. The string can contain the
   * letters w, a, s, d representing up, left, down, right moves. If
   * there is a wall or the field ends, the individual move is not
   * executed. Any further moves are done. Any coins are collected and the
   * save position is evaluated.
   */
  def move(s: String): Unit = {
    var i = 0
    while (i < s.length) {
      val c = s(i)
      if (c == 'w') {
        moveUp()
      } else if (c == 'a') {
        moveLeft()
      } else if (c == 's') {
        moveDown()
      } else {
        moveRight()
      }
      i += 1
    }
    checkCoins()

  }

  /**
   * Identifies the maximum overall score in the game. This is the sum
   * of the current score and the possible score from collecting all of the remaining coins.
   * No coins are collected here, only the max score is returned.
   */
  def maxScore(): Int = {
    def getPossibleScore: Int = {
      var max = 0
      var i = 0
      var j = 0
      while (i <= 9) {
        j = 0
        while (j <= 9) {
          val c = field(i)(j)
          if (c > 0) {
            max += c
          }
          j += 1
        }
        i += 1
      }
      max
    }
    getScore() + getPossibleScore
  }

  /**
   * Checks if the rectangle defined by the current position and saved position
   * covers nine or more positions. If yes, it collects coins in it, increases the
   * score, and erases the coins. Also resets the saved position to -1,-1.
   */
  def checkCoins(): Unit = {
    def min(a: Int, b: Int): Int = {
      if (a > b) {
        b
      } else {
        a
      }
    }

    def max(a: Int, b: Int): Int = {
      if (a > b) {
        a
      } else {
        b
      }
    }

    if (saveX == -1 && saveY == -1) {
      return
    }

    val minX = min(saveX, positionX)
    val maxX = max(saveX, positionX)
    val minY = min(saveY, positionY)
    val maxY = max(saveY, positionY)

    val area = (maxX - minX + 1) * (maxY - minY + 1)
    if (area >= 9) {
      var i = minX
      var j = minY
      while (i <= maxX) {
        j = minY
        while (j <= maxY) {
          val c = field(i)(j)
          if (c > 0) {
            score += c
            field(i)(j) = -1
          }
          j += 1
        }
        i += 1
      }
      saveX = -1
      saveY = -1
    }
  }

  /**
   * This gives a string in the format for move, which collects all the available coins. No specific
   * requirements for the efficiency of the solution exist, but the solution must consist of a finite number
   * of steps. The move is combined of a number of moves given by suggestMove.
   * If these are not possible, an empty string is returned. No coins are collected
   * and the player must be at the original position after the execution of the method.
   */
  def suggestSolution(): String = {
    ""
  }

  /**
   * This gives a string in the format for move, which moves from the current position to
   * position x,y. No specific requirements for the efficiency of the solution exist. The move
   * cannot jump walls. The method is restricted to finding a path which is combined of a number of
   * left and then a number of up movement, or left/down, or right/up, or right/down movements only.
   * If this is not possible due to walls, it returns an empty string. No actual move is done. If
   * x or y are outside the field, an empty string is returned as well.
   */
  def suggestMove(x: Int, y: Int): String = {
    if (x < 0 || x > 9 || y < 0 || y > 9) {
      return ""
    }
    val c = field(x)(y)
    if (c == 0) {
      return ""
    }
    if (x >= positionX && y < positionY) {
      val r1 = (positionX + 1 to x).map(i => (i, positionY)).toList
      val u1 = (y + 1 to positionY).map(i => (x, i)).toList
      val rightUp = r1 ++ u1
      val pathNotExists = rightUp.exists(x => field(x._1)(x._2) == 0)
      if (pathNotExists) {
        val u2 = (y + 1 to positionY).map(i => (positionX, i)).toList
        val r2 = (positionX + 1 to x).map(i => (i, y)).toList
        val upRight = u2 ++ r2
        val pathNotExists1 = upRight.exists(x => field(x._1)(x._2) == 0)
        if (pathNotExists1) {
          return ""
        } else {
          val res = u2.map(x => "w") ++ r2.map(x => "d")
          return res.mkString("")
        }
      } else {
        val res = r1.map(x => "d") ++ u1.map(x => "w")
        return res.mkString("")
      }

    } else if (x >= positionX && y >= positionY) {
      val r1 = (positionX + 1 to x).map(i => (i, positionY)).toList
      val d1 = (positionY + 1 to y).map(i => (x, i)).toList
      val rightDown = r1 ++ d1
      val pathNotExists = rightDown.exists(x => field(x._1)(x._2) == 0)
      if (pathNotExists) {
        val d2 = (positionY + 1 to y).map(i => (positionX, i)).toList
        val r2 = (positionX + 1 to x).map(i => (i, y)).toList
        val downRight = d2 ++ r2
        val pathNotExists1 = downRight.exists(x => field(x._1)(x._2) == 0)
        if (pathNotExists1) {
          return ""
        } else {
          val res = d2.map(x => "s") ++ r2.map(x => "d")
          return res.mkString("")
        }
      } else {
        val res = r1.map(x => "d") ++ d1.map(x => "s")
        return res.mkString("")
      }
    } else if (x < positionX && y >= positionY) {
      val l1 = (x + 1 to positionX).map(i => (i, positionY)).toList
      val d1 = (positionY + 1 to y).map(i => (x, i)).toList
      val leftDown = l1 ++ d1
      val pathNotExists = leftDown.exists(x => field(x._1)(x._2) == 0)
      if (pathNotExists) {
        val d2 = (positionY + 1 to y).map(i => (positionX, i)).toList
        val l2 = (x + 1 to positionX).map(i => (i, y)).toList
        val downLeft = d2 ++ l2
        val pathNotExists1 = downLeft.exists(x => field(x._1)(x._2) == 0)
        if (pathNotExists1) {
          return ""
        } else {
          val res = d2.map(x => "s") ++ l2.map(x => "a")
          return res.mkString("")
        }
      } else {
        val res = l1.map(x => "a") ++ d1.map(x => "s")
        return res.mkString("")
      }
    } else {
      val l1 = (x + 1 to positionX).map(i => (i, positionY)).toList
      val u1 = (y + 1 to positionY).map(i => (x, i)).toList
      val leftUp = l1 ++ u1
      val pathNotExists = leftUp.exists(x => field(x._1)(x._2) == 0)
      if (pathNotExists) {
        val u2 = (y + 1 to positionY).map(i => (positionX, i)).toList
        val l2 = (x + 1 to positionX).map(i => (i, y)).toList
        val upLeft = u2 ++ l2
        val pathNotExists1 = upLeft.exists(x => field(x._1)(x._2) == 0)
        if (pathNotExists1) {
          return ""
        } else {
          val res = u2.map(x => "w") ++ l2.map(x => "a")
          return res.mkString("")
        }
      } else {
        val res = l1.map(x => "a") ++ u1.map(x => "w")
        return res.mkString("")
      }
    }
  }


  /* --- The three save methods below are used by the unit tests to simulate certain conditions --- */

  /**
   * Updates saveX and saveY to the current player position.
   */
  def save(): Unit = {
    /* This method is already implemented. You should not change it */
    saveX = positionX
    saveY = positionY
  }

  /**
   * Returns the current save position as a tuple, in (x,y) order.
   */
  def getSavePos(): (Int, Int) = {
    /* This method is already implemented. You should not change it */
    return (saveX, saveY);
  }

  /**
   * Sets the savePos to the values of the parameters.
   */
  def setSavePos(saveX: Int, saveY: Int): Unit = {
    /* This method is already implemented. You should not change it */
    this.saveX = saveX
    this.saveY = saveY
  }

}

/**
 * This object builds and returns a standard instance of Game.
 * It is used by the unit tests to initialise the game in different states.
 * Currently, there are three ways in which a game can be initialised,
 * the first has been completed but the other two initialisation methods need writing.
 */
object GameBuilder {

  /**
   * @return A game with
   * - walls in positions 3,0 3,1 and 3,2
   * - a coin at 4,1 which increases score by 100
   * - a coin at 3,3 which increases score by 250
   * - the player starting in position 0,0
   */
  def initialiseGame1(): Game = {
    /* This method is already implemented. You should not change it */
    return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 100), (3, 3, 250)), 0, 0)
  }

  /**
   * @return A game with
   * - walls in positions 3,3 3,4 3,5 5,3 5,4 and 5,5
   * - a coin at 4,4 which increases score by 200
   * - a coin at 6,3 which increases score by 200
   * - the player starting in position 3,2
   */
  def initialiseGame2(): Game = {
    return new Game(List((3, 3), (3, 4), (3, 5), (5, 3), (5, 4), (5, 5)), List((4, 4, 200), (6, 3, 200)), 3, 2)
  }

  /**
   * @return A game with
   * - walls in positions 3,0 3,1 and 3,2
   * - a coin at 4,1 which increases score by 300
   * - a coin at 3,3 which increases score by 150
   * - the player starting in position 4,1
   */
  def initialiseGame3(): Game = {
    return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 300), (3, 3, 150)), 4, 1)
  }
}