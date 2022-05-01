package stefanrosario19

// package cw

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
    return (positionX, positionY);
  }

  /**
   * Returns the current score.
   */
  def getScore(): Int = score

  /**
   * Move the player one place to the left.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft(): Unit = {
    if (positionX > 0 && field(positionX - 1)(positionY) != 0) {
      positionX -= 1
      checkCoin()
      checkCoins()
    }
  }

  /**
   * Move the player one place to the right.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight(): Unit = {
    if (positionX < 9 && field(positionX + 1)(positionY) != 0) {
      positionX += 1
      checkCoin()
      checkCoins()
    }
  }

  /**
   * Move the player one place up.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp(): Unit = {
    if (positionY > 0 && field(positionX)(positionY - 1) != 0) {
      positionY -= 1
      checkCoin()
      checkCoins()
    }
  }

  /**
   * Move the player one place down.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown(): Unit = {
    if (positionY < 9 && field(positionX)(positionY + 1) != 0) {
      positionY += 1
      checkCoin()
      checkCoins()
    }
  }

  /**
   * Move the player n places to the left. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft(n: Int): Unit = {
    if (n > 0) {
      for (_ <- 0 until n) moveLeft()
    }
  }

  /**
   * Move the player n places to the right. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight(n: Int): Unit = {
    if (n > 0) {
      for (_ <- 0 until n) moveRight()
    }
  }

  /**
   * Move the player n places up. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp(n: Int): Unit = {
    if (n > 0) {
      for (_ <- 0 until n) moveUp()
    }
  }

  /**
   * Move the player n places down. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown(n: Int): Unit = {
    if (n > 0) {
      for (_ <- 0 until n) moveDown()
    }
  }

  /**
   * Checks if the current position is a coin. A coin exists if the cell
   * has a value larger than 0. If a coin does exist, increase the score,
   * and then erase the coin, i.e. set it to -1.
   */
  def checkCoin(): Unit = {
    if (field(positionX)(positionY) > 0) {
      score += field(positionX)(positionY)
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
    s.foreach {
      case 'w' => moveUp()
      case 'a' => moveLeft()
      case 's' => moveDown()
      case 'd' => moveRight()
      case _ =>
    }
    checkCoins()
  }

  /**
   * Identifies the maximum overall score in the game. This is the sum
   * of the current score and the possible score from collecting all of the remaining coins.
   * No coins are collected here, only the max score is returned.
   */
  def maxScore(): Int = {
    var mScore = getScore()
    field.foreach(x => {
      x.foreach(y => if (y > 0) mScore += y)
    })
    mScore
  }

  /**
   * Checks if the rectangle defined by the current position and saved position
   * covers nine or more positions. If yes, it collects coins in it, increases the
   * score, and erases the coins. Also resets the saved position to -1,-1.
   */
  def checkCoins(): Unit = {
    if (saveX == -1 && saveY == -1) return
    if (positionX != saveX && positionY != saveY) {
      val y1 = math.abs(positionY - saveY) + 1
      val x1 = math.abs(positionX - saveX) + 1
      val area = x1 * y1
      if (area >= 9) {
        val minX = math.min(positionX, saveX)
        val maxX = math.max(positionX, saveX)
        val minY = math.min(positionY, saveY)
        val maxY = math.max(positionY, saveY)
        for {
          x <- minX to maxX
          y <- minY to maxY
        } {
          if (field(x)(y) > 0) {
            score += field(x)(y)
            field(x)(y) = -1
          }
        }
        saveX = -1
        saveY = -1
      }
    }
  }

  /**
   * This gives a string in the format for move, which collects all the available coins. No specific
   * requirements for the efficiency of the solution exist, but the solution must consist of a finite number
   * of steps. The move is combined of a number of moves given by suggestMove.
   * If these are not possible, an empty string is returned. No coins are collected
   * and the player must be at the original position after the execution of the method.
   */
  def suggestSolution(): String = ""

  /**
   * This gives a string in the format for move, which moves from the current position to
   * position x,y. No specific requirements for the efficiency of the solution exist. The move
   * cannot jump walls. The method is restricted to finding a path which is combined of a number of
   * left and then a number of up movement, or left/down, or right/up, or right/down movements only.
   * If this is not possible due to walls, it returns an empty string. No actual move is done. If
   * x or y are outside the field, an empty string is returned as well.
   */
  def suggestMove(x: Int, y: Int): String = {
    if (x < 0 || x > 9 || y <0 || y > 9)
      return ""
    if (field(x)(y) == 0)
      return ""

    return ""
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