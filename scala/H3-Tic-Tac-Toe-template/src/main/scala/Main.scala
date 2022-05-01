object Main {
  type Line = List[Player]
  type Board = List[Line]

  def profileID:Int = 1 // REPLACE with token received in email

  def newline = "\n"

  // creates a board from a string
  def makeBoard(s: String): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    // first split the string with new line so that string will be Array[String] and then use .toList to convert array to List
    // use map funtion on the above List to iterate through each element
    // each element will be string, so convert it to char array using .toList
    // use map function on char array and convert each char to Player using above function implemented already
    s.split("\n").toList.map(x => x.toList.map(toPos))
  }

  // checks if the position (x,y) board b is free
  def isFree(x:Int, y:Int, b:Board):Boolean = {
    // Need to return true if (x,y) position is free
    // Just check whether its Empty. If it is Empty return true else return False
    // Easy way of doing it is by == Empty which returns above behaviour
    b(x)(y) == Empty
  }

  // returns the "other" player from a position, if it exists
  def complement(p: Player): Player = p match {
    // This is to compute complement.
    // Compliment for One is Two, Two is One and Empty will be Empty
    // Used match pattern on input and then returning right value based on value of input
    case One => Two
    case Two => One
    case Empty => Empty
  }

  def show(b: Board): String = {
    // This is helper function to take player as input and returns char as output
    // player One will be X, Two will be 0 and Empty will be .
    // used match pattern to solve this
    def toPos(p: Player): Char =
      p match {
        case One => 'X'
        case Two => '0'
        case Empty => '.'
      }

    // used map to iterate each row of the board
    b.map(x => {
      // this converts each Line of the board to string using above helper function
      x.map(toPos)
        .foldLeft("")((a, b) => a  + b) // foldleft will help to convert array of char to string by just concatinating all chars
    }).reduce((a, b) => a + "\n" + b) // This reduce will concat all lines with new line and make sure last line will not have new line
  }

  // Returns a list of columns from a board
  // This will just return same board as output
  def getColumns(b:Board): Board = b

  //returns the first diagonal as a line
  def getFstDiag(b:Board): Line = {

    // This is helper function which zips index to each Line
    // if input List(l1,l2,l3), output will be List((l1, 0), (l2, 1), (l3, 2))
    def zip(l: List[Line], ind: Int): List[(Line, Int)] = {
      l match {
        case Nil => Nil
        case head :: tail => (head, ind) :: zip(tail, ind + 1)
      }
    }

    val newBoard = zip(b, 0) // new board with index for each line
    // used fpr comprehension to iterate through the board and access diagonal and returns new list using yield
    for {
      (line, ind) <- newBoard
    } yield {
      line(ind)
    }
  }

  //returns the second diagonal as a line
  def getSndDiag(b:Board): Line = {
    // This is helper function which zips index (in reverse order) to each Line
    // if input List(l1,l2,l3), output will be List((l1, 2), (l2, 1), (l3, 0))
    def zip(l: List[Line], ind: Int): List[(Line, Int)] = {
      l match {
        case Nil => Nil
        case head :: tail => (head, ind) :: zip(tail, ind - 1)
      }
    }

    val newBoard = zip(b, b.length-1) // new board with index for each line
    // used fpr comprehension to iterate through the board and access diagonal and returns new list using yield
    for {
      (line, ind) <- newBoard
    } yield {
      line(ind)
    }
  }

  // retrieves all the diagonals above the first line
  def getAboveFstDiag(b: Board): List[Line] = ???

  def getBelowFstDiag(b: Board): List[Line] = ???

  def getAboveSndDiag(b: Board): List[Line] = ???

  def getBelowSndDiag(b: Board): List[Line] = ???

  //write a function which checks if a given player is a winner
  //hints: patterns and exists
  def winner(p: Player)(b: Board): Boolean = {
    ???
  }

  /*
   * Write a function which updates a position (with a player) at given indices from the board.
   * Your function need not check if the position is empty.
   * Partial stub - you can remove it if you want to implement it another way
   */

  def update(p: Player)(ln: Int, col: Int, b: Board) : Board = ???


  /*
   * generates one possible next move for player p. Hint - use "isFree" and "update"
   *
   * */
  def next(p: Player)(b: Board): List[Board] = ???


  // for testing purposes only.
  def main(args: Array[String]) = {

  }
}

