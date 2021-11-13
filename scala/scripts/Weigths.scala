import scala.collection.mutable

object Weights {

  def main(args: Array[String]): Unit = {
    val number = args(0).toLong // input weight
    getPan(number, TwoSidedPan(number)).display()
  }

  def getPan(num: Long, pan: TwoSidedPan): TwoSidedPan = {
    if (pan.isCorrect) return pan
    val b = findBoundary(num)
    if (num > b.findCumilativeSum()) {
      pan.updateAndSwitchPosition(b.rightBoundary)
      getPan(b.rightBoundary - num, pan)
    } else {
      pan.update(b.leftBoundary)
      getPan(num - b.leftBoundary, pan) // section 5: recursion
    }
  }

  case class Boundary(var leftBoundary: Long = 0, var rightBoundary: Long = 0) { // section 3: case class
    private var higherIndex: Long = 0

    def update(rIndex: Long): Unit = {
      this.higherIndex = rIndex
      this.leftBoundary = rightBoundary
      this.rightBoundary = powerOf3(rIndex)
    }

    def findCumilativeSum(): Long = {
      sumCumilativePowers(powerOf3, higherIndex - 1)
    }
  }

  def findBoundary(num: Long): Boundary = {
    var i = 0L
    val boundary = Boundary()
    while (boundary.rightBoundary <= num) {
      boundary.update(i)
      i += 1
    }
    boundary
  }

  def power(a: Long, b: Long): Long = {
    if (b == 0) return 1
    if (b == 1) return a
    a * power(a, b - 1) // section 5: recursion
  }

  def powerOf3: (Long => Long) = power(3, _) // section 2: partial application

  def sumCumilativePowers(powerFun: (Long => Long), num: Long): Long = { // section 1: higher order function
    if (num == 0) return 1
    powerFun(num) + sumCumilativePowers(powerFun, num - 1)  // section 5: recursion
  }

  sealed abstract class Pan() {
    val weights: mutable.Set[Long] = mutable.Set[Long]()

    def add(weight: Long): Unit = weights.add(weight)

    def total: Long = weights.sum
  }

  case class LeftPan() extends Pan // section 3: case class
  case class RightPan() extends Pan // section 3: case class


  case class TwoSidedPan(number: Long, leftPan: LeftPan = LeftPan(), rightPan: RightPan = RightPan()) { // section 3: case class
    private var positionPan: Pan = LeftPan()

    def isCorrect: Boolean = {
      leftPan.total - rightPan.total == number
    }

    def update(weight: Long): Unit = {
      val correctPan = positionPan match { // section 4: pattern matching
        case _: LeftPan => leftPan
        case _: RightPan => rightPan
      }
      correctPan.add(weight)
    }

    def updateAndSwitchPosition(weight: Long): Unit = {
      update(weight)
      positionPan = positionPan match { // section 4: pattern matching
        case _: LeftPan => RightPan()
        case _: RightPan => LeftPan()
      }
    }

    def display(): Unit = {
      println(s"Weights on Left pan: ${leftPan.weights.toSeq.sorted.reverse.mkString(", ")}")
      if (rightPan.weights.isEmpty) println("No weights needed on Right pan!!!")
      else println(s"Weights on Right pan: ${rightPan.weights.toSeq.sorted.reverse.mkString(", ")}")
    }
  }
}
