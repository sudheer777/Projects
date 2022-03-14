package lab5
import cui._

class OutOfGas extends UserError("You are out of gas")
class Escaped extends UserError("You have escaped!")

enum Heading:
  case north, east, south, west

class Robot(val name: String) {
  var heading: Heading = Heading.east
  var fuel: Int = 100
  var position: (Int, Int) = (0, 0)

  private def step: Unit = {
    heading match {
      case Heading.north =>
        position = (position._1+1, position._2)
      case Heading.south =>
        position = (position._1-1, position._2)
      case Heading.east =>
        position = (position._1, position._2+1)
      case Heading.west =>
        position = (position._1, position._2-1)
    }
  }

  def move(steps: Int): Unit = {
    for (i <- 0 until steps) {
      if (fuel > 0) {
        fuel -= 1
        step
      }
    }
  }

  def turn(head: Heading) = {
    heading = head
  }
}

object maze extends Console with App {
  val rng = new scala.util.Random()
  var exit = (rng.nextInt(10), rng.nextInt(10))
  val robot = Robot("Robbie")

  def distance (p1: (Int, Int), p2: (Int, Int)) =
    val (a, b) = p1
    val (c, d) = p2
    (math.sqrt((a - c) * (a - c) + (b - d) * (b - d))).toInt

  // instruction = "Commands ::= restart | move STEPS | turn HEADING"

  override def execute(cmmd: String): String = {
    val cmdAr = cmmd.split(" ")
    val dis = distance(robot.position, exit)
    cmdAr.head match {
      case "move" =>
        if (dis == 0) {
          throw new Escaped
        } else if (robot.fuel == 0) {
          throw new OutOfGas
        } else {
          try {
            val steps = cmdAr(1).toInt
            robot.move(steps)
            val d = distance(robot.position, exit)
            if (d == 0) {
              throw new Escaped
            } else {
              s"${robot.name} at ${robot.position} heading ${robot.heading} with ${robot.fuel} units of fuel. Distance to goal = $d"
            }
          } catch {
            case _: NumberFormatException => throw UserError("Steps must be a number")
          }
        }
      case "turn" =>
        if (dis == 0) {
          throw new Escaped
        } else if (robot.fuel == 0) {
          throw new OutOfGas
        } else {
          try {
            robot.turn(Heading.valueOf(cmdAr(1)))
            s"${robot.name} at ${robot.position} heading ${robot.heading} with ${robot.fuel} units of fuel. Distance to goal = $dis"
          } catch {
            case _: IllegalArgumentException => throw UserError("Invalid heading")
          }
        }
      case "restart" =>
        exit = (rng.nextInt(10), rng.nextInt(10))
        robot.fuel = 100
        robot.position = (0, 0)
        val d = distance(robot.position, exit)
        s"${robot.name} at ${robot.position} heading ${robot.heading} with ${robot.fuel} units of fuel. Distance to goal = $d"
      case x =>  throw UserError(s"unrecognized command: $x")
    }
  }

  repl // start the repl

}