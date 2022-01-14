package fr.esgi.al.funprog

import com.typesafe.config.{Config, ConfigFactory}
import play.api.libs.json._

import java.io.PrintWriter

object Main extends App {
  case class Point(x: Int, y: Int)
  case class PointDirection(point: Point, direction: Char)

  case class Tondeuses(debut: PointDirection, instructions: Array[Char], fin: PointDirection)
  case class Record(limite: Point, tondeuses: Array[Tondeuses])

  implicit val PointWrites = new Writes[Point] {
    def writes(p: Point) = Json.obj(
      "x"  -> p.x,
      "y" -> p.y
    )
  }

  implicit val PointDirectionWrites = new Writes[PointDirection] {
    def writes(p: PointDirection) = Json.obj(
      "point"  -> p.point,
      "direction" -> p.direction.toString
    )
  }

  implicit val TondeusesWrites = new Writes[Tondeuses] {
    def writes(t: Tondeuses) = Json.obj(
      "debut"  -> t.debut,
      "instructions" -> t.instructions.map(_.toString),
      "fin" -> t.fin
    )
  }


  implicit val RecordWrites = new Writes[Record] {
    def writes(rec: Record) = Json.obj(
      "limite"  -> rec.limite,
      "tondeuses" -> rec.tondeuses
    )
  }


  class Mover(grid: Point) {

    def executeSteps(position: PointDirection, instructions: String): PointDirection = {
      val moveArr = new Array[PointDirection](instructions.length)
      instructions.zipWithIndex.foreach(c => {
        if (c._2 == 0) {
          moveArr(c._2) = executeStep(position, c._1)
        } else {
          moveArr(c._2) = executeStep(moveArr(c._2-1), c._1)
        }
      })
      moveArr(instructions.length-1)
    }

    private def executeStep(position: PointDirection, e: Char): PointDirection = {
      e match {
        case 'D' =>
          val resDirection = position.direction match {
            case 'N' => 'E'
            case 'S' => 'W'
            case 'E' => 'S'
            case 'W' => 'N'
          }
          PointDirection(position.point, resDirection)
        case 'G' =>
          val resDirection = position.direction match {
            case 'N' => 'W'
            case 'S' => 'E'
            case 'E' => 'N'
            case 'W' => 'S'
          }
          PointDirection(position.point, resDirection)
        case 'A' =>
          position.direction match {
            case 'N' =>
              if (position.point.y + 1 > grid.y) {
                position
              } else {
                PointDirection(Point(position.point.x, position.point.y+1), position.direction)
              }
            case 'S' =>
              if (position.point.y - 1 < 0) {
                position
              } else {
                PointDirection(Point(position.point.x, position.point.y-1), position.direction)
              }
            case 'E' =>
              if (position.point.x + 1 > grid.x) {
                position
              } else {
                PointDirection(Point(position.point.x+1, position.point.y), position.direction)
              }
            case 'W' =>
              if (position.point.x - 1 < 0) {
                position
              } else {
                PointDirection(Point(position.point.x-1, position.point.y), position.direction)
              }
          }
        case _ =>
          println("Malformed instruction char: " + e.toString)
          System.exit(1)
          PointDirection(Point(0, 0), ' ')
      }
    }
  }

  def convertToInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }


  println("Ici le programme principal")
  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")

  val conf: Config = ConfigFactory.load()
  val message: String = conf.getString("appplication.input-file")
  val fc =  scala.io.Source.fromFile(message)
  val lines = fc.getLines()
  val head = lines.next()
  val a = head.split(" ")
  val ax = convertToInt(a(0))
  val ay = convertToInt(a(1))
  if (ax.isEmpty || ay.isEmpty) {
    if (ax.isEmpty) println("Malformed x value for grid: " + a(0))
    else println("Malformed y value for grid: " + a(1))
    System.exit(1)
  } else {
    val limite = Point(ax.getOrElse(0), ay.getOrElse(0))
    val mover = new Mover(limite)
    val tondeuses = lines.grouped(2).map(i => {
      val inp = i(0).split(" ")
      val inpx = convertToInt(inp(0))
      val inpy = convertToInt(inp(1))
      if (inpx.isEmpty || inpx.isEmpty) {
        if (inpx.isEmpty) println("Malformed x value for input: " + inp(0))
        else println("Malformed y value for input: " + inp(1))
        System.exit(1)
      } else {
        println("Input x: " + inp(0) + ", y: " + inp(1))
      }
      val inPoint = Point(inpx.getOrElse(0), inpy.getOrElse(0))
      val d = inp(2)
      if (d == "N" || d == "S" || d == "W" || d == "E") {
        println("direction: " + d)
      } else {
        println("Malformed direction: " + d)
        System.exit(1)
      }
      val direction = d.head
      val ic = PointDirection(inPoint, direction)
      val inst = i(1)
      val fc = mover.executeSteps(ic, inst)
      Tondeuses(ic, inst.toCharArray, fc)
    }).toArray
    fc.close()
    val rec = Record(limite, tondeuses)

    val writer = new PrintWriter(conf.getString("appplication.output-json-file"))
    writer.write(Json.prettyPrint(Json.toJson(rec)))
    writer.close()

    val csvWriter = new PrintWriter(conf.getString("appplication.output-csv-file"))
    csvWriter.write("numero;debut_x;debut_y;debut_direction;fin_x;fin_y;fin_direction;instructions\n")
    for ((t, i) <- tondeuses.zipWithIndex) {
      val num = i+1
      val row = Array(num.toString, t.debut.point.x.toString, t.debut.point.y.toString, t.debut.direction.toString,
        t.fin.point.x.toString, t.fin.point.y.toString, t.fin.direction.toString, t.instructions.mkString)
      csvWriter.write(row.mkString(";") + "\n")
    }
    csvWriter.close()
    println("Successfully completed!!")
  }
}
