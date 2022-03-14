package scala_sessions

object Session2 {
  /*
    PROBLEM STATEMENT:
    top level all of them are shapes
    trainagle, rectangle, square, circle are shapes
    We need to implement circle and square share which should give area, isValid, type, perimeter

    Input would cordinates, for circle input will be 3 points (one is centre, other on circle)
    Input for square will be 4 points


    Exercise:
    create class for Square and compute all 4 available def/val
    create class for SuperSquare which should have each side of size 10 on top of Square

    in the main create points and objects for square and supersquare and print all the available attributes for them.
    Refer Circle and follow similar way
   */

  // this is very similar to interface in JAVA
  // kind of base class
  trait Shape {
    def getArea(): Double

    def isValid(): Boolean

    val typ: String

    def perimeter(): Double
  }

  def computeDistance(p1: Point, p2: Point): Double = {
    math.sqrt(((p1.y - p2.y) * (p1.y - p2.y)) + ((p1.x - p2.x) * (p1.x - p2.x)))
  }

  case class Point(x: Int, y: Int)

  class Circle(centre: Point, point1: Point, point2: Point) extends Shape {
    // private vals are not accessable from created object
    //protected vals are not accessable from created object but accessable from classes which are derived from this base class
    protected val radius = computeDistance(centre, point1)
    println("Radius of circle: " + radius)
    private val pi = 3.1417D

    override val typ: String = "Circle"

    override def perimeter(): Double = {
      2 * pi * radius
    }

    override def isValid(): Boolean = {
      val radius2 = computeDistance(centre, point2)
      if (radius == radius2) true else false
    }

    override def getArea(): Double = {
      pi * radius * radius
    }
  }

  // radius is greater than 10
  class SuperCircle(centre1: Point, point11: Point, point21: Point) extends Circle(centre1, point11, point21) {
    override val typ: String = "Super Circle"

    override def isValid(): Boolean = {
      super.isValid() && radius > 10
    }
  }

  case class Test(a: String, b: Int) {
    // top level code is constructor and gets executed immediately after any new object is created from instantiating this class
    var c = a + a + b.toString

    // function which can be called only from the objected created by this class
    def updateC(d: Int): Unit = {
      c = c + d.toString
    }
  }

  def main(args: Array[String]): Unit = {
    val t1 = new Test("te", 10)
    val t2 = new Test("tes", 15)

    println(t1.c)
    println(t2.c)

    t1.updateC(20)
    t1.updateC(50)
    t2.updateC(30)
    println(t1.c)
    println(t2.c)

    // dont need new if its case class
    val centre = Point(11,0)
    val point1 = Point(0,0)
    val point2 = Point(22,0)
    val circle1 = new Circle(centre, point1, point2)

    println("Area: " + circle1.getArea())
    println("type: " + circle1.typ)
    println("Perimeter: " + circle1.perimeter())
    println("is valid: " + circle1.isValid())

    val circle2 = new SuperCircle(centre, point1, point2)
    println("SuperCircle Area: " + circle2.getArea())
    println("SuperCircle type: " + circle2.typ)
    println("SuperCircle Perimeter: " + circle2.perimeter())
    println("SuperCircle is valid: " + circle2.isValid())
  }
}
