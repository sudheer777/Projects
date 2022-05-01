package bg6aui

object Assignment9 {
  // TEST HELPER
  def passed(points: Int) {
    require(points >=0)
    if (points == 1) print(s"\n*** Tests Passed (1 point) ***\n")
    else print(s"\n*** Tests Passed ($points points) ***\n")
  }

  // Problem 1A
  import scala.util.matching.Regex

  abstract class Item(val name: String) {
    /* Needs a method to return a string describing the product */
    def toString: String
    /* Two products are equal if their SKNs are equal */
    def equals (p: Item): Boolean = {
      //BEGIN SOLUTION
      name == p.name
      //END SOLUTION
    }
  }

  /*
    Note that for a product to be equal to an object of type Item, that object
    must be a product with matching skn
  */

  class Product(override val name: String,
                val skn: String,
                val qty: Int,
                val price: Double) extends Item(name) {
    //BEGIN SOLUTION
    override def toString: String = s"$name,$skn,$qty,$price"

    override def equals(p: Item): Boolean = {
      p match {
        case p: Product => skn == p.skn
        case _ => false
      }
    }
    //END SOLUTION
  }


  object Product {
    def apply(formattedInput: String): Product = {
      /*
      Input format must be
          (name with possible spaces)[optional whitespaces],[optional whitespaces](SKN)[optional whitespaces],[optional whitespaces](quantity)[optional whitespaces],[optional whitespaces](price)
      */
      //BEGIN SOLUTION
      val regex = """^\s*([a-zA-Z ]+[a-zA-Z])\s*,\s*([a-zA-Z0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+.[0-9]*)\s*""".r.pattern
      val m = regex.matcher(formattedInput)
      if (m.matches()) {
        new Product(m.group(1), m.group(2), m.group(3).toInt, m.group(4).toDouble)
      } else {
        throw new IllegalArgumentException("pattern not matching")
      }
      //END SOLUTION
    }
  }

  // problem 1B
  //BEGIN SOLUTION
  class Service(override val name: String,
                val service_id: String,
                val rate: Double) extends Item(name) {
    override def toString: String = s"$name,$service_id,$rate"

    override def equals(p: Item): Boolean = {
      p match {
        case s: Service => s.name == name && s.service_id == service_id
        case _ => false
      }
    }
  }
  //END SOLUTION


  // Problem 2B
  def computeCost(i:Item, nos:Int): Double = {
    //BEGIN SOLUTION
    i match {
      case p: Product => p.price * nos
      case s: Service => s.rate * nos
      case _ => throw new IllegalArgumentException("cost cannot be computed")
    }
    //END SOLUTION
  }

  // Problem 2A
  abstract class Shape {
    def getCenter:(Double, Double, Double)
    def translate(xShift: Double, yShift: Double, zShift: Double): Shape
  }

  trait WithCorners {
    def getVertices: List[(Double, Double, Double)]
  }

  class Ellipsoid(val center: (Double, Double, Double), val axisLengths: (Double, Double, Double)) extends Shape {
    //TODO: Finish the methods that need to be implemented.
    //BEGIN SOLUTION
    override def getCenter: (Double, Double, Double) = center

    override def translate(xShift: Double, yShift: Double, zShift: Double): Ellipsoid = {
      new Ellipsoid((center._1 + xShift, center._2 + yShift, center._3 + zShift), axisLengths)
    }
    //END SOLUTION
  }

  class Polyhedron(val listOfVerts: List[(Double, Double, Double)]) extends Shape with WithCorners {
    assert(listOfVerts.length >= 1)

    // TODO: Finish the methods that need to be implemented.
    //BEGIN SOLUTION
    override def getCenter: (Double, Double, Double) = {
      val len = listOfVerts.length
      val sum = listOfVerts.reduce((x, y) => (x._1+y._1, x._2+y._2, x._3+y._3))
      (sum._1/len, sum._2/len, sum._3/len)
    }

    override def translate(xShift: Double, yShift: Double, zShift: Double): Polyhedron = {
      new Polyhedron(listOfVerts.map(x => (x._1+xShift, x._2+yShift, x._3+zShift)))
    }

    override def getVertices: List[(Double, Double, Double)] = {
      listOfVerts
    }
    //END SOLUTION
  }

  //TODO: Complete definitions of triangle, rectangle and circle classes.
  //BEGIN SOLUTION
  class Pyramid(x0: (Double, Double, Double), x1: (Double, Double, Double), x2: (Double, Double, Double), x3: (Double, Double, Double))
    extends Polyhedron(List(x0,x1,x2,x3)) with WithCorners {
    override def translate(xShift: Double, yShift: Double, zShift: Double): Pyramid = {
      new Pyramid(
        (x0._1+xShift, x0._2+yShift, x0._3+zShift),
        (x1._1+xShift, x1._2+yShift, x1._3+zShift),
        (x2._1+xShift, x2._2+yShift, x2._3+zShift),
        (x3._1+xShift, x3._2+yShift, x3._3+zShift))
    }
  }

  class Cuboid(lowerLeft: (Double, Double, Double), length: Double, width: Double, height:Double)
    extends Polyhedron(List(lowerLeft, (lowerLeft._1+length, lowerLeft._2, lowerLeft._3),
      (lowerLeft._1+length, lowerLeft._2+width, lowerLeft._3), (lowerLeft._1, lowerLeft._2+width, lowerLeft._3),
      (lowerLeft._1, lowerLeft._2, lowerLeft._3+height), (lowerLeft._1+length, lowerLeft._2, lowerLeft._3+height),
        (lowerLeft._1+length, lowerLeft._2+width, lowerLeft._3+height), (lowerLeft._1, lowerLeft._2+width, lowerLeft._3+height))) with WithCorners {
    override def translate(xShift: Double, yShift: Double, zShift: Double): Cuboid = {
      new Cuboid((lowerLeft._1+xShift, lowerLeft._2+yShift, lowerLeft._3+zShift), length, width, height)
    }
  }

  class Sphere(override val center: (Double, Double, Double), val rad: Double) extends Ellipsoid(center, (center._1+rad, center._2+rad, center._3+rad)) {
    override def translate(xShift: Double, yShift: Double, zShift: Double): Sphere = {
      new Sphere(
        (center._1+xShift, center._2+yShift, center._3+zShift),
        rad
      )
    }
  }
  //END SOLUTION

  def main(args: Array[String]): Unit = {
    {
      class MyItem(override val name: String) extends Item(name){
        override def toString: String = {
          name
        }
      }
      val p1 = new MyItem("Eggs")
      assert(p1.equals(new MyItem("Eggs")), "Failed")
      assert(!p1.equals(new MyItem("Not Eggs")), "Failed")
      passed(3)
    }
    {
      //BEGIN TEST
      val p1 = Product("Baby Wipes, 1a2e34, 100, 22.03")
      println(p1.toString)
      assert(p1.name == "Baby Wipes", "Name not parsed correctly")
      assert(p1.skn == "1a2e34", "SKN not parsed correctly")
      assert(p1.qty == 100, "qty not parsed correctly")
      assert(p1.equals(new Product("Infant Wet Wipes", "1a2e34", 9, 22.04)))
      passed(3)
      //END TEST
    }
    {
      //BEGIN TEST
      try {
        val p2 = Product("Badly Formatted String, 2bce54, $3102, 45.89")
        assert(false, "Failed, the string is badly formatted but your code is OK with it.")
      } catch {
        case e => println(s"Expected behavior seen")
      }
      passed(3)
      //END TEST
    }
    {
      //BEGIN TEST
      val e1 = Product("Wiper Blades    , 99a2ef2   , 119,  100.00")
      println(e1.toString)
      assert(e1.name == "Wiper Blades", "Product name not parsed correctly")
      assert(e1.skn == "99a2ef2", "SKN not parsed correctly")
      assert(e1.qty == 119, "Qty not parsed correctly")
      assert(!e1.equals(new Product("Wiper Blades", "random", 119, 100.00)))
      passed(3)
      //END TEST
    }

    {
      //BEGIN TEST
      val e1 = Product("Barn Door, 2014ae7, 10, 568.09 ")
      val c1 = new Service("Door installation", "20221a98", 35.00)
      assert( !c1.equals(e1), "Failed")
      assert( c1.equals(new Service("Door installation", "20221a98", 45.00)), "Failed")
      passed(4)
      //END TEST
    }
    {
      //BEGIN TEST
      val s1 = new Service("Door installation", "20221a98", 35.00)
      val s2 = new Service("Barn Door installation", "20221a98", 35.00)
      assert( !s1.equals(s2), "Failed")
      passed(4)
      //END TEST
    }
    {
      assert(computeCost(Product("Barn Door, 2014ae7, 10, 568.09 "),2) == 1136.18, "Failed")
      assert(computeCost(Product("Barn Door, 2014ae7, 10, 568.09 "),2) ==
        computeCost(Product("Washing Machine, 9024ae7, 11, 1136.18 "),1), "Failed")
      assert(computeCost(new Service("Door installation", "20221a98", 35.00), 10) == 350.0, "Failed")
      class MyItem(override val name: String) extends Item(name){
        override def toString: String = {
          name
        }
      }
      try {
        val p2 = new MyItem("generic")
        computeCost(p2, 10)
        assert(false, "Failed, Your code computes the cost of an item without knowing its price or rate")
      } catch {
        case e => println(s"Expected behavior seen")
      }
      passed(10)
    }
    {
      val el1 = new Ellipsoid((5.0, -3.5, 0.0), (2.1, 1.3, 2.0))
      val el2 = el1.translate(3,4,3)
      assert (el2.center == (8.0, 0.5, 3.0), "Test failed: After translation, the center must be (8,0.5,3.0)")

      val pl1 = new Polyhedron(List((0,0,0), (2,1,2)))
      assert (pl1.getCenter == (1.0, 0.5, 1.0), "center must be (1,0.5)")

      passed(5)
    }
    {
      val pyr = new Pyramid( (0,0, 0), (2,0,0), (1,1,0), (1.5, 1.5, 3))
      val pyr2 = pyr.translate(1,1,1)
      assert (pyr2.isInstanceOf[Pyramid])
      print(pyr2.getCenter)
      assert(pyr2.getCenter._1 == 2.125)
      assert(pyr2.getCenter._3 == 1.75)
      passed(5)
    }
    {
      val cub = new Cuboid((-1,-1,-1), 2, 2, 2)
      val listOfVerts = cub.getVertices
      assert(listOfVerts.length == 8)
      print(cub.getCenter)
      assert (cub.getCenter == (0,0,0))
      val cub2: Cuboid = cub.translate(1,1,1)
      print(cub2.getCenter)
      assert(cub2.getCenter == (1,1,1))
      passed(5)
    }
    {
      val spr = new Sphere((1,1,1), 3)
      val new_spr: Sphere = spr.translate(2,2,2)
      assert (new_spr.center == (3,3,3))
      assert (new_spr.rad == 3)
      passed(5)
    }
  }

}
