package datrollman

object Assignment6 {

  sealed trait Expr
  case class Const(d: Double) extends Expr
  case class Ident(s: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Mult(e1: Expr, e2: Expr) extends Expr
  case class Let(id: String, e1: Expr, e2: Expr) extends Expr
  case class MultiLet(id: List[String], eList: List[Expr], e2: Expr) extends Expr



  sealed trait Value
  case class NumValue(f: Double) extends Value
  case object Error extends Value /* -- Do not return Error -- simply throw an new IllegalArgumentException whenever you encounter an erroneous case --*/

  type Environment = Map[String, Value]

  def evalExpr(e: Expr, env: Environment): Value = {

    e match {
      case Const(f) => NumValue(f)
      case Ident(x) => {
        if (env.contains(x)) {
          env(x)
        } else {
          throw new IllegalArgumentException("Not found identifier")
        }
      }
      case Plus(e1, e2) => {
        val v1 = evalExpr(e1, env)
        val v2 = evalExpr(e2, env)
        (v1, v2) match {
          case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
          case _ => throw new IllegalArgumentException("plus failed")
        }
      }
      case Mult(e1, e2) => {
        val v1 = evalExpr(e1, env)
        val v2 = evalExpr(e2, env)
        (v1, v2) match {
          case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
          case _ => throw new IllegalArgumentException("mult failed")
        }
      }
      case Let(x, e1, e2) => {
        // YOUR CODE HERE
        val valn = evalExpr(e1, env)
        evalExpr(e2, env ++ Map(x -> valn))
      }
      case MultiLet(xList, eList, e2) => {
        // YOUR CODE HERE
        val updatedMap = xList.zip(eList).map {
          case (x1, x2) => (x1, evalExpr(x2, env))
        }.toMap
        evalExpr(e2, env ++ updatedMap)
      }
    }

  }

  def main(args: Array[String]): Unit = {

    {
      val x = Ident("x")
      val y = Ident("y")
      val let1 = Let("x", y, Plus(x, Mult(x, y)) )
      val mlet1 = MultiLet( List("x", "y"), List(Const(10.0), Const(20.0)), let1)
      val v = evalExpr(mlet1, Map.empty)
      println(v)
      assert(v == NumValue(420.0), s"Test 1 failed expected: NumValue(420.0), obtained $v")
    }

    {
      val x = Ident("x")
      val y = Ident("y")
      val let1 = Let("x", y, Plus(x, Mult(x, y)) )
      val mlet1 = MultiLet( List("x", "y"), List(Const(10.0), x), let1)
      try {
        val v = evalExpr(mlet1, Map.empty)
        assert(false, "Test 2 failed -- your code should detect a usage of x that is out of scope")
      } catch {
        case e:IllegalArgumentException => { println("Illegal argument exception caught -- as expected!!") }
        case _ => {println("Wrong type of exception thrown")}
      }
    }

    {
      val x = Ident("x")
      val y = Ident("y")
      val z = Ident("z")
      val w = Ident("w")
      val ten = Const(10.0)
      val twenty = Const(20.0)
      val innerLet2 = Let("w", w, Mult(x, Plus(y, w)))
      val multiLet1 = MultiLet(Nil, Nil, innerLet2)
      val e = MultiLet(List("x","y","z","w"), List(ten, ten, ten, twenty), multiLet1)
      val v = evalExpr(e, Map.empty)
      println(v)
      assert(v == NumValue(300.0), "Test2 Failed -- expected value NumValue(300.0), obtained value $v")
    }
  }
}
