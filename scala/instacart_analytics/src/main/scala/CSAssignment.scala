import scala.annotation.tailrec

import scala.language.implicitConversions

object CSAssignment {

  sealed trait Expr
  case class Const(f: Double) extends Expr
  case class Ident(s: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Mult(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Sine(e: Expr) extends Expr
  case class Cosine(e: Expr) extends Expr
  case class Exp(e: Expr) extends Expr


  implicit def toExpr(f: Double) = Const(f)
  implicit def toExpr(f: Int) = Const(f.toDouble)
  implicit def toExpr(s: String) = Ident(s)

  def pl(e1: Expr, e2: Expr) = Plus(e1, e2)
  def ms(e1: Expr, e2: Expr) = Minus(e1, e2)
  def st(e1: Expr, e2: Expr) = Mult(e1, e2)
  def dv(e1: Expr, e2: Expr) = Div(e1, e2)

  // 1A
  def derivativeExpr(e: Expr, x: String): Expr = {
    e match {
      case Const(_) => {
        Const(0.0)
      }
      case Ident(x1) => {
        if (x1.s == x) {
          Const(1.0)
        } else {
          Const(0.0)
        }
      }
      case Plus(x1, x2) =>
        {
          val dx1 = derivativeExpr(x1, x)
          val dx2 = derivativeExpr(x2, x)
          Plus(dx1, dx2)
        }
      case Minus(x1, x2) => {
        val dx1 = derivativeExpr(x1, x)
        val dx2 = derivativeExpr(x2, x)
        Minus(dx1, dx2)
      }
      case Div(x1, x2) => {
        val dx1 = derivativeExpr(x1, x)
        val dx2 = derivativeExpr(x2, x)
        val p1 = Div(dx1, x2)
        val p2 = Div(Mult(x1, dx2), Mult(x2, x2))
        Minus(p1, p2)
      }
      case Mult(x1, x2) => {
        val dx1 = derivativeExpr(x1, x)
        val dx2 = derivativeExpr(x2, x)
        val p1 = Mult(x1, dx2)
        val p2 = Mult(x2, dx1)
        Plus(p1, p2)
      }
      case Sine(x1) => {
        Cosine(x1)
      }
      case Cosine(x1) => {
        Mult(Const(-1.0), Sine(x1))
      }
      case Exp(x1) => {
        val dx1 = derivativeExpr(x1, x)
        Mult(Exp(x1), dx1)
      }
    }
  }

  def evalExpr (e: Expr, env: Map[String, Double]): Double = e match {
    case Const (f) => f
    case Ident (str) => { if (env.contains(str)){
      env(str)
    } else {
      throw new IllegalArgumentException(s"Environment does not contain mapping for $str")
    }
    }
    case Plus(e1, e2) => {
      (evalExpr(e1, env)) + (evalExpr(e2, env))
    }

    case Minus(e1, e2) => {
      (evalExpr(e1, env)) - (evalExpr(e2, env))
    }

    case Mult(e1, e2) => {
      (evalExpr(e1, env)) * (evalExpr(e2, env))
    }

    case Div(e1, e2) => {
      val v2 = evalExpr(e2, env)
      if (math.abs(v2) > 1E-09){
        (evalExpr(e1, env)) / (v2)
      } else {
        throw new IllegalArgumentException("Division by Zero Error -bailing out")
      }
    }

    case Exp(e) => math.exp( evalExpr(e, env))

    case Sine(e) => math.sin( evalExpr(e, env))

    case Cosine(e) => math.cos(evalExpr(e, env))
  }

  def testExpressions(exp: Expr, deriv_expected: Expr, testVals: List[Double]): Boolean = {
    val tol: Double = 1E-06
    val deriv_act = derivativeExpr(exp, "x")
    testVals forall {
      x => {
        val res = math.abs( evalExpr(deriv_act, Map("x"-> x)) - evalExpr(deriv_expected, Map("x" -> x)) ) <= tol
        if (!res) { println(s"Failed at $x")}
        res
      }
    }
  }

  // 1B
  def ConstFold(e: Expr): Expr = {
    e match {
      case const: Const => {
        const
      }
      case ident: Ident => {
        ident
      }
      case Mult(x1, x2) => {
        val p1 = ConstFold(x1)
        val p2 = ConstFold(x2)
        if (p1.isInstanceOf[Const] && p2.isInstanceOf[Const]) {
          val pc1 = p1.asInstanceOf[Const]
          val pc2 = p2.asInstanceOf[Const]
          Const(pc1.f * pc2.f)
        } else {
          Mult(p1, p2)
        }
      }
      case Plus(x1, x2) => {
        val p1 = ConstFold(x1)
        val p2 = ConstFold(x2)
        if (p1.isInstanceOf[Const] && p2.isInstanceOf[Const]) {
          val pc1 = p1.asInstanceOf[Const]
          val pc2 = p2.asInstanceOf[Const]
          Const(pc1.f + pc2.f)
        } else {
          Plus(p1, p2)
        }
      }
      case Minus(x1, x2) => {
        val p1 = ConstFold(x1)
        val p2 = ConstFold(x2)
        if (p1.isInstanceOf[Const] && p2.isInstanceOf[Const]) {
          val pc1 = p1.asInstanceOf[Const]
          val pc2 = p2.asInstanceOf[Const]
          Const(pc1.f - pc2.f)
        } else {
          Minus(p1, p2)
        }
      }
      case Div(x1, x2) => {
        val p1 = ConstFold(x1)
        val p2 = ConstFold(x2)
        if (p1.isInstanceOf[Const] && p2.isInstanceOf[Const]) {
          val pc1 = p1.asInstanceOf[Const]
          val pc2 = p2.asInstanceOf[Const]
          if (pc2.f != 0) {
            Const(pc1.f / pc2.f)
          } else {
            throw new IllegalArgumentException("Division by Zero Error")
          }
        } else {
          Div(p1, p2)
        }
      }
      case s => throw new IllegalArgumentException(s"No support exists for $s")
    }
  }

  // 2A
  @tailrec
  def isFibonacciList(lst: List[Int]): Boolean = {
    lst match {
      case x if x.length <= 2 => {
        true
      }
      case x1 :: x2 :: x3 :: x => {
        if (x1 + x2 == x3) {
          isFibonacciList(x2 :: x3 :: x)
        } else {
          false
        }
      }
      case _ => {
        false
      }
    }
  }

  // 2B
  @tailrec
  def longestAscendingPrefix(lst: List[Int], index: Int = 0): Int = {
    lst match {
      case a if a.length == 1 => {
        index
      }
      case x1 :: x2 :: x => {
        if (x1 > x2) {
          index
        } else {
          longestAscendingPrefix(x2 :: x, index + 1)
        }
      }
      case _ => {
        index
      }
    }
  }

  def main(args: Array[String]): Unit = {

    val allVals = List(-5.0, -4.5, -4.0, -3.5, -3.0, -2.5, -1.9, -1.4, -1.0, -0.5, 0.1, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)
    val e11 = Plus(Ident("x"), Const(2.0))
    println(testExpressions(e11, Const(1.0), allVals))

    val e12 = Plus(Cosine(Ident("x")), Sine(Ident("x")))
    val ed12 = Minus(Cosine(Ident("x")), Sine(Ident("x")))
    println(testExpressions(e12, ed12, allVals))

    val x = Ident("x")
    val e13 = Exp(Mult(x, x))
    val ed13 = Mult(Mult(Const(2.0), x), e13)
    println(testExpressions(e13, ed13, allVals))

    val e14 = Div(x, Plus(x, Const(2.0)))
    val ed14 = Div(Const(2.0), Mult(Plus(x, Const(2.0)), Plus(x, Const(2.0))) )
    println(testExpressions(e14, ed14, allVals))

    val e15 = Sine(Mult(Exp(Minus( Cosine(Div(x,x)), Cosine(Const(1.0)) )), x))
    val ed15 = Cosine(x)
    println(testExpressions(e15, ed15, allVals))



    val e = st(pl(3, 3), dv(3, 2))
    println(ConstFold(e))

    val e1 = ms(st(2, 3), pl(3, 2))
    println(ConstFold(e1))

    val e2 = ms(st(2, "x") , pl(3, 2))
    println(ConstFold(e2))

    val e3 = st(dv("x", 3) , ms(st(3, 2), "y"))
    println(ConstFold(e3))

    println(isFibonacciList(List(3,4,7,11)))
    println(isFibonacciList(List(-1,1, 0,  0, 1, 2, 3)))
    println(isFibonacciList(Nil))

    println(longestAscendingPrefix(List(1,2,3,4,3)))
    println(longestAscendingPrefix(List(5,2,3,4,3)))
    println(longestAscendingPrefix(List()))
    println(longestAscendingPrefix(List(1,2,3,4,10)))
  }
}
