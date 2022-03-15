import scala.annotation.tailrec

import scala.language.implicitConversions

object Assignment {

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
      case Const(_) => Const(0.0)
      case Ident(e) => if (e.s == x) Const(1.0) else Const(0.0)
      case Plus(e1, e2) =>
        Plus(derivativeExpr(e1, x), derivativeExpr(e2, x))
      case Minus(e1, e2) =>
        Minus(derivativeExpr(e1, x), derivativeExpr(e2, x))
      case Div(e1, e2) =>
        Minus(Div(derivativeExpr(e1, x), e2), Div(Mult(e1, derivativeExpr(e2, x)), Mult(e2, e2)))
      case Mult(e1, e2) =>
        Plus(Mult(e2, derivativeExpr(e1, x)), Mult(e1, derivativeExpr(e2, x)))
      case Sine(e) => Cosine(e)
      case Cosine(e) => Mult(Const(-1.0), Sine(e))
      case Exp(e) => Mult(Exp(e), derivativeExpr(e, x))
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
      case c: Const => c
      case i: Ident => i
      case Mult(e1, e2) =>
        val k1 = ConstFold(e1)
        val k2 = ConstFold(e2)
        k1 match {
          case const: Const =>
            k2 match {
              case const1: Const => const.f * const1.f
              case _ => Mult(k1, k2)
            }
          case _ =>
            Mult(k1, k2)
        }

      case Plus(e1, e2) =>
        val k1 = ConstFold(e1)
        val k2 = ConstFold(e2)
        k1 match {
          case const: Const =>
            k2 match {
              case const1: Const => const.f + const1.f
              case _ => Plus(k1, k2)
            }
          case _ =>
            Plus(k1, k2)
        }

      case Minus(e1, e2) =>
        val k1 = ConstFold(e1)
        val k2 = ConstFold(e2)
        k1 match {
          case const: Const =>
            k2 match {
              case const1: Const => const.f - const1.f
              case _ => Minus(k1, k2)
            }
          case _ =>
            Minus(k1, k2)
        }

      case Div(e1, e2) =>
        val k1 = ConstFold(e1)
        val k2 = ConstFold(e2)
        k1 match {
          case const: Const =>
            k2 match {
              case const1: Const =>
                if (math.abs(const1.f) > 1E-09) const.f / const1.f
                else throw new IllegalArgumentException("Division by Zero Error -bailing out")
              case _ => Div(k1, k2)
            }
          case _ =>
            Div(k1, k2)
        }
      case s => throw new IllegalArgumentException(s"Unsupported $s")
    }
  }

  // 2A
  @tailrec
  def isFibonacciList(lst: List[Int]): Boolean = {
    if (lst.length <= 2) {
      true
    } else {
      lst match {
        case a :: b if a + b(0) == b(1) =>
          isFibonacciList(b)
        case _ => false
      }
    }
  }

  // 2B
  @tailrec
  def longestAscendingPrefix(lst: List[Int], ind: Int = 0): Int = {
    lst match {
      case a :: b =>
        if (b.length == 0 || a > b(0)) {
          ind
        } else {
          longestAscendingPrefix(b, ind + 1)
        }
      case _ => ind
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
    println(isFibonacciList(Nil))

    println(longestAscendingPrefix(List(1,2,3,4,3)))
    println(longestAscendingPrefix(List(5,2,3,4,3)))
    println(longestAscendingPrefix(List()))
    println(longestAscendingPrefix(List(1,2,3,4,10)))
  }
}
