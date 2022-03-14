object Assignment6_2 {

  def passed(points: Int) {
    require(points >=0)
    if (points == 1) print(s"Tests Passed (1 point)")
    else print(s"Tests Passed ($points points)")
  }


  // 2A
  sealed trait Program
  sealed trait Expr
  // Write the definitions below
  // YOUR CODE HERE
  case class Const(d: Double) extends Expr
  case class Ident(s: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class FunDef(s: String, e: Expr) extends Expr
  case class FunCall(e1: Expr, e2: Expr) extends Expr
  case class FunDefMulti(s: List[String], e: Expr) extends Expr
  case class FunCallMulti(e1: Expr, e2: List[Expr]) extends Expr
  case class Let(id: String, e1: Expr, e2: Expr) extends Expr

  case class TopLevel(e: Expr) extends Program


  // 2B

  def autoCurryProgram(p: Program ): Program = p match {
    case TopLevel(e) => TopLevel(autoCurry(e))
  }
  // YOUR CODE HERE
  def autoCurry(e: Expr): Expr = {
    e match {
      case FunDef(s, e) =>
        FunDef(s, autoCurry(e))
      case FunCall(e1, e2) =>
        FunCall(autoCurry(e1), autoCurry(e2))
      case FunDefMulti(s, e1) =>
        s match {
          case f :: t => FunDef(f, autoCurry(FunDefMulti(t, autoCurry(e1))))
          case Nil => autoCurry(e1)
        }
      case FunCallMulti(e1, e2) =>
        e2 match {
          case f :: t =>  autoCurry(FunCallMulti(FunCall(autoCurry(e1), autoCurry(f)), t))
          case Nil => autoCurry(e1)
        }
      case Let(id, e1, e2) =>
        Let(id, autoCurry(e1), autoCurry(e2))
      case Plus(e1, e2) =>
        Plus(autoCurry(e1), autoCurry(e2))
      case _ => e
    }
  }


  case class RuntimeError(s: String) extends Exception {
    override def toString(): String = f"Runtime error: $s"
  }

  sealed trait Value
  case class Num(f: Double) extends Value
  case class Closure(x: String, e:Expr, env: Map[String, Value]) extends Value

  def binop(v1: Value)(v2: Value)(op: (Double, Double) => Double) = (v1, v2) match {
    case (Num(f1), Num(f2)) => Num(op(f1, f2))
    case _ =>  throw new RuntimeError("Runtime type mismatch")
  }
  def evalExpr(e: Expr, env: Map[String, Value]): Value = e match {
    case Const(f) => Num(f)
    case Ident(x) => if (env contains x) { env(x)} else { throw new RuntimeError("Unknown identifer")}
    case Plus(e1, e2) => binop(evalExpr(e1, env))(evalExpr(e2, env))(_ + _)
    case Let(x, e1, e2) => {
      val v1 = evalExpr(e1, env)
      evalExpr(e2, env +(x -> v1))
    }
    case FunDef(x, e) => Closure(x, e, env)
    case FunCall(e1, e2) => {
      val v1 = evalExpr(e1, env)
      v1 match {
        case Closure(p, eBody, envClosure) => {
          val v2 = evalExpr(e2, env)
          evalExpr(eBody, envClosure+( p -> v2))
        }
        case _ => throw new RuntimeError("Runtime function call type mismatch")
      }
    }
    case FunDefMulti(_, _) => throw new RuntimeError("You were supposed to get rid of multiple function calls")
    case FunCallMulti(_, _) => throw new RuntimeError("You were supposed to get rid of multiple function calls")

  }


  def testResultHasNoMultiFun(e: Expr): Boolean = e match {
    case Const(_) => true
    case Ident(_) => true
    case Plus(e1, e2) => testResultHasNoMultiFun(e1) && testResultHasNoMultiFun(e2)
    case Let(x, e1, e2) => testResultHasNoMultiFun(e1) && testResultHasNoMultiFun(e2)
    case FunDef(x, e1) => testResultHasNoMultiFun(e1)
    case FunCall(e1, e2) => testResultHasNoMultiFun(e1) && testResultHasNoMultiFun(e2)
    case FunDefMulti(_, _) => throw new RuntimeError("You were supposed to get rid of multiple function calls")
    case FunCallMulti(_, _) => throw new RuntimeError("You were supposed to get rid of multiple function calls")

  }

  def main(args: Array[String]): Unit = {
    {
      val x = Ident("x")
      val y = Ident("y")
      val foo = Ident("foo")
      val fun1 = FunDefMulti(List("x", "y"), Plus(x, y))
      val l1 = Let("foo", fun1, FunCallMulti(foo, List(Const(10.0), Const(20.0))))
      val p1 = TopLevel(l1)
      passed(3)
    }

    {
      val x = Ident("x")
      val foo = Ident("foo")
      val fcall = FunCallMulti(foo, List())
      val fdef = FunDefMulti(List(), x)
      val l2 = Let("foo", fdef, fcall)
      val l3 = Let("x", Const(10), l2)
      val p2 = TopLevel(l3)
      passed(4)
    }

    {
      val x = Ident("x")
      val foo = Ident("foo")
      val fcall = FunCall(foo, Const(10))
      val fdef = FunDef("x", Plus(x, Const(10)))
      val l2 = Let("foo", fdef, fcall)
      val l3 = Let("x", Const(10), l2)
      val p2 = TopLevel(l3)
      passed(4)
    }

    {
      val x1 = Ident("x1")
      val x2 = Ident("x2")
      val x3 = Ident("x3")

      // function (x1, x2, x3) (x1 + x2 + x3)

      val e1 = FunDefMulti(List("x1", "x2", "x3"), Plus(x1, Plus(x2, x3)) )

      val f1 = autoCurry(e1)

      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f1)
      println("Success!!")
      println("Checking result is correct:")
      // check that result is function (x1) function (x2) function (x3) x1 + x2 + x3
      assert(f1 == FunDef("x1", FunDef("x2", FunDef("x3",Plus(x1, Plus(x2, x3))  ))))

      passed(3)
    }

    {
      val x1 = Ident("x1")
      val x2 = Ident("x2")
      val x3 = Ident("x3")
      // ( (x1, x2, x3) => (x1+ x2+ x3 + x3)) (1, 2, 3)
      val e1 = FunDefMulti(List("x1", "x2", "x3"), Plus(x1, Plus(x2, Plus(x3, x3)) ))
      val e2 = FunCallMulti(e1, List(Const(1), Const(2), Const(3)))

      val f2 = autoCurry(e2)
      println(f2)

      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f2)
      println("Success!!")
      println("Checking result is correct:")
      assert(evalExpr(f2, Map.empty)== Num(9.0))
      passed(3)
    }

    {
      val e1 = FunDefMulti(Nil, Const(15))
      val e2 = FunCallMulti(Ident("foo"), Nil)
      val e = Let("foo", e1, e2)

      // let foo = function () 15 in foo()
      val f = autoCurry(e)
      println(f)

      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f)
      println("Success!!")
      println("Checking result is correct:")
      assert(evalExpr(f, Map.empty) == Num(15.0))
      passed(3)
    }

    {
      val x1 = Ident("x1")
      val x2 = Ident("x2")
      val x3 = Ident("x3")

      /*
        let foo = function (x) {
                 (function (x1, x2, x3) x1 + x2 + x3 + x3) (1, 2, 3) + x
        } in
          foo(10)
      */

      val e1 = FunDefMulti(List("x1", "x2", "x3"), Plus(x1, Plus(x2, Plus(x3, x3)) ))
      val e2 = FunCallMulti(e1, List(Const(1), Const(2), Const(3)))

      val e3 = FunDef("x", Plus(e2, Ident("x")))

      val e4 = Let("foo", e3 , FunCall(Ident("foo"), Const(10)))

      val f4 = autoCurry(e4)
      println(f4)


      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f4)
      println("Success!!")
      println("Checking result is correct:")
      assert( evalExpr(f4, Map.empty) == Num(19.0))

      passed(3)
    }

    {
      val x1 = Ident("x1")
      val x2 = Ident("x2")
      val x3 = Ident("x3")
      val x4 = Ident("x4")
      val x5 = Ident("x5")


      /*
        let foo = function (x1, x2) {
                 (function (x3, x4, x5) x1 + x2 + x2 + x3 + x4 + x5 + x5 )
        } in
          foo(10,20)(30, 40, 50)
      */

      val e1 = FunDefMulti(List("x1", "x2"), FunDefMulti(List("x3", "x4", "x5"),
        Plus(x1,
          Plus(x2, Plus(x2,
            Plus(x3,
              Plus(x4, Plus(x5, x5))
            )))
        ) ))

      val e2 = FunCallMulti(Ident("foo"), List(Const(10), Const(20)))

      val e3 = FunCallMulti(e2, List(Const(30), Const(40), Const(50)))

      val e4 = Let("foo", e1 , e3)

      val f4 = autoCurry(e4)

      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f4)
      println("Success!!")
      println("Checking result is correct:")

      assert( evalExpr(f4, Map.empty) == Num(220.0))

      passed(3)
    }

    {
      /*
  let foo = function (x1, x2, x3){
              function () {
                   x1 + x1 + x2 + x3
              }
        } in
  let bar = function() ( 10 + foo(5, 2, 1)()) in
   bar() + ( (function() 10) () )
*/

      val x1 = Ident("x1")
      val x2 = Ident("x2")
      val x3 = Ident("x3")
      val x4 = Ident("x4")
      val x5 = Ident("x5")

      val e1 = FunDefMulti(List("x1", "x2", "x3"), FunDefMulti(Nil, Plus(x1, Plus(x1, Plus(x2, x3)))))
      val e2 = FunDefMulti(Nil, Plus(Const(10), FunCallMulti(FunCallMulti(Ident("foo"), List(Const(5),Const(2), Const(1))), Nil)))
      val e3 = Plus(FunCallMulti(Ident("bar"), Nil), FunCallMulti( FunDefMulti(Nil, Const(10)), Nil))

      val e = Let("foo", e1, Let("bar", e2, e3))

      val f = autoCurry(e)

      println("Checking that result from your function has no multiple arg functions left.")
      testResultHasNoMultiFun(f)
      println("Success!!")
      println("Checking result is correct:")

      val v = evalExpr(f, Map.empty)
      println(v)
      assert( v == Num(33.0))

      passed(10)
    }

  }
}
