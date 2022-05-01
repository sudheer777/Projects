package datrollman

object Assignment7 {

  // TEST HELPER
  def passed(points: Int) {
    require(points >=0)
    if (points == 1) print(s"Tests Passed (1 point)")
    else print(s"Tests Passed ($points points)")
  }

  /*- 1. Abstract Syntax Tree for Expressions */
  trait Expr
  case class Const(d: Double) extends Expr
  case class Ident(s: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Geq(e1: Expr, e2: Expr) extends Expr
  case class IfThenElse(eCond: Expr, eThen: Expr, eElse: Expr) extends Expr
  case class Let(id: String, e1: Expr, e2: Expr) extends Expr
  case class FunDef(param: String, body: Expr) extends Expr
  case class FunCall(e1: Expr, e2: Expr) extends Expr

  /*- 2. Definitions of Environments and Values -*/
  sealed trait Environment
  sealed trait Value

  case class NumValue(d: Double) extends Value
  case class BoolValue(b: Boolean) extends Value
  case class Closure(param: String, expr: Expr, env: Environment ) extends Value

  case object EmptyEnvironment extends Environment
  case class Extend(s: String, v: Value, env: Environment) extends Environment
  /* -- Define a runtime error exception --*/
  case class RunTimeError(msg: String) extends Exception


  // PROBLEM 1
  /* If v1 and v2 are NumValue, then add them, otherwise raise a
   RunTimeError exception */
  def addValue_k[T](v1: Value, v2: Value, k: Value => T ): T = (v1, v2) match {
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      k(NumValue(d1+d2))
    }
    case _ => {
      throw RunTimeError("Asked to add two values that are not numbers : type mismatch during runtime. ")
    }
  }

  /* Implement the function divValue using CPS style. However, if
     the second value v2 is zero, then ensure that you are throwing
     a RunTimeError exception */
  def divValue_k[T](v1: Value, v2: Value, k: Value => T ): T = (v1, v2) match {
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      if (d2 == 0) {
        throw RunTimeError("Division error")
      }
      k(NumValue(d1/d2))
    }
    case _ => {
      throw RunTimeError("Asked to divide two values that are not numbers : type mismatch during runtime. ")
    }
  }

  /* Implement >= comparison between numerical values. Values of any type other
  than numbers should raise a RunTimeError */
  def geqValue_k[T](v1: Value, v2: Value, k: Value => T ): T = (v1, v2) match {
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      k(BoolValue(d1 >= d2))
    }
    case _ => {
      throw RunTimeError("Asked to compare two values that are not numbers : type mismatch during runtime. ")
    }
  }

  /* Lookup a identifierr if it is present in the environment, if not, raise a
  RuntimeError */
  def lookup_k[T](env: Environment, x: String, k: Value => T): T = env match {
    case EmptyEnvironment => throw RunTimeError(s"Cannot find identifier: $x")
    case Extend(s, v, env2) if s == x => {
      // YOUR CODE HERE
      k(v)
    }
    case Extend(s, v, env2)  => {
      // YOUR CODE HERE
      lookup_k(env2, x, k)
    }
  }

  def continue1(v: Value): String = v match {
    case NumValue(d) => d.toString
    case BoolValue(b) => b.toString
    case Closure(_,_,_) => "Some Closure"
  }

  def continue2(v: Value): Int = v match {
    case NumValue(_) => 1
    case BoolValue(_) => 2
    case Closure(_,_,_) => 3
  }

  import scala.annotation.tailrec
  def self(v: Value): Value = v

  def eval_k[T](e: Expr, env: Environment, k: Value => T): T = e match {
    case Const(d) => k(NumValue(d))
    case Ident(x) => lookup_k(env, x, k)
    case Let(id, e1, e2) => {
      val val1 = eval_k(e1, env, self)
      eval_k(e2, Extend(id, val1, env), k)
    }
    case Plus(e1, e2) => addValue_k(eval_k(e1, env, self), eval_k(e2, env, self), k)
    case Div(e1, e2) => divValue_k(eval_k(e1, env, self), eval_k(e2, env, self), k)
    case Geq(e1, e2) => geqValue_k(eval_k(e1, env, self), eval_k(e2, env, self), k)
    case IfThenElse(eCond, eThen, eElse) => {
      val val1 = eval_k(eCond, env, self)
      val1 match {
        case BoolValue(x) => eval_k(if (x) eThen else eElse, env, k)
        case _ => throw RunTimeError("Expected Boolean value")
      }
    }
    case FunDef(p, b) => k(Closure(p, b, env))
    case FunCall(e1, e2) => {
      val val1 = eval_k(e1, env, self)
      val1 match {
        case Closure(p, b, _) => eval_k(b, Extend(p, eval_k(e2, env, self), env), k)
        case _ => throw RunTimeError("Runtime function call type mismatch")
      }
    }
  }

  implicit def toExpr(d: Double) : Expr = Const(d)
  implicit def toExpr(s: String): Expr = Ident(s)


  // PROBLEM 2
  /* --
  Now let's implement things with error continuation
  Implement the addValue helper function to add the two values and pass result to
  continuation if they are nummbers.
  Otherwise, call the error continuation.
-- */
  def addValueErr_k[T](v1: Value, v2: Value, k: Value => T, err_k: () => T ): T = (v1, v2) match {
    // YOUR CODE HERE
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      k(NumValue(d1+d2))
    }
    case _ => {
      err_k()
    }
  }


  /* Implement the function divValue using CPS style. However, if the second value v2 is zero,
     then ensure that you call the error continuation. */
  def divValueErr_k[T](v1: Value, v2: Value, k: Value => T, err_k: () => T ): T = (v1, v2) match {
    // YOUR CODE HERE
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      if (d2 == 0) {
        err_k()
      } else {
        k(NumValue(d1/d2))
      }
    }
    case _ => {
      err_k()
    }
  }

  // Same as above. Implement >= comparison of numbers but with error continuation called
  // if there is an error.
  def geqValueErr_k[T](v1: Value, v2: Value, k: Value => T, err_k: ()=> T ): T = (v1, v2) match {
    // YOUR CODE HERE
    case (NumValue(d1), NumValue(d2)) => {
      // YOUR CODE HERE
      k(BoolValue(d1 >= d2))
    }
    case _ => {
      err_k()
    }
  }

  //Implement lookup of a value from the environment with continuation and error continuation.
  //Error continuation will be called if there is an error.

  def lookupErr_k[T](env: Environment, x: String, k: Value => T, err_k: ()=> T ): T = env match {
    // YOUR CODE HERE
    case EmptyEnvironment => err_k()
    case Extend(s, v, env2) if s == x => {
      // YOUR CODE HERE
      k(v)
    }
    case Extend(s, v, env2)  => {
      // YOUR CODE HERE
      lookupErr_k(env2, x, k, err_k)
    }
  }

  case class NullValue() extends Value
  def errValue(): Value = NullValue()

  /*--
  Implement the eval function but now with the error continuation.
  --*/

  /*def evalErr_k[T](e: Expr, env: Environment, k: Value => T, err_k: () => T ): T = e match {
    // YOUR CODE HERE
    case Const(d) => k(NumValue(d))
    case Ident(x) => lookupErr_k(env, x, k, err_k)
    case Let(id, e1, e2) => {
      val val1 = evalErr_k(e1, env, self, errValue)
      evalErr_k(e2, Extend(id, val1, env), k, err_k)
    }
    case Plus(e1, e2) =>
      addValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case Div(e1, e2) =>
      divValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case Geq(e1, e2) =>
      geqValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case IfThenElse(eCond, eThen, eElse) => {
      val val1 = evalErr_k(eCond, env, self, errValue)
      val1 match {
        case BoolValue(x) => evalErr_k(if (x) eThen else eElse, env, k, err_k)
        case _ => err_k()
      }
    }
    case FunDef(p, b) => k(Closure(p, b, env))
    case FunCall(e1, e2) => {
      val val1 = evalErr_k(e1, env, self, errValue)
      val1 match {
        case Closure(p, b, _) => {
          val val2 = evalErr_k(e2, env, self, errValue)
          evalErr_k(b, Extend(p, val2, env), k, err_k)
        }
        case _ => err_k()
      }
    }
  }*/

  // PROBLEM 3

  case class TryCatch(e1: Expr, e2: Expr) extends Expr

  def evalErr_k[T](e: Expr, env: Environment, k: Value => T, err_k: () => T ): T = e match {
    // YOUR CODE HERE
    case Const(d) => k(NumValue(d))
    case Ident(x) => lookupErr_k(env, x, k, err_k)
    case Let(id, e1, e2) => {
      val val1 = evalErr_k(e1, env, self, errValue)
      evalErr_k(e2, Extend(id, val1, env), k, err_k)
    }
    case Plus(e1, e2) =>
      addValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case Div(e1, e2) =>
      divValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case Geq(e1, e2) =>
      geqValueErr_k(evalErr_k(e1, env, self, errValue), evalErr_k(e2, env, self, errValue), k, err_k)
    case IfThenElse(eCond, eThen, eElse) => {
      val val1 = evalErr_k(eCond, env, self, errValue)
      val1 match {
        case BoolValue(x) => evalErr_k(if (x) eThen else eElse, env, k, err_k)
        case _ => err_k()
      }
    }
    case FunDef(p, b) => k(Closure(p, b, env))
    case FunCall(e1, e2) => {
      val val1 = evalErr_k(e1, env, self, errValue)
      val1 match {
        case Closure(p, b, _) => {
          val val2 = evalErr_k(e2, env, self, errValue)
          evalErr_k(b, Extend(p, val2, env), k, err_k)
        }
        case _ => err_k()
      }
    }
    case TryCatch(e1, e2) => {
      // YOUR CODE HERE
      val val1 = evalErr_k(e1, env, self, errValue)
      val1 match {
        case NullValue() => evalErr_k(e2, env, k, err_k)
        case _ => k(val1)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    {
      val v1: String = addValue_k(NumValue(15), NumValue(30), continue1)
      assert(v1 == "45.0", s"Test 1 Fail: expected 45.0 but got: $v1")

      val v2: Int = addValue_k(NumValue(-15), NumValue(30), continue2)
      assert(v2 == 1, s"Test 2 Fail: expected 1 but got: $v2")

      val v3: String = divValue_k(NumValue(15), NumValue(30), continue1)
      assert(v3 == "0.5", s"Test 3 Fail: expected 0.5 but got: $v3")

      val v4: String = {
        try {
          divValue_k(NumValue(15), NumValue(0), continue1);
          "NOT_OK"
        } catch {
          case RunTimeError(_) => "OK!!"
        }
      }
      assert(v4 == "OK!!", s"Test 4 Fail: expected OK but got: $v4")

      val env = Extend("x", NumValue(10.0),
        Extend("y", BoolValue(true),
          EmptyEnvironment ) )

      val v5 = lookup_k(env, "y", continue2)
      assert (v5 == 2, s"Test 5 Fail: expected 2 but got: $v5")

      passed(5)
    }
    {

      val x = Ident("x")
      val y = Ident("y")
      val e1 = Plus(x, 1.0)
      val e2 = Div(e1, Plus(1.0, y))
      val e3 = Let("x", 1.0, e2)
      val e4 = Let("y", 3.0, e3)

      val v1: String = eval_k(e4, EmptyEnvironment, continue1)
      assert(v1 == "0.5", s"Test 1: Expected 0.25 obtained: {$v1}")
      // function(x) if (x >= 2) then (x+1)/(y+1) else (x+1)
      val e5 = FunDef("x", IfThenElse(Geq(x, 2.0), e2, e1))
      // let y = 4 in let f = e5 in f(y) // should return 1.0
      val e6 = Let("y", 4,
        Let("f", e5,
          FunCall(Ident("f"), y)))
      val v2 = eval_k(e6, EmptyEnvironment, continue1)
      assert(v2 == "1.0", s"Test 2: Expected 1.0 obtained {$v2}")
      passed(10)
    }
    {
      def errorHandler1() = {
        println("Successfully caught error in computation.")
        "errr"
      }

      // x + 10
      val x = Ident("x")
      val e1 = Plus(x, Const(10.0))


      val v1 = evalErr_k(e1, EmptyEnvironment, continue1, errorHandler1)
      assert (v1 == "errr", "Failed to catch error in interpreter for e1")

      val e2 = Let("x", Const(5.0), e1)
      val v2 = evalErr_k(e2, EmptyEnvironment, continue1, errorHandler1)
      assert (v2 == "15.0", s"Test 2 failed: Expected 15.0 got {$v2}")

      val e3 = Let("y", Const(5.0), e1)
      val v3 = evalErr_k(e3, EmptyEnvironment, continue1, errorHandler1)
      assert (v3 == "errr", s"Test 3 failed to catch error")

      val e4 = Let("x", Div(Const(1.0), Const(0)), Const(10))
      val v4 = evalErr_k(e3, EmptyEnvironment, continue1, errorHandler1)
      assert (v4 == "errr", s"Test 4 failed to catch error")

      val e5 = IfThenElse(Plus(5,10), Plus(10,15), Plus(15,20) )
      val v5 = evalErr_k(e5, EmptyEnvironment, continue1, errorHandler1)
      assert (v5 == "errr", s"Test 5 failed to catch error")

      val e6 = IfThenElse(Geq(Plus(5,10), Plus(10,5)), Plus(10,15), Plus(15,20) )
      val v6 = evalErr_k(e6, EmptyEnvironment, continue1, errorHandler1)
      assert (v6 == "25.0", s"Test 6 Expected: 25.0 obtained $v6")

      val e7 = FunCall(e6, Const(6.0))
      val v7 = evalErr_k(e7, EmptyEnvironment, continue1, errorHandler1)
      assert (v7 == "errr", s"Test 7: Failed to catch error")

      val e8 = Geq(Geq(10, 5), 4)
      val v8 = evalErr_k(e8, EmptyEnvironment, continue1, errorHandler1)
      assert (v8 == "errr", s"Test 8: Failed to catch error")

      val e9 = Plus(Geq(10, 5), 4)
      val v9 = evalErr_k(e9, EmptyEnvironment, continue1, errorHandler1)
      assert (v9 == "errr", s"Test 9: Failed to catch error")

      val e10 = Div(5, Geq(10, 5))
      val v10 = evalErr_k(e10, EmptyEnvironment, continue1, errorHandler1)
      assert (v10 == "errr", s"Test 10: Failed to catch error")

      passed(15)
    }

    {
      def continue1(v: Value): Double= v match {
        case NumValue(d) => d
        case BoolValue(b) => if (b) 1.0 else -1.0
        case Closure(_,_,_) => 1000.111
      }
      def continue2(v: Value): Int = v match {
        case NumValue(_) => 1
        case BoolValue(_) => 2
        case Closure(_,_,_) => 3
      }

      def errorHandler1() = {
        println("Successfully caught error in computation.")
        "error"
      }
      val x = Ident("x")
      val y = Ident("y")

      // try 1/0 catch 10 (division by zero err handle)
      val e1 = TryCatch(Div(1,0), 10)
      val v1 = evalErr_k(e1, EmptyEnvironment, continue1, errorHandler1)
      println(s"v1 = {$v1}")
      assert(v1 == 10.0, "Test 1 failed")

      // try x + 5 catch -25 (unknown identifier err handle)
      val e2 = TryCatch(Plus(x, 5), -25)
      val v2 = evalErr_k(e2, EmptyEnvironment, continue1, errorHandler1)
      println(s"v2 = {$v2}")
      assert(v2 == -25.0, "Test 2 failed")

      //try if (5+10) then 5 else 4 catch 15 (if-then-else err handle)
      val e3 = TryCatch( IfThenElse(Plus(5,10), 5, 4), 15.0)
      val v3 = evalErr_k(e3, EmptyEnvironment, continue1, errorHandler1)
      println(s"v3 = {$v3}")
      assert(v3 == 15.0, "Test 3 failed")

      //try let f = 25 in f(25) catch -5 (function call type mismatch)

      val e4 = TryCatch(Let("f", 25, FunCall("f", 25)), -5)
      val v4 = evalErr_k(e4, EmptyEnvironment, continue1, errorHandler1)
      println(s"v4 = {$v4}")
      assert(v4 == -5.0, "Test 4 failed")

      //try  25 + (10 >= 5) in  catch -5 (function call type mismatch)
      val e5 = TryCatch(Plus(25, Geq(10, 5)), Geq(10, 5))
      val v5 = evalErr_k(e5, EmptyEnvironment, continue1, errorHandler1)
      println(s"v5 = {$v5}")
      assert(v5 == 1.0, "Test 4 failed")


      passed(10)
    }
    {
      def continue1(v: Value): Double= v match {
        case NumValue(d) => d
        case BoolValue(b) => if (b) 1.0 else -1.0
        case Closure(_,_,_) => 1000.111
      }
      def continue2(v: Value): Int = v match {
        case NumValue(_) => 1
        case BoolValue(_) => 2
        case Closure(_,_,_) => 3
      }

      def errorHandler1() = {
        println("Successfully caught error in computation.")
        "error"
      }
      val x = Ident("x")
      val y = Ident("y")

      /*
     try
        let x = try 1/0 catch 10 in
          x/(x+(-10))
      catch
        15
    */


      val e11 = TryCatch(Div(1,0), 10)
      val e12 = Let("x", e11, Div("x", Plus("x", -10)))
      val e1 = TryCatch(e12, 15)
      val v1 = evalErr_k(e1, EmptyEnvironment, continue1, errorHandler1)
      println(s"v1 = {$v1}")
      assert (v1 == 15.0, "Test 1 failed")

      /*
       try
          let x = try 1/0 catch 10 in
             x + 10
        catch
          15
      */

      val e21 = TryCatch(Div(1,0), 10)
      val e22 = Let("x", e21, Plus("x", 10))
      val e2 = TryCatch(e22, 15)
      val v2 = evalErr_k(e2, EmptyEnvironment, continue1, errorHandler1)
      println(s"v2 = {$v2}")
      assert (v2 == 20.0, "Test 2 failed")



      /*
       try
          try
            try
               x + 10
            catch
              25
          catch
            20
        catch
          15
      */

      val e3 = TryCatch( TryCatch(TryCatch(Plus(x, 10), 25), 20), 15)
      val v3 = evalErr_k(e3, EmptyEnvironment, continue1, errorHandler1)
      println(s"v3 = {$v3}")
      assert (v3 == 25.0, "Test 3 failed")


      /*
      let z = 10 in
        try
          let f = function (x) y + x in
            f(10)
         catch
           (25 + z)
      */

      val e41= FunDef("x", Plus("y", "x"))
      val e42 =Let("f", e41, FunCall("f", 10.0))
      val e43 = TryCatch(e42, Plus(25, "z"))
      val e4 = Let("z", 10.0, e43)

      val v4 = evalErr_k(e4, EmptyEnvironment, continue1, errorHandler1)
      println(s"v4 = {$v4}")
      assert (v4 == 35.0, "Test 4 failed")

      passed(10)
    }

    {
      def continue1(v: Value): Double= v match {
        case NumValue(d) => d
        case BoolValue(b) => if (b) 1.0 else -1.0
        case Closure(_,_,_) => 1000.111
      }
      def continue2(v: Value): Int = v match {
        case NumValue(_) => 1
        case BoolValue(_) => 2
        case Closure(_,_,_) => 3
      }

      def errorHandler1() = {
        println("Successfully caught error in computation.")
        "error"
      }
      val x = Ident("x")
      val y = Ident("y")

      /*
      try
         x + 10
      catch
        25 + y
      */
      val e = TryCatch(Plus("x", 10), Plus(25, "y"))
      val v = evalErr_k(e, EmptyEnvironment, continue1, errorHandler1)
      println(s"v = $v")
      assert(v == "error", "Test 1 Failed")


      /*
        try
            try
               x + 10
            catch
              25 + y
         catch
            15
      */

      val e2 = TryCatch(e, 15.0)
      val v2 = evalErr_k(e2, EmptyEnvironment, continue1, errorHandler1)
      println(s"v2 = $v2")
      assert(v2 == 15.0, "Test 2 Failed")


      /*
        try
            try
               x + 10
            catch
              (
               try 25 + y catch -5
              )
         catch
            15
      */

      val e3 = TryCatch( TryCatch(
        Plus("x", 10),
        TryCatch(Plus(25, "y"), -5))
        , 15)
      val v3 = evalErr_k(e3, EmptyEnvironment, continue1, errorHandler1)
      println(s"v3 = $v3")
      assert(v3 == -5.0, "Test 3 Failed")


      passed(5)
    }

  }
}
