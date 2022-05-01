package datrollman

object Assignment8 {
  def passed(points: Int) {
    require(points >=0)
    if (points == 1) print(s"\n*** Tests Passed (1 point) ***\n")
    else print(s"\n*** Tests Passed ($points points) ***\n")
  }

  // Problem 1A

  /*
  Answers
  ???1 = unit
  ???2 = t
  ???3 = ref(t)
  ???4 = typeerror

   */

  // Problem 1B
  /*
  type1 = num
  type2 = num => ref(num)
  type3 = num => num
  type4 = {foo: num => ref(num), bar: num => num}
   */

  // Problem 1C
  sealed trait Type
  case object NumType extends Type
  case object BoolType extends Type
  case class FunType(t1: Type, t2: Type) extends Type
  // TODO: Write new case classes for UnitType, RefType, and RecordType
  //BEGIN SOLUTION
  case object UnitType extends Type
  case class RefType(t: Type) extends Type
  case class RecordType(rt: List[(String, Type)]) extends Type

  //END SOLUTION

  sealed trait Program
  sealed trait Expr

  case class Const(f: Double) extends Expr
  case class Ident(s: String) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Geq(e1: Expr, e2: Expr) extends Expr
  case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class Let(x: String, xType: Type, e1: Expr, e2: Expr) extends Expr
  case class FunDef(id: String, idType: Type, e: Expr) extends Expr
  case class FunCall(calledFun: Expr, argExpr: Expr) extends Expr
  case class NewRef(e: Expr) extends Expr
  case class DeRef(e: Expr) extends Expr
  case class UnitConst() extends Expr
  // TODO: Write new case classes for AssignRef and Record
  //BEGIN SOLUTION
  case class AssignRef(e1: Expr, e2: Expr) extends Expr
  case class Record(r: List[(String, Expr)]) extends Expr

  //END SOLUTION
  case class TopLevel(e: Expr) extends Program

  def typeEquals(t1: Type, t2: Type): Boolean = t1 == t2
  case class TypeErrorException(s: String) extends Exception

  def typeOf(e: Expr, alpha: Map[String, Type]): Type = {
    def checkType(opName: String, e1: Expr, t1: Type, e2: Expr, t2: Type, resType: Type): Type = {
      val t1hat = typeOf(e1, alpha)
      if (! typeEquals(t1hat, t1)){
        throw new TypeErrorException(s"Type mismatch in arithmetic/comparison/bool op $opName, Expected type $t1, obtained $t1hat")
      }

      val t2hat = typeOf(e2, alpha)
      if (! typeEquals(t2hat, t2)){
        throw new TypeErrorException(s"Type mismatch in arithmetic/comparison/bool op $opName, Expected type $t2, obtained $t2hat")
      }

      resType
    }

    e match {
      case Const(f) => NumType
      case Ident(s) => {if (alpha contains s)
        alpha(s)
      else
        throw TypeErrorException(s"Unknown identifier $s")}
      case Plus(e1, e2) =>  checkType("Plus", e1,  NumType, e2, NumType, NumType)
      case Minus(e1, e2) => checkType("Minus",e1,  NumType, e2, NumType, NumType)
      case Geq(e1, e2) => checkType("Geq", e1,  NumType, e2, NumType, BoolType)
      case IfThenElse(e, e1, e2) => {
        val t = typeOf(e, alpha)
        if (t == BoolType){
          val t1 = typeOf(e1, alpha)
          val t2 = typeOf(e2, alpha)
          if (typeEquals(t1, t2))
            t1
          else
            throw TypeErrorException(s"If then else returns unequal types $t1 and $t2")
        } else {
          throw TypeErrorException(s"If then else condition expression not boolean $t")
        }
      }

      case Let(x, t, e1, e2) => {
        val t1 = typeOf(e1, alpha)
        if (typeEquals(t1, t)){
          val newAlpha = alpha + (x -> t)
          typeOf(e2, newAlpha)
        } else {
          throw TypeErrorException(s"Let binding has type $t whereas it is bound to expression of type $t1")
        }
      }

      case FunDef(x, t1, e) => {
        val newAlpha = alpha + (x -> t1)
        val t2 = typeOf(e, newAlpha)
        FunType(t1, t2)
      }

      case FunCall(e1, e2) => {
        val ftype = typeOf(e1, alpha)
        ftype match {
          case FunType(t1, t2) => {
            val argType = typeOf(e2, alpha)
            if (typeEquals(argType, t1)){
              t2
            } else {
              throw TypeErrorException(s"Call to function with incompatible argument type. Expected $t1, obtained $argType")
            }
          }
          case _ => { throw TypeErrorException(s"Call to function but with a non function type $ftype")}

        }
      }

      case NewRef(e) => {
        //BEGIN SOLUTION
        val n = typeOf(e, alpha)
        RefType(n)
        //END SOLUTION
      }

      case AssignRef(e1, e2) => {
        //BEGIN SOLUTION
        val t1 = typeOf(e1, alpha)
        val t2 = typeOf(e2, alpha)
        typeEquals(t1, t2)
        UnitType
        //END SOLUTION
      }

      case DeRef(e) => {
        //BEGIN SOLUTION
        val t = typeOf(e, alpha)
        t match {
          case RefType(t1) => t1
          case _ => throw TypeErrorException(s"Invalid call to deref")
        }
        //END SOLUTION
      }

      case Record(ies) => {
        //BEGIN SOLUTION
        val r = ies.map {
          case (s, e) => {
            val t = typeOf(e, alpha)
            (s, t)
          }
        }
        RecordType(r)
        //END SOLUTION
      }

    }
  }

  def typeOfProgram(p: Program) = p match {
    case TopLevel(e) => {
      val t = typeOf(e, Map())
      println(s"Program type computed successfully as $t")
      t
    }
  }


  // problem 3A
  val allOdd:Stream[Int] = {
    //BEGIN SOLUTION
    Stream.from(1).filter(_%2 == 1)
    //END SOLUTION
  }

  // 3B
  val specialNatNumberStream: Stream[Int] = {
    //BEGIN SOLUTION
    allOdd.filter(x => {
      val s = math.sqrt(x)
      s.toInt == s
    })
    //END SOLUTION
  }


  def main(args: Array[String]): Unit = {
    {
      //BEGIN TEST

      /*
      let x : ref(num) = NewRef(10 ) in
         let dummy: unit = AssignRef(x, 30) in
             DeRef(x)
             */

      val p1 = Let("x", RefType(NumType), NewRef(Const(10)), Let("dummy", UnitType, AssignRef(Ident("x"), Const(30) ), DeRef(Ident("x"))) )
      val t1 = typeOfProgram(TopLevel(p1))
      assert(t1 == NumType, "Test 1 failed: answer should be NumType")
      passed(2)
      //END TEST
    }

    {
      //BEGIN TEST
      /*
      let x : ref(num => num) = NewRef(function(z: num) z + 10) in
         let dummy: unit = AssignRef(NewRef(35), 30) in
             DeRef(x)
             */
      val fdef = FunDef("z", NumType, Plus(Ident("z"), Const(10)))
      val p4 = Let("x", RefType(FunType(NumType, NumType)), NewRef(fdef), Let("dummy", UnitType, AssignRef(NewRef(Const(35)), Const(30) ), DeRef(Ident("x"))) )
      val t4 =  typeOfProgram(TopLevel(p4))
      assert(t4 == FunType(NumType, NumType), "Test failed")
      passed(2)
      //END TEST
    }

    {
      //BEGIN TEST
      /*
      let x : ref(num => num) = NewRef(function(z: num) z + 10) in
         let dummy: num = AssignRef(x, 30) in
             DeRef(x)
             */
      val fdef = FunDef("z", NumType, Plus(Ident("z"), Const(10)))
      val p3 = Let("x", RefType(FunType(NumType, NumType)), NewRef(fdef), Let("dummy", NumType, AssignRef(Ident("x"), Const(30) ), DeRef(Ident("x"))) )
      val t3 = try {
        typeOfProgram(TopLevel(p3))
        assert(false, "The program should not receive a type")
      } catch {
        case TypeErrorException(msg) => s"OK -- caught a type error exception: $msg"
        case e => print(e); assert(false, "Please throw TypeErrorException(message) when a type failure occurs")
      }
      passed(2)
      //END TEST
    }

    {
      //BEGIN TEST

      val p1 = ("foo",Const(10.0))
      val t1 = ("foo",NumType)

      val p2 = ("bar",Geq(Const(9.0), Const(10.0)))
      val t2 = ("bar",BoolType)

      val rdec1 = Record(List(p1,p2))
      val rtyp1 = RecordType(List(t1,t2))

      val prog1 = TopLevel(rdec1)
      assert(typeOfProgram(prog1) == rtyp1, "Test failed")
      passed(2)

      val p3 = ("baz",NewRef(Const(11.0)))
      val t3 = ("baz",RefType(NumType))
      val p4 = ("qux",rdec1)
      val t4 = ("qux", rtyp1)

      val rdec2 = Record(List(p3,p4))
      val rtyp2 = RecordType(List(t3,t4))
      // prog2 = {baz=11.0, qux={foo=10.0, bar=9.0>10.0}}
      val prog2 = TopLevel(rdec2)
      assert(typeOfProgram(prog2) == rtyp2, "Test failed")
      passed(2)


      //END TEST
    }

    {
      //BEGIN TEST
      /*
      let x: ref(Num) = NewRef(0) in
       let f : num => unit = function (z:num) { x := z} in
         let obj: {set_counter:ref(num => unit)} = {set_counter = NewRef(f)} in
           obj
      */
      val fdef = FunDef("z", NumType, AssignRef(Ident("x"), Ident("z")))
      val gdef = FunDef("z", UnitType, DeRef(Ident("x")))
      val recrd = Record(List(("set_counter",NewRef(Ident("f")))))
      val rectyp = RecordType(List(("set_counter",RefType(FunType(NumType,UnitType)))))
      val p3 = Let("x", RefType(NumType), NewRef(Const(0.0)),
        Let("f", FunType(NumType, UnitType), fdef,
          Let("obj",rectyp, recrd,
            Ident("obj"))) )
      val t = typeOfProgram(TopLevel(p3))
      assert(t == rectyp, s"Test failed: answer should be $rectyp")
      passed(2)

      //END TEST
    }

    {
      //BEGIN TEST
      /*
      let x: ref(Num) = NewRef(0) in
       let f : num => unit = function (z:num) { x := z} in
        let g : unit => num = function (z:unit) {DeRef(x)} in
         let obj: {set_counter:num => unit, get_counter:unit=>num} = {set_counter = f, get_counter=g} in
           obj
      */
      val fdef = FunDef("z", NumType, AssignRef(Ident("x"), Ident("z")))
      val gdef = FunDef("z", UnitType, DeRef(Ident("x")))
      val recrd = Record(List(("set_counter",Ident("f")), ("get_counter",Ident("g"))))
      val rectyp = RecordType(List(("set_counter",FunType(NumType,UnitType)),
        ("get_counter",FunType(UnitType,NumType))))
      val p3 = Let("x", RefType(NumType), NewRef(Const(0.0)),
        Let("f", FunType(NumType, UnitType), fdef,
          Let("g", FunType(UnitType, NumType), gdef,
            Let("obj",rectyp, recrd,
              Ident("obj")))) )
      val t = typeOfProgram(TopLevel(p3))
      assert(t == rectyp, s"Test failed: answer should be $rectyp")
      passed(3)

      //END TEST
    }

    {
      //BEGIN TEST
      def testStream(n: Int): Boolean = {
        (allOdd.take(n).toList == List.range(0, 2*n).filter(_ % 2 == 1))
      }
      assert(testStream(5), "Testing first 5 elements of the stream")
      assert(testStream(0), "Testing first 0 elements of the stream")
      assert(testStream(25), "Testing first 25 elements of the stream")
      assert(testStream(55), "Testing first 55 elements of the stream")
      assert(testStream(1303), "Testing first 1303 elements of the stream")
      passed(10)
      //END TEST
    }

    {
      //BEGIN TEST
      def cond(j: Int): Boolean = {
        val sq = math.sqrt(j).toInt
        sq*sq == j
      }

      def testStream(n: Int): Boolean = {
        specialNatNumberStream.take(n).forall( cond)
      }

      assert(testStream(2000), "First 2000 elements test failed")
      passed(10)
      //END TEST
    }
  }
}
