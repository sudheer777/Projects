import context._
import value._
import expression._

@main def testNumbers() =

  val life = Exact(42)
  val two = Exact(2)
  val pi = Inexact(3.14)
  val e = Inexact(2.718)
  val twotoo = Inexact(2.0)

  println(two * life) // 84
  println(-life) // -42
  println(e * pi) // 8.53452
  println(pi * two) // 6.28
  println(two * pi) // 6.28
  println(two < life) // true
  println(e < two) // false
  println(two == twotoo) // true

@main def testBooles() =

  println(!Boole.TRUE || Boole.FALSE || Boole.TRUE) // true
  println(!(Boole.FALSE || Boole.TRUE) || Boole.TRUE) // true
  println(Boole.TRUE && !(Boole.FALSE && Boole.TRUE)) // true

@main def testChars() =
  val mars = Chars("Mars")
  val jupiter = Chars("Jupiter")
  val hello = Chars("Hello, ")
  val result = Chars("result = ")
  val two = Exact(2)

  println(hello + mars) // Hello, Mars
  println(hello + jupiter) // Hello, Jupiter
  println(result + Exact(42)) // result = 42
  println(jupiter.subChars(two, Exact(5))) // pit
  println(jupiter < mars) // true
  println(Chars("Mars") == mars) // true

@main def aluTests() =
  println(alu.execute(Identifier("add"), List(Exact(3), Exact(9), Inexact(2.0)))) // 14.0
  println(alu.execute(Identifier("mul"), List(Exact(3), Exact(9), Inexact(2.0)))) // 54.0
  println(alu.execute(Identifier("sub"), List(Exact(3), Exact(9), Inexact(2.0)))) // -8.0
  println(alu.execute(Identifier("div"), List(Inexact(32.0), Exact(2), Exact(2)))) // 8.0
  println(alu.execute(Identifier("add"), List(Chars("result = "), Exact(42)))) // result = 42
  println(alu.execute(Identifier("less"), List(Chars("cat"), Chars("bat")))) // false



