package simonov1

// import Library._

// to do: define the other case classes of the ArithC type
sealed abstract class ArithC
case class NumC(num: Int) extends ArithC
case class PlusC(l:ArithC,r:ArithC) extends ArithC
case class MultC(l:ArithC,r:ArithC) extends ArithC

case class ParseException(string: String) extends RuntimeException

object Parser {
  def parse(str: String): ArithC = parse(Reader.read(str))

  def parse(sexpr: SExpr): ArithC = {
    // throw ParseException("TODO")
    // to do: define the parse function
    sexpr match {
      case SNum(a) => NumC(a)
      case SList(List(SSym(s), s1, s2)) =>
        s match {
          case "+" => PlusC(parse(s1), parse(s2))
          case "*" => MultC(parse(s1), parse(s2))
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println(parse("(+ 12 (* 6 5))"))
  }
}