package simonov1

// import Library._

// to do: define the other case classes of the ArithC type
sealed abstract class ArithC
case class NumC(num: Int) extends ArithC

case class ParseException(string: String) extends RuntimeException

object Parser {
  def parse(str: String): ArithC = parse(Reader.read(str))

  def parse(sexpr: SExpr): ArithC = {
    // throw ParseException("TODO")
    // to do: define the parse function
    sexpr match {
      case SNum(a) => NumC(a)
    }
  }
}