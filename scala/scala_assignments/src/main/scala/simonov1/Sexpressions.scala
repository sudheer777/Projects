package simonov1

sealed abstract class SExpr

// TODO: define the case classes for SExpr
case class SSym() extends SExpr
case class SList() extends SExpr
case class SNum(a: Int) extends SExpr

import scala.util.parsing.combinator._

object Reader extends JavaTokenParsers {

  def read(text: String): SExpr = {
    val result = parseAll(sexpr,text)
    result match {
      case Success(r,_) => r
      case Failure(msg,n) =>
        sys.error(msg+" (input left: \""+n.source.toString.drop(n.offset)+"\")")
      case Error(msg, n) =>
        sys.error(msg+" (input left: \""+n.source.toString.drop(n.offset)+"\")")
    }
  }


  // You are not expected to understand the below syntax. You can safely ignore anything below this line.
  def sexpr  : Parser[SExpr] = (num | symbol | slist)
  def symbol : Parser[SExpr] = not(wholeNumber) ~> "[^()\\s]+".r ^^ {s => SSym()}
  def slist  : Parser[SExpr] = "(" ~> sexpr.+ <~ ")"          ^^ {s => SList()}
  def num    : Parser[SExpr] = wholeNumber                    ^^ {s => SNum(s.toInt)}
}
