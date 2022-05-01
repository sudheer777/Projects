package context

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
      }

      def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
        case "if"~"("~cond~")"~cons~None => Conditional(cond, cons, null)
        case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
      }

      def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
        case con ~ Nil => con
        case con ~ more => Disjunction(con::more)
      }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case e ~ Nil => e
    case e ~ list => Conjunction(e :: list)
  }

  // equality ::= inequality ~ ("==" ~ inequality)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case e ~ Nil => e
    case e ~ list => FunCall(Identifier("equals"), e :: list)
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case thisExp ~ None => thisExp
    case thisExp ~ Some("<" ~ that) => FunCall(Identifier("less"), List[Expression](thisExp, that))
    case thisExp ~ Some(">" ~ that) => FunCall(Identifier("more"), List[Expression](thisExp, that))
    case thisExp ~ Some("!=" ~ that) => FunCall(Identifier("unequals"), List[Expression](thisExp, that))
  }

  // sum ::= product ~ ("+" | "-") ~ product)*
     def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
       case p ~ more => parseSums(p, more)
     }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case (t ~ blah) => parseProduct(t, blah)
  }

  // generates left-to-right calls to mul and div:
  private  def parseProduct(t: Expression, terms: List[~[String, Expression]]): Expression = {
    terms match {
      case Nil => t
      case ~("*", t1)::more => parseProduct(FunCall(Identifier("mul"), List(t, t1)), more)
      case ~("/", t1)::more => parseProduct(FunCall(Identifier("div"), List(t, t1)), more)
    }
  }


  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

      def literal = boole | inexact | exact | chars | identifier


  // chars ::= any characters bracketed by quotes
    def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
      case characters => Chars(characters.substring(1, characters.length - 1))
    }

  // exact ::= """0|(\+|-)?[1-9][0-9]*"""
  def exact: Parser[Exact] = """0|(\+|-)?[1-9][0-9]*""".r ^^ {
    case characters => Exact(characters.toInt)
  }

  // inexact ::= """(\+|-)?[0-9]+\.[0-9]+"""
  def inexact: Parser[Inexact] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
    case characters => Inexact(characters.toDouble)
  }

  // boole ::= true|false
  def boole: Parser[Boole] = """true|false""".r ^^ {
    case characters => Boole(characters.toBoolean)
  }

  // identifier ::= "[a-zA-Z][a-zA-Z0-9]*"
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case identifiers => Identifier(identifiers)
  }

  // funCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case (op ~ ops) => FunCall(op, ops)
  }

  //operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~> expression)) ~ ")" ^^ {
    case "(" ~ Some(expression ~ more) ~ ")" => expression :: more
    case "(" ~ None ~ ")" => Nil
  }

}
