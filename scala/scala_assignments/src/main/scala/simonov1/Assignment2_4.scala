package simonov1

object Assignment2_4 {

  sealed abstract class ExprExt
  case class TrueExt() extends ExprExt
  case class FalseExt() extends ExprExt
  case class NumExt(num: Int) extends ExprExt
  case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt
  case class UnOpExt(s: String, e: ExprExt) extends ExprExt
  case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt
  case class ListExt(l: List[ExprExt]) extends ExprExt
  case class NilExt() extends ExprExt
  case class CondExt(cs: List[(ExprExt, ExprExt)]) extends ExprExt
  case class CondEExt(cs: List[(ExprExt, ExprExt)], e: ExprExt) extends ExprExt

  object ExprExt {
    val binOps = Set("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons")
    val unOps = Set("-", "not", "head", "tail", "is-nil", "is-list")
  }

  sealed abstract class Value
  case class NumV(v: Int) extends Value
  case class BoolV(v: Boolean) extends Value
  case class NilV() extends Value
  case class ConsV(head: Value, tail: Value) extends Value

  abstract class ParseException(msg: String = null) extends Exception
  abstract class InterpException(msg: String = null) extends Exception

  case class NotImplementedException(s: String) extends RuntimeException(s)
  class ParserException(s:String)  extends ParseException(s)
  class InterpreterException(s:String)  extends InterpException(s)

  object Parser {
    def parse(str: String): ExprExt = parse(Reader.read(str))

    def parse(sexpr: SExpr): ExprExt = sexpr match {
      case SNum(n) => NumExt(n)
      case SSym(s) => s match
      {
        case "true" => TrueExt()
        case "false" => FalseExt()
        case "list" => NilExt()
        case "nil" => NilExt()
        case _ => throw new ParserException("Mitakaebog")
      }
      case SList(list) => list match {
        case SSym("+")::l::r::Nil        => BinOpExt ("+", parse(l), parse(r) )
        case SSym("-")::l::r::Nil        => BinOpExt ("-", parse(l), parse(r) )
        case SSym("*")::l::r::Nil        => BinOpExt ("*", parse(l), parse(r) )
        case SSym("num<")::l::r::Nil     => BinOpExt ("num<", parse(l), parse(r) )
        case SSym("num>")::l::r::Nil     => BinOpExt ("num>", parse(l), parse(r) )
        case SSym("num=")::l::r::Nil     => BinOpExt ("num=", parse(l), parse(r) )
        case SSym("cons")::l::r::Nil     => BinOpExt ("cons", parse(l), parse(r) )
        case SSym("or")::l::r::Nil       => BinOpExt ("or", parse(l), parse(r) )
        case SSym("and")::l::r::Nil      => BinOpExt ("and", parse(l), parse(r) )
        case SSym("not")::l::Nil         => UnOpExt  ("not", parse(l) )
        case SSym("head")::l::Nil        => UnOpExt  ("head", parse(l) )
        case SSym("tail")::l::Nil        => UnOpExt  ("tail", parse(l) )
        case SSym("is-nil")::l::Nil      => UnOpExt  ("is-nil", parse(l) )
        case SSym("is-list")::l::Nil     => UnOpExt  ("is-list", parse(l) )
        case SSym("-")::l::Nil           => UnOpExt  ("-", parse(l) )
        case SSym("if")::c::t::f::Nil    => IfExt    (parse(c), parse(t), parse(f))
        case SSym("cond")::list1         => list1 match {
          case rest :+ lastElement => {
            (rest,lastElement) match {
              case (Nil, SList(SSym("else")::ans::Nil)) => throw new ParserException("begins with Nil")
              case (_, SList(SSym("else")::ans::Nil))  => CondEExt(helperfunc(rest), parse(ans))
              case (_, SList(_::_::Nil)) => CondExt(helperfunc(list1))
              case (_, _) => throw new ParserException("doesn't match required syntax for cond")
            }
          }
        }
        case SSym("list")::list => ListExt( list.map(x => parse(x)))
        case _ => throw new ParserException("doesn't match required syntax")

      }
    }
    def helperfunc(list: List[SExpr]):  List[(ExprExt, ExprExt)] = {
      list.map( x =>  x match
      {
        case SList(left::right::Nil) => (parse(left), parse(right))
        case _ => throw new ParserException("Mitakaebog")
      }  )
    }
  }

  object Interp {
    def interp(e: ExprExt): Value = e match {
      case TrueExt()         => BoolV(true)
      case FalseExt()        => BoolV(false)
      case NumExt(n)         => NumV(n)
      case NilExt()          => NilV()
      case BinOpExt(s, l, r) =>
        s match {
          case "+" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => NumV(l1+r1)
              case _ => throw new InterpreterException("Invalid arguments for + operation")
            }
          case "-" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => NumV(l1-r1)
              case _ => throw new InterpreterException("Invalid arguments for - operation")
            }
          case "*" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => NumV(l1*r1)
              case _ => throw new InterpreterException("Invalid arguments for * operation")
            }
          case "num<" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => BoolV(l1 < r1)
              case _ => throw new InterpreterException("Invalid arguments for num< operation")
            }
          case "num>" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => BoolV(l1 > r1)
              case _ => throw new InterpreterException("Invalid arguments for num> operation")
            }
          case "num=" =>
            (interp(l), interp(r)) match {
              case (NumV(l1), NumV(r1)) => BoolV(l1 == r1)
              case _ => throw new InterpreterException("Invalid arguments for num= operation")
            }
          case "cons" =>
            (interp(l), interp(r)) match {
              case (h, t) => ConsV(h, t)
              case _ => throw new InterpreterException("Invalid arguments for cons operation")
            }
          case "or" =>
            interp(l) match {
              case BoolV(x) => if (x) BoolV(true) else {
                interp(r)
              }
              case _ => throw new InterpreterException("Invalid arguments for or operation")
            }
          case "and" =>
            interp(l) match {
              case BoolV(x) => if (!x) BoolV(false) else {
                interp(r)
              }
              case _ => throw new InterpreterException("Invalid arguments for or operation")
            }
        }
      case UnOpExt(s, l) => s match {
        case "-" => interp(l) match {
          case NumV(k) => NumV(-1 * k)
          case _ => throw new InterpreterException("Invalid arguments for - operation")
        }
        case "not" => interp(l) match {
          case BoolV(k) => BoolV(!k)
          case _ => throw new InterpreterException("Invalid arguments for not operation")
        }
        case "head" => interp(l) match {
          case ConsV(h, tail) => h
          case _ => throw new InterpreterException("Invalid arguments for head operation")
        }
        case "tail" => interp(l) match {
          case ConsV(h, tail) => tail
          case _ => throw new InterpreterException("Invalid arguments for not operation")
        }
        case "is-nil" => interp(l) match {
          case NilV() => BoolV(true)
          case ConsV(_,_) => BoolV(false)
          case _ => throw new InterpreterException("Invalid arguments for not operation")
        }
        case "is-list" => interp(l) match {
          case ConsV(h, t) => BoolV(true)
          case NilV() => BoolV(true)
          case _ => BoolV(false)
        }
      }
      case IfExt(c, t, e) =>
        interp(c) match {
          case BoolV(l) =>
            if (l) interp(t) else interp(e)
          case _ => throw new InterpreterException("Invalid arguments for if operation")
        }
      case ListExt(l) => l match {
        case Nil => NilV()
        case h :: t =>  ConsV(interp(h), interp(ListExt(t)))
      }
      case CondExt(l) => l match {
        case Nil => throw new InterpreterException("something else")
        case (h1, h2) :: t => interp(h1) match {
          case BoolV(true) => interp(h2)
          case _ => interp(CondExt(t))
        }
      }
      case CondEExt(l, e) => l match {
        case Nil => interp(e)
        case (h1, h2) :: t => interp(h1) match {
          case BoolV(true) => interp(h2)
          case _ => interp(CondEExt(t, e))
        }
      }
      case _ => throw new InterpreterException("something else")

    }
  }

}
