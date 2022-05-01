package context

import scala.util.parsing.combinator._
import expression._

class JediException(val gripe: String = "Jedi error ") extends Exception(gripe)
class UndefinedException(val name: Identifier) extends JediException("Undefined identifier: " + name)
class TypeException(override val gripe: String = "Type Error") extends JediException(gripe)
class IllegalValueException(override val gripe: String = "Illegal Value") extends JediException(gripe)
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")

