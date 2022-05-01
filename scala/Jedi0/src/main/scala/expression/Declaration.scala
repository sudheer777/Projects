package expression
import value._
import context._

// Implement ...
case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = ???
}
