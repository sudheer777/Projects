package expression
import value._
import context._

// Implement ...
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = ???
}
