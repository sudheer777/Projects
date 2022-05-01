package expression
import value._
import context._

// Implement ...
case class Disjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = ???
}
