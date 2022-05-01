package expression
import value._
import context._

// Implement ...
case class Conditional(condition:Expression, consequent:Expression, alternative: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = ???
}
