package expression
import context.Environment
import value.Value

trait Expression {
  def execute(env: Environment): Value
}