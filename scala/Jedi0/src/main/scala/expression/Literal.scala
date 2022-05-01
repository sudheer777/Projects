package expression
import context.Environment
import value.*
//import context.*
// When you execute Literal
trait Literal extends Expression with Value{
  def execute(env: Environment): Literal= this
}