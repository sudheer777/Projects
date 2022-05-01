package expression

import context._
import value._

case class Identifier(name: String) extends Expression {
  override def toString: String = name
  override def execute(env: Environment): Value = env(this)
}