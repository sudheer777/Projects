package value
import expression.Literal

trait Addable extends Literal{
  def +(other:Value): Addable

}