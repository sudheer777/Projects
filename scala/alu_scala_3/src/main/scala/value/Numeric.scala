package value

trait Numeric extends Addable {
  def *(other: Value): Numeric
  def -(other: Value): Numeric
  def /(other: Value): Numeric
  def unary_ : Numeric
}
