package value
import context.Environment
import expression._

case class Chars(val value:String) extends Addable with Ordered[Chars] {
  def subChars(to:Exact, from:Exact):Chars = Chars(this.value.substring(to.value, from.value))
  def size: Exact = Exact(value.length)
  override def +(other: Value): Addable = Chars(this.value + other.toString)
  override def compareTo(that: Chars): Int = super.compareTo(that)
  override def hashCode(): Int = super.hashCode()
  override def toString: String = value.toString
  override def equals(other: Any): Boolean =
    other match {
      case other: Chars => this.canEqual(other) && (other.value == this.value)
      case _ => false
    }
  override def compare(that: Chars): Int ={
    if (this.value < that.value) -1 else if (that.value < value) 1 else 0
  }
}