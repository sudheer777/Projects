package value

class Notification(val notice: String) extends Value {
  override def toString: String = super.toString
}
object Notification {
  def apply(msg: String) = new Notification(msg)

  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val UNSPECIFIED = new Notification("UNSPECIFIED")

}