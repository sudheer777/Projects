trait WTreeInterface {
  def isEmpty: Boolean
  def filter(pred: Token => Boolean): WTree
  def ins(w: Token): WTree
  def contains(s:String): Boolean
  def size: Int
}
