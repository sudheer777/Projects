package aryanb1239

case class Numbers(x: Int, y: Int) {
  val MULTIPLIER = 1

  def add(): Int = {
    x + y
  }

  def multiply(a: Int): Int = {
    MULTIPLIER * a
  }

  def subtract(b: Int, c: Int): Int = {
    b - c
  }

  def value(): (Int, Int) = {
    (x, y)
  }
}

object Q5 {
  def main(args: Array[String]): Unit = {
    val n = Numbers(1, 2)
    println(n.add())
    println(n.multiply(4))
    println(n.subtract(10, 9))
    println(n.value())
  }
}
