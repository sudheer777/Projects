package aryanb1239

object Q10 {

  def isArmstrongNumber(n: Int): Boolean = {
    val digits = n.toString.length
    var sum = 0
    def armstrong(num: Int): Int = {
      if (num != 0) {
        sum += math.pow(num%10, digits).toInt
        armstrong(num/10)
      }
      sum
    }
    armstrong(n) == n
  }

  def main(args: Array[String]): Unit = {
    println(isArmstrongNumber(1634))
    println(isArmstrongNumber(1635))
  }

}
