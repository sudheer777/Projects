package aryanb1239

object Fibonacci {
  def fibonacci(i : Int) : Int = {
    def rec(last : Int, cur: Int, num : Int) : Int = {
      if ( num == 0) cur
      else rec(cur, last + cur, num - 1)
    }

    if (i < 0) - 1
    else if (i == 0 || i == 1) 1
    else rec(1,2, i - 2)
  }

  def main(args: Array[String]){
    (0 to 10).foreach( (x : Int) => print(fibonacci(x) + " "))
  }
}