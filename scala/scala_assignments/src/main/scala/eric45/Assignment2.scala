package eric45

object Assignment2 {

  // Problem 1
  // pipeline
  def spellCheck(doc: String, dict: List[String]): Int = {
    val words = doc
      .split("\\s+")
      .filter(word => word.matches("[a-zA-Z]+"))
      .map(_.toLowerCase)
    words.count(word => !dict.contains(word))
  }

  // recursive
  def spellCheckRecursion(doc: String, dict: List[String]): Int = {
    if (doc.isEmpty) return 0
    val firstWord = doc.takeWhile(_ != ' ').toLowerCase()
    val restSentence = if (firstWord == doc) "" else doc.substring(firstWord.length+1)
    val l = if (firstWord.matches("[a-zA-Z]+")) {
      if (dict.contains(firstWord)) 0 else 1
    } else {
      0
    }
    l + spellCheckRecursion(restSentence, dict)
  }

  // Problem 2
  class Transaction(val amt: Double, val fromAcct: Int, val toAcct: Int)

  def balance(acct: Int, ledger: List[Transaction]): Double = {
    val sendMoney = ledger
      .filter(x => x.fromAcct == acct)
      .map(_.amt)
      .sum
    val recMoney = ledger
      .filter(x => x.toAcct == acct)
      .map(_.amt)
      .sum
    recMoney - sendMoney
  }

  def main(args: Array[String]): Unit = {
    val oed = List("dog", "cat", "bat", "bug", "fox", "see", "run", "bite", "the", "a", "and")
    val essay = "See the blue dog run . See the blue dog bite the man ."
    println(spellCheck(essay, oed)) // = 3
    println(spellCheckRecursion(essay, oed))

    val ledger = List(
      new Transaction(100.0, 1, 2),
      new Transaction(105.0, 1, 3),
      new Transaction(200.0, 2, 3),
      new Transaction(180.0, 3, 4)
    )
    println(balance(3, ledger))


  }
}
