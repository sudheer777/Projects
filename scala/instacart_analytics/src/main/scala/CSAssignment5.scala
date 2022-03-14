import scala.annotation._

object CSAssignment5 {
  // Problem A
  def elementWiseDiff(lstA: List[Int], lstB: List[Int]): List[Int] = {
    require(lstA.length == lstB.length)
    lstA.zip(lstB).map(x => x._1 - x._2)
  }

  // Problem B
  @tailrec
  private def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)


  def gcdOfDenominators(lst: List[Int]): Int = {
    val l = lst.filter(x => x != 0 && x != 1)
    val h = l.head
    val t = l.tail
    l.foldRight(h)((x, y) => gcd(x, y))
  }

  // Problem C
  def listElementsLessThanIndex(lst: List[Int]): List[Int] = {
    val lstInd = lst.zipWithIndex
    lstInd.filter(x => x._1 < x._2).map(x => x._1)
  }

  // Problem D
  def allIndicesDivisibleBy3(lst: List[Int]): List[Int] = {
    val lstInd = lst.zipWithIndex
    lstInd.filter(x => x._1 % 3 == 0).map(x => x._2)
  }

  // Problem 2E
  def convertListToListTuples(lst: List[String]): List[(String, Int, String, Int)] = {
    val lstInd = lst.zipWithIndex
    lstInd.map(x => {
      val (inp, ind) = x
      (inp, inp.length, inp.reverse, ind)
    })
  }

  // Problem F
  def makeIndexedList(lst:List[String]): List[(String, Int)] = {
    val emptyList = List[(String, Int)]()
    lst.foldLeft(emptyList)((x, y) => {
      x ++ List((y, x.length + 1))
    })
  }

  // Problem G
  def oddEvenPartition(lst: List[Int]): (List[Int], List[Int]) = {
    val emptyList = (List[Int](), List[Int]())
    lst.foldLeft(emptyList)((x, y) => {
      if (y % 2 == 0) {
        (x._1 ++ List(y), x._2)
      } else {
        (x._1, x._2 ++ List(y))
      }
    })
  }

  def main(args: Array[String]): Unit = {
    println(elementWiseDiff(List(1,2,3), List(4,5,6)))
    println(elementWiseDiff(List(1,4,0), List(2,1,5)))
    println(elementWiseDiff(List(), List()))
    println(elementWiseDiff(List(1, 0, 3, -1, -2), List(0, 4, 1, 0, -3)))

    println(gcdOfDenominators(List(1, 1, 1, 0, 1, 1, 0, 5)))
    println(gcdOfDenominators(List(1, 2, 0, 4, 6, 8)))
    println(gcdOfDenominators(List(18, 27, 729, 1110)))

    println(listElementsLessThanIndex(List(1,2,2,-2,4,6,8)))
    println(listElementsLessThanIndex(List(-1,-2,-3,-4,-5,-6)))
    println(listElementsLessThanIndex(List(1,2,3,4,5,6,7,8)))
    println(listElementsLessThanIndex(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, -1, -2, -3)))

    println(allIndicesDivisibleBy3(List(1,3,5,7,6,9,12,11)))
    println(allIndicesDivisibleBy3(List(1,5,7,9,11,13,15)))
    println(allIndicesDivisibleBy3(List(1,5,7,11,13)))
    println(allIndicesDivisibleBy3(List(-3,-6,-9,-12,-15,-18,-21,-24)))

    println(convertListToListTuples(List("Que", "Sera", "Sera", "Whatever", "Will", "Be")))
    println(convertListToListTuples(List("All", "The", "World", "Is", "A", "Stage")))
    println(convertListToListTuples(List("Cry", "Havoc!", "And", "Let", "Loose", "The", "Dogs", "Of", "War")))
    println(convertListToListTuples(List("To", "Be", "Or", "Not", "to", "be", "is", "the", "question")))

    println(makeIndexedList(List("hello")))
    println(makeIndexedList(List("hello", "world")))
    println(makeIndexedList(Nil))
    println(makeIndexedList(List("a","b","c","d","e")))

    println(oddEvenPartition(List(1, 2, 8, 7, 5)))
    println(oddEvenPartition(List(23, 18, 20, 15)))
    println(oddEvenPartition(List(14, 12, 18, 20)))
    println(oddEvenPartition(List()))
  }
}
