package simonov1

object RacketVSScala {

  /** 1. Definition of the datatype ArithC */
  sealed abstract class ArithC
  // TODO: implement other types
  case class NumC(num: Int) extends ArithC
  case class PlusC(l:ArithC,r:ArithC) extends ArithC
  case class MultC(l:ArithC,r:ArithC) extends ArithC

  /** 2. Implementing a basic interpreter for ArithC */
  def interp(a: ArithC): Int = a match {
    case NumC(n) => n
    case PlusC(l, r) => interp(l) + interp(r)
    case MultC(l, r) => interp(l) * interp(r)
  }

  def main(args: Array[String]): Unit = {
    println(interp(NumC(4)))
    println(interp(PlusC(NumC(3), NumC(6))))
    println(interp(MultC(NumC(9), NumC(8))))
  }

}