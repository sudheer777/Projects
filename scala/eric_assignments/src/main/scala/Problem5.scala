
trait Instruction:
  def execute(register: Double): Double

class Add(val arg: Double) extends Instruction:
  override def execute(register: Double): Double = register + arg

class Mul(val arg: Double) extends Instruction:
  override def execute(register: Double): Double = register * arg

class Rep(val num: Int, val ins: Instruction) extends Instruction:
  override def execute(register: Double): Double = {
    var re = register
    for (i <- 0 until num) {
      re = ins.execute(re)
    }
    re
  }

class Blt(val n: Int, val m: Int) extends Instruction:
  override def execute(register: Double): Double = register

class Halt() extends Instruction:
  def execute(register: Double): Double = 0

object Accumulator:
  var program: List[Instruction] = _

  var register: Double = 0

  def run(): Unit = {
    register = 0
    var i = 0
    var isHalt: Boolean = false
    while (i < program.length && !isHalt) {
      program(i) match {
        case h: Halt => isHalt = true
        case b: Blt =>
          if (register < b.n) {
            i = i + b.m - 1
          }
        case k: Instruction =>
          register = k.execute(register)
      }
      i += 1
    }
  }

object testAccumulator extends App {
  // Problem 1
  // computing 3 * 4 + 9
  Accumulator.program = List(Add(3), Mul(4), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing (((10 * 2) + 3) * 5)
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println("register = " + Accumulator.register)



  // Problem 2

  Accumulator.program = List(Add(3), Mul(4), Halt(), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 12


  // Problem 3
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 42


  // Problem 4

  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Blt(33, 2), Add(10), Halt(), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 32
}