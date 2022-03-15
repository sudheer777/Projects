import scala.collection.mutable

trait Command:
  def execute(): Unit

class Push(val ele: Double) extends Command:
  override def execute(): Unit = {}

class Add() extends Command:
  override def execute(): Unit = {}

class Mul() extends Command:
  override def execute(): Unit = {}

class Top() extends Command:
  override def execute(): Unit = {}

class Pop() extends Command:
  override def execute(): Unit = {}

object StackMachine extends Command:
  var program: List[Command] = _

  val stack = mutable.Stack[Double]()

  override def execute(): Unit = {
    stack.clear()
    program.foreach{
      case p: Push => stack.push(p.ele)
      case _: Pop => stack.pop()
      case _: Top =>
        if (stack.isEmpty) {
          println("empty collection")
        } else {
          println(s"top = ${stack.top}")
        }
      case _: Add =>
        if (stack.nonEmpty) {
          val t1 = stack.pop()
          if (stack.nonEmpty) {
            val t2 = stack.pop()
            stack.push(t1 + t2)
          }
        }
      case _: Mul =>
        if (stack.nonEmpty) {
          val t1 = stack.pop()
          if (stack.nonEmpty) {
            val t2 = stack.pop()
            stack.push(t1 * t2)
          }
        }
    }
  }


object StackMachineDemo extends App {
  StackMachine.program = List(Push(2), Push(3), Push(4), Add(), Mul(), Top())
  StackMachine.execute()
  StackMachine.program = List(Push(2), Push(3), Add(), Mul(), Top())
  StackMachine.execute()
}