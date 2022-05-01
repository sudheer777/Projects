

// Problem 1A
def loopWhile[T](test: T=>Boolean, update: T=>T): T=>T = {
  def res(t: T): T = {
    var result = t
    while (test(result)) {
      result = update(result)
    }
    result
  }
  res
}

// Problem 1B
def finalPopulation(initialPopulation: Int): Int =
  val newF = loopWhile[Int](_ < 10000, _ * 2)
  newF(initialPopulation)


// Problem 2
def countRoots[T](f: T => Double, inputs: List[T]): Int =
  inputs match {
    case Nil => 0
    case head :: tail =>
      val isRoot = if (math.abs(f(head)) <= 1E-010) 1 else 0
      isRoot + countRoots(f, tail)
  }

// Problem 3
// A flight tracking system maintains a record of each flight scheduled for the current day.
// The record includes the flight number, estimated time of arrival (eta), and status
// (-1 = cancelled, 0 = boarding, 1 = in transit, 2 = arrived). Here's a test driver:
// NOTE: THERE IS A TYPO IN QUESTION. INSTEAD OF 2= arrived, IT WAS GIVEN 3=arrived. CORRECTED IT

enum Status:
  case cancelled, boarding, transit, arrived

class Flight(id: Int, var _eta: (Int, Int), var _status: Int):
  if (_status < -1 || _status > 3) throw new Exception("Invalid status")
  if (_eta._1 < 0 || _eta._1 > 23) throw new Exception("Invalid eta")
  if (_eta._2 < 0 || _eta._2 > 59) throw new Exception("Invalid eta")

  def status: Int = _status
  def status_=(s: Int): Unit =
    if(s < -1 || s > 2) throw new Exception("Invalid status")
    _status = s

  def eta: (Int, Int) = _eta
  def eta_=(e: (Int, Int)): Unit =
    if (e._1 < 0 || e._1 > 23) throw new Exception("Invalid eta")
    if (e._2 < 0 || e._2 > 59) throw new Exception("Invalid eta")
    _eta = e

  private def getStatus: Status = status match {
    case x if x == -1 => Status.cancelled
    case x if x == 0 => Status.boarding
    case x if x == 1 => Status.transit
    case x if x == 2 => Status.arrived
  }

  override def toString: String = s"flight #$id arrives at ${eta._1}:${eta._2} status: ${getStatus}"


object March16_scala3 extends App:
  // Test for Problem 1A
  def test1(a: Int): Boolean = a < 100
  def update1(a: Int): Int = a + 15
  val p1a = loopWhile(test1, update1)
  println(p1a(90))
  println(p1a(100))
  println(p1a(10))

  // Test for problem 1B
  println(finalPopulation(50))

  // Test for problem 2
  def f(x: Double): Double = x*x - 2 * x + 1
  println(countRoots(f, List(1, -1, 2.0, 3.0, 3.4)))

  // Problem 3 tests
  val flight1 = Flight(314, (15, 10), 0)
  println(flight1.toString)
  // flight #314 arrives at 15:10 status: boarding
  flight1.eta = (15, 35)
  flight1.status = 1
  println(flight1.toString)
  // flight #314 arrives at 15:35 status: in transit
  try
    val flight2 = Flight(802, (15, 60), 1)
  catch
    case e: Exception => println(e.getMessage) // Invalid eta


  try
    flight1.eta = (25, 45)
  catch
    case e: Exception => println(e.getMessage) // Invalid eta


  try
    flight1.status= 3
  catch
    case e: Exception => println(e.getMessage) // Invalid status
    