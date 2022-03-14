enum LetterGrade:
  case A, B, C, D, F

class Assignment(val name: String, val assignmentNumber: Int, var _grade: Int):
  if(_grade < 0 || _grade > 100) throw new Exception("Invalid grade")

  def grade = _grade
  def grade_=(gr: Int) =
    if(gr < 0 || gr > 100) throw new Exception("Invalid grade")
    _grade = gr

  def letterGrade: LetterGrade = grade match {
    case x if x>= 90 => LetterGrade.A
    case x if x>= 80 => LetterGrade.B
    case x if x>= 70 => LetterGrade.C
    case x if x>= 60 => LetterGrade.D
    case _ => LetterGrade.F
  }

  override def toString: String = s"$name assn $assignmentNumber: $grade (= $letterGrade)"


object testAssignment extends App:

  try
    val simpson = Assignment("Simpson", 1, -88)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

  val jones = Assignment("Jones", 1, 88)
  val hanson = Assignment("Hanson", 1, 95)

  println(jones.grade) // 88
  println(jones.letterGrade) // B
  jones.grade = jones.grade + 10
  println(jones.grade) //98
  println(jones.letterGrade) // A
  println(jones) // Jones assn 1: 98 (= A)

  println(hanson.grade) // 95
  println(hanson.letterGrade) // A
  try
    hanson.grade = hanson.grade + 10
  catch
    case e: Exception => println(e.getMessage) // Invalid grade
  finally
    println(hanson) // Hanson assn 1: 95 (= A)
  try
    val smith = Assignment("Smith", 1, -10)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade


// ++++++++++++++++++++++++
// Implementing static variables & methods
// ++++++++++++++++++++++++

var globalId: Int = 500
class Transaction(val fromAccount: Int, val toAccount: Int, val amount: Double):
  if(amount<=0) throw new Exception("Invalid amount")
  val id = globalId
  globalId += 1

  override def toString: String = s"Transaction #$id: $$$amount from acct $fromAccount to acct $toAccount"

object testTransactions extends App:
  try
    val t1 = Transaction(119, 212, -20.50)
  catch
    case e: Exception => println(e.getMessage) // Invalid amount
  val ledger = List(
    Transaction(119, 212, 600.50),
    Transaction(212, 119, 1200),
    Transaction(212, 119, 98.75)
  )
  ledger.foreach(println) // how to create unique IDs
/*
Transaction #500: $600.5 from acct 119 to acct 212
Transaction #501: $1200.0 from acct 212 to acct 119
Transaction #502: $98.75 from acct 212 to acct 119
*/

// ++++++++++++++++++++++++
// Implementing a value class
// ++++++++++++++++++++++++

class Time(val hour: Int, val minute: Int = 0) extends Ordered[Time]:
  if(hour < 0 || hour > 23) throw new Exception("Invalid hour")
  if(minute < 0 || minute > 59) throw new Exception("Invalid minute")

  def this(t: String) = {
    this(t.split(":")(0).toInt, t.split(":")(1).toInt)
  }

  override def toString: String = {
    val m = if (minute < 10) s"0$minute" else minute.toString
    s"$hour:$m"
  }

  override def compare(that: Time): Int = {
    if (this.hour > that.hour) 1
    else if (this.hour < that.hour) -1
    else {
      if (this.minute > that.minute) 1
      else if (this.minute < that.minute) -1
      else 0
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case k: Time => hour == k.hour && minute == k.minute
    case _ => false
  }

  override def hashCode(): Int = {
    31 * (
      hour.##
      ) + minute.##
  }

  def +(that: Time): Time = {
    var newMin = this.minute + that.minute
    val overHeadHr = if (newMin < 60) 0 else {
      newMin = newMin - 60
      1
    }
    var newHour = this.hour + that.hour + overHeadHr
    if (newHour > 23) {
      newHour = newHour - 24
    }
    Time(newHour, newMin)
  }


class PreciseTime (hour: Int, minute: Int = 0, val second: Int = 0) extends Time(hour, minute):
  if(second < 0 || second > 59) throw new Exception("Invalid second")
  override def toString: String = {
    val s = if (second < 10) s"0$second" else second.toString
    s"${super.toString}:$s"
  }

  override def hashCode(): Int = {
    31 * (
      super.hashCode()
      ) + second.##
  }

  override def equals(obj: Any): Boolean = obj match {
    case k: PreciseTime => hour == k.hour && minute == k.minute && second == k.second
    case _ => false
  }


object testTime extends App:
  try
    val t = Time(24, 50)
  catch
    case e: Exception => println(e.getMessage) // Invalid hour
  try
    val t = Time(12, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid minute

  val t1 = Time(10, 30)
  val t2 = Time(15, 45)
  val t3 = Time(10, 30)
  val t4 = Time("18:45")
  val t5 = Time(17)
  val t6 = t1 + t2
  println(t1) // 10:30
  println(t2) // 15:45
  println(t3) // 10:30
  println(t4) // 18:45
  println(t5) // 17:00
  println(t6) // 2:15
  println(t1 == t3) // true
  println(t1 != t5) // true
  println(t1 < t2)  // true
  println(t4 < t2)  // false
  println(t1 <= t3) // true

  val schedule = Map(
    t1 -> "coffee break",
    t2 -> "nap",
    t4 -> "cocktail hour"
  )
  println(schedule) // Map(10:30 -> coffee break, 15:30 -> nap, 18:45 -> cocktail hour)
  println(schedule(t3)) // coffee break

  try
    val pt = PreciseTime(12, 0, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid second

  val pt2 = PreciseTime(18)
  val pt1 = PreciseTime(10, 30)
  println(pt1) // 10:30:00
  println(pt2) // 18:00:00
  println(t1) // 10:30
  println(pt1 == t1) // false
  try
    println(schedule(pt1))
  catch
    case e: Exception => println(e.getMessage) // key not found: 10:30:00
