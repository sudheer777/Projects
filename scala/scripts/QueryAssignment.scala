package imdb

object Assignment {

  case class Student(name: String, course: String, grade: Double) {
    def print() {
      println(s"$name, $course, $grade")
    }
  }


  val myClass = List(
    Student("Jack", "English", 80),
    Student("Jack", "Math", 89),
    Student("Jack", "Biology", 85),
    Student("Jill", "English", 90),
    Student("Jill", "Math", 92),
    Student("Jill", "Biology", 87),
    Student("Joe", "English", 75),
    Student("Joe", "Math", 95),
    Student("Joe", "English", 77)
  )


  class Query(val data: List[Student]) {
    def select(entry: (String, String)): Query = {
      entry._1 match {
        case "name" => new Query(data.filter(_.name == entry._2))
        case "course" => new Query(data.filter(_.course == entry._2))
        case _ => new Query(List())
      }
    }

    def showData(): Unit = {
      data.foreach(_.print())
    }

    def average(): Double = {
      val sum = data.map(_.grade).sum
      val tot = data.length
      sum / tot
    }

    def showAverage() {
      val av = (average() * 10).round / 10.0
      println(s"Average = $av")
    }

  }

}
