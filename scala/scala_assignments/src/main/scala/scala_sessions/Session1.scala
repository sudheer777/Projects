package scala_sessions

import scala.collection.mutable
import scala.io.Source

object Session1 {
  /**
   * PROBLEM STATEMENT:
   *
   * we have a CSV file with 4 columns, name, age, gender, salary
   * we have to find out average salary for each gender and age group (divided into 4 groups, [0-25], [26-40], [41-59], [60+])
   *
   * input file sample:
   * name,age,gender,salary
   * t1,20,male,100
   * t4,21,male,110
   * t2,34,female,200
   *
   *
   * output should look like:
   * male, [0-25], 120
   * female, [0-25], 130
   * male, [26-40], 190
   * female, [26-40], 170
   * male, [41-59], 220
   * female, [41-59], 200
   * male, [60+], 120
   * female, [60+], 100
   *
   *
   * Additional exersice to solve
   *
   * input file will look like:
   * 3 event types are possible: imp, cnv,clk
   * date is of format yyyy-MM-dd
   * event_type,date,count
   * imp,2021-10-12,120
   * imp,2021-10-10,100
   * cnv,2021-10-12,100
   * clk,2021-10-12,20
   * imp,2022-01-20,300
   * cnv,2022-01-20,100
   * clk,2022-01-20,199
   *
   * output should be like
   * for each year and event_type, need to print total count
   * 2021,imp,220
   * 2021,cnv,122
   * 2021,clk,100
   * 2022,imp,100
   * ...
   *
   */
  def main(args: Array[String]): Unit = {

    val bufferedSource = Source.fromFile("session1.csv")
    val lines = bufferedSource.getLines
    val header = lines.next()
    println(s"header row: $header")
    // key is tupple of 2 strings, where first string is gender and second one is age group
    // value is tupple of Double and int, first one is total salary for that gender and age group and second one is total population in that gender and age group
    val ageGenderMap = mutable.Map[(String, String), (Double, Int)]()
    for (line <- lines) {
      val lineArray = line.split(",")
      val (name, age, gender, salary) = (lineArray(0), lineArray(1).toInt, lineArray(2), lineArray(3).toDouble)
      val ageGroup = age match {
        case x if x <= 25 => "[0-25]"
        case x if x > 25 && x <= 40 => "[26-40]"
        case x if x > 40 && x <= 59 => "[40-59]"
        case _ => "[60+]"
      }
      if (ageGenderMap.contains((gender, ageGroup))) {
        val existing = ageGenderMap((gender, ageGroup))
        // println(s"Existing: $existing")
        ageGenderMap((gender, ageGroup)) = (existing._1 + salary, existing._2 + 1)
      } else {
        // println("first time for this age group")
        ageGenderMap((gender, ageGroup)) = (salary, 1)
      }
      // println(s"name: $name, age: $age, gender: $gender, salary: $salary, age_group: $ageGroup")
    }
    bufferedSource.close

    ageGenderMap.foreach(x => {
      val key = x._1
      val gender = key._1
      val ageGroup = key._2
      val value = x._2
      val totSalary = value._1
      val totPopulation = value._2
      val avgSalary = totSalary/totPopulation
      println(s"$gender, $ageGroup, $avgSalary")
    })
  }
}

