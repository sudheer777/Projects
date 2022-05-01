package aryanb1239

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

object Q3 {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    val gradeFile = "src/main/scala/aryanb1239/grade_score.csv"

    // import and view top 10 rows
    val df = sqlContext.read.options(Map("header" -> "true", "inferSchema" -> "true")).csv(gradeFile)
    df.show(10, false)

    df.registerTempTable("grade")
    // print exam1 and exam2 who passed final exam
    sqlContext.sql("select Exam1, Exam2 from grade where `Pass/fail` = 'P'").show(1000)

    // print grade wise summary containing grades and correcsponding count of students
    sqlContext.sql("select Grade, count(*) student_count from grade where Grade is not null group by Grade order by Grade desc").show(1000)

    // print grades of students who score more than 120 in exam1
    sqlContext.sql("select Grade from grade where Exam1 > 120").show(1000)

    // print descriptive statistics on final_score3 column
    sqlContext.sql("select Final_score, count(*) student_count from grade where Final_score is not null group by Final_score order by Final_score desc").show(1000)

    // Print number of rows and columns for dataframe
    println(s"Number of rows: ${df.count()}")
    println(s"Number of columns: ${df.columns.length}")

    val validRows = sqlContext.sql("select * from grade where Exam1 is not null")
    println(s"Number of valid rows: ${validRows.count()}")
    println(s"Number of valid columns: 7")
  }
}
