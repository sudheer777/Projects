package aryanb1239

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

object Q2A1 {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    val telecomFile = "src/main/scala/aryanb1239/telecom.csv"
    val df = sqlContext.read.options(Map("header" -> "true", "inferSchema" -> "true")).csv(telecomFile)

    // view first 20 record
    df.show(20, false)

    // show schema of dataset
    df.printSchema()

    df.registerTempTable("telecom")

    // customers who do more than 150 calls in average per week
    sqlContext.sql("select CustID from (select CustID, avg(Calls) avg_calls from telecom group by CustID) where avg_calls > 150").show()

    // print average amount spent by customers week wise
    sqlContext.sql("select Week, avg(Amt) from telecom group by Week order by Week").show(30, false)

    // maximum number of minutes called week wise
    sqlContext.sql("select Week, max(Calls) from telecom group by Week order by Week").show(30, false)

    // import text file to RDD
    val rdd = sqlContext.sparkContext.textFile(telecomFile, 4)

    // print number of partitions
    println(s"number of partitions: ${rdd.getNumPartitions}")

    // print data in first 2 partitions
    val d = rdd.glom().collect()
    val p1 = d(0)
    val p2 = d(1)
    println("first partition data:")
    p1.foreach(x => println(x))
    println("second partition data:")
    p2.foreach(x => println(x))

    // total number of records and the length of the row
    val rowCount = rdd.map(x => 1).reduce(_ + _)
    println(s"row count: $rowCount")
    val rowLengthTotal = rdd.map(_.length).sum()
    println(s"Row length sum: $rowLengthTotal")



  }
}
