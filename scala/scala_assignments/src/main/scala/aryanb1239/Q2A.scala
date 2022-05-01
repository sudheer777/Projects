package aryanb1239

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

object Q2A {
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
    val rdd = sqlContext.sparkContext.textFile(telecomFile, 4).map(x => x.split(","))

    // print all data in partitions seperately
    rdd.glom().collect().foreach(x => {
      println("Start of partition data")
      x.foreach(y => {
        println(y.mkString(","))
      })
    })

    // sort by and flat map
    val rdd1 = rdd.flatMap(x => Array((x(0), x))).sortBy(_._1)

    // add 1 with map and reduce operation
    val count = rdd1.map(x => 1).reduce(_ + _)

    println(count)
  }
}
