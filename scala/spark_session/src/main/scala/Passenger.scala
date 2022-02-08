import org.apache.spark.sql.functions.udf
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}

object Passenger {

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    val passengerDf = sqlContext.read.option("header", "true").csv("passengers.csv") // |passengerId|firstName|lastName
    passengerDf.show()
    val flightDataDf = sqlContext.read.option("header", "true").csv("flightData.csv")  // |passengerId|flightId|from| to|      date|
    flightDataDf.show()


    // problem1
    val flightMonthDf = computeMonth(flightDataDf).cache()
    flightMonthDf.show()
    val res1 = flightMonthDf.select("month", "flightId").distinct()
    res1.groupBy("month").count().show() // similar thing with spark

    flightDataDf.registerTempTable("flight_data")
    passengerDf.registerTempTable("passengers")
    sqlContext.sql("select * from (select passengerId, count(*) cnt from flight_data group by passengerId order by cnt desc limit 100) A join passengers ON A.passengerId = passengers.passengerId").show(100, false)


  }

  def computeMonth(df: DataFrame): DataFrame = {
    val cmonth = (date: String) => {
      val d = date.split("/")
      d(1).toInt
    }
    val cmudf = udf(cmonth)
    df.withColumn("month", cmudf(df("date")))
  }
}
