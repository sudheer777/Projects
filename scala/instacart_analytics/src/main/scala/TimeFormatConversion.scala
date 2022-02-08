import org.apache.spark.sql.functions.{col, from_utc_timestamp, to_utc_timestamp, udf, unix_timestamp}
import org.apache.spark.sql.types.TimestampType
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}

import java.text.SimpleDateFormat
import java.util.TimeZone

object TimeFormatConversion {

  def getThreadLocalDateFormat(format: String, timeZone: TimeZone): ThreadLocal[SimpleDateFormat] = {
    new ThreadLocal[SimpleDateFormat]() {
      override def initialValue(): SimpleDateFormat = {
        val fo = new SimpleDateFormat(format)
        fo.setTimeZone(timeZone)
        fo
      }
    }
  }

  val formatUTC = getThreadLocalDateFormat("yyyy-MM-dd HH:mm:ss", TimeZone.getTimeZone("UTC"))
  val formatCLT = getThreadLocalDateFormat("yyyy-MM-dd HH:mm:ss", TimeZone.getTimeZone("America/Santiago"))

  def convertUTCToCLT(input: String): String = {
    val dateFormat = formatUTC.get()
    //println(dateFormat.parse(input))
    val dt = dateFormat.parse(input)
    formatCLT.get().format(dt)
  }

  def method1(df: DataFrame): DataFrame = {
    df.withColumn("date_clt", from_utc_timestamp(unix_timestamp(col("date"), "yyyy-MM-dd HH:mm:ss").cast(TimestampType), "America/Santiago"))
  }

  // convert using Dataframe operations
  def method2(df: DataFrame): DataFrame = {
    val convertTime = (input: String) => convertUTCToCLT(input)
    val convertUDf = udf(convertTime)
    df.withColumn("date_clt", convertUDf(col("date")))
  }

  case class ROW(date: String, name: String)

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)
    import sqlContext.implicits._

    val rows = Array(ROW("2022-01-20 14:12:34", "test1"), ROW("2021-06-20 14:12:34", "test2"), ROW("2021-10-22 14:12:30", "test3"),
      ROW("2021-04-22 16:12:30", "test4"))
    // rows.foreach(x => println(convertUTCToCLT(x.date)))
    val dataframe = sqlContext.sparkContext.parallelize(rows).toDF()
    dataframe.show(false)

    val m1Df = method1(dataframe) // this is just one line. easy to understand
    m1Df.show(false)

    val m2Df = method2(dataframe)
    m2Df.show(false)

    m2Df.registerTempTable("m2_tbl")
    sqlContext.sql("select date, date_clt, name from m2_tbl where name = 'test1' or name = 'test2'").show()
    sqlContext.sql("select * from m2_tbl").show()

    sparkContext.stop()
  }
}
