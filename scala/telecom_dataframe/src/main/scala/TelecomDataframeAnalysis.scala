import org.apache.spark.sql.functions.{col, udf}
import org.apache.spark.sql.{Row, SQLContext}
import org.apache.spark.{SparkConf, SparkContext}

object TelecomDataframeAnalysis {
  case class Record(host: String, var emptyCol: String, time: String, restMethod: String, path: String, protocal: String, statusCode: Int,
                    latency: Long, var url: String, userAgent: String) {
    var date: String = _
  }

  def convertToInt(a: String): Int = {
    try {
      a.toInt
    } catch {
      case _: Exception => 0
    }
  }

  def convertToLong(a: String): Long = {
    try {
      a.toLong
    } catch {
      case _: Exception => 0L
    }
  }

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Apple Store Spark RDD").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    /////////////////////////////////////
    // Load data into Spark DataFrame
    //////////////////////////////////////
    val telecomRdd = sparkContext.textFile("sample_data_apple_store/access.clean.log")
      .map(line => {
        val l = line.split(" - ")
        val host = l(0)
        if (l.length == 2) {
          val k = l(1).split(" ")
          val time = k(1).drop(1) + " " + k(2).dropRight(1)
          val method = k(3)
          val path = k(4)
          val protocal = k(5)
          val statusCode = convertToInt(k(6))
          val latency = convertToLong(k(7))
          val url = k(8)
          val userAgent = k.drop(9).mkString(" ").dropRight(2)
          Record(host, null, time, method, path, protocal, statusCode, latency, url, userAgent)
        } else {
          val k = l(1).split(" ")
          val time = k(1).drop(1) + " " + k(2).dropRight(1)
          val method = k(3)
          val path = k(4)
          val protocal = k(5)
          val statusCode = convertToInt(k(6))
          val latency = convertToLong(if (k.length < 8) "0" else k(7))
          val userAgent = l(2).dropRight(2)
          Record(host, null, time, method, path, protocal, statusCode, latency, null, userAgent)
        }
      })
    import sqlContext.implicits._

    val telecomDf = telecomRdd.toDF().cache()
    telecomDf.registerTempTable("telecom")

    val recordCount = telecomDf.count()
    println("Record count: " + recordCount) // 2,338,006

    /////////////////////////////////////
    // Load data into Spark DataFrame
    //////////////////////////////////////

    /////////////////////////////////////
    // Find out how many 404 HTTP codes are in access logs
    //////////////////////////////////////

    val sts404 = sqlContext.sql("select count(*) from telecom where statusCode = 404")
    sts404.show()
    val c0 = sts404
      .take(1)
      .head
      .getLong(0)
    println(s"There are '$c0' 404 HTTP codes are in access logs")

    /////////////////////////////////////
    // Find out how many 404 HTTP codes are in access logs
    //////////////////////////////////////


    /////////////////////////////////////
    // Find out which URLs are broken
    //////////////////////////////////////

    val brokenUrls = sqlContext.sql("select count(distinct url) from telecom where url is not null and cast(statusCode/100 as int) = 4").cache()
    brokenUrls.show()
    val c1 = brokenUrls
      .take(1)
      .head
      .getLong(0)
    println(s"There are total of $c1 broken urls")

    //////////////////////////////////////
    // Find out which URLs are broken
    //////////////////////////////////////


    /////////////////////////////////////
    // Verify there are no null columns in the original dataset
    //////////////////////////////////////

    val nullColumns = sqlContext.sql("select count(*) from telecom where url is null").cache()
    nullColumns.show()
    val c2 = nullColumns
      .take(1)
      .head
      .getLong(0)
    println(s"There are total of $c2 where url column is null and empty string is a null column for all rows")

    //////////////////////////////////////
    // Verify there are no null columns in the original dataset
    //////////////////////////////////////


    /////////////////////////////////////
    // Replace null values with constants
    //////////////////////////////////////

    val nullTrans = (url: String) => {
      if (url == null) "" else url
    }
    val nullUdf = udf(nullTrans)
    val telecomDfUpdated = telecomDf.withColumn("url", nullUdf(col("url")))
    telecomDfUpdated.registerTempTable("telecom_updated")
    val noNulls = sqlContext.sql("select count(*) from telecom_updated where url is null").cache()
    noNulls.show()
    val c3 = noNulls
      .take(1)
      .head
      .getLong(0)
    println(s"Replaced null values with constants. no null count: $c3 (this should be 0)")

    //////////////////////////////////////
    // Replace null values with constants
    //////////////////////////////////////


    /////////////////////////////////////
    // Parse timestamp to readable date
    //////////////////////////////////////

    val parseToReadableDate = (d: String) => {
      val k = d.split(":").head
      val l = k.split("/")
      val da = l(0)
      val y = l(2)
      val m = l(1)
      val month = m match {
        case "Jan" => "01"
        case "Feb" => "02"
        case "Mar" => "03"
        case "Apr" => "04"
        case "May" => "05"
        case "Jun" => "06"
        case "Jul" => "07"
        case "Aug" => "08"
        case "Sep" => "09"
        case "Oct" => "10"
        case "Nov" => "11"
        case "Dec" => "12"
      }
      s"$y-$month-$da"
    }
    val dateTranformUdf = udf(parseToReadableDate)

    val telecomDateDf = telecomDf.withColumn("date", dateTranformUdf(col("time"))).select("date").distinct().cache()
    telecomDateDf.show()
    println("Parse timestamp to readable date Done. Printing top 10 rows date")
    telecomDateDf.take(10).foreach(x => println(s"Date converted: ${x.getString(0)}"))

    /////////////////////////////////////
    // Parse timestamp to readable date
    //////////////////////////////////////


    /////////////////////////////////////
    // Describe which HTTP status values appear in data and how many
    //////////////////////////////////////

    val statusAnl = sqlContext.sql("select statusCode, count(*) from telecom where statusCode != 0 group by 1 order by 2 desc").cache()
    statusAnl.show()
    println("HTTP status values and its count:")
    statusAnl.collect().foreach(x => println(s"status code: ${x.getInt(0)}, count: ${x.getLong(1)}"))

    /////////////////////////////////////
    // Describe which HTTP status values appear in data and how many
    //////////////////////////////////////


    /////////////////////////////////////
    // How many unique hosts are there in the entire log and their average request
    //////////////////////////////////////

    val avgReqDf = sqlContext.sql("select count(*)/count(distinct host) avg_req_per_host, count(distinct host) distinct_hosts from telecom").cache()
    avgReqDf.show()
    val avgReq = avgReqDf
      .collect()
      .head
      .getDouble(0)
    val hostsDistinct = avgReqDf
      .collect()
      .head
      .getLong(1)
    println(s"Distinct hosts: $hostsDistinct, average request per host: $avgReq")

    /////////////////////////////////////
    // How many unique hosts are there in the entire log and their average request
    //////////////////////////////////////


  }
}
