import org.apache.spark.sql.{Row, SQLContext}
import org.apache.spark.{SparkConf, SparkContext}

object TelecomRddAnalysis {
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
    // Load file as a text file in spark
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
          if (k.length != 8) {
            //println(k.toList)
            //throw new Exception("Length of second element should be 8")
          }
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
      .cache()

    val recordCount = telecomRdd.count()
    println("Record count: " + recordCount) // 2,338,006

    /////////////////////////////////////
    // Load file as a text file in spark
    //////////////////////////////////////

    /////////////////////////////////////
    // Find out how many 404 HTTP codes are in access logs
    //////////////////////////////////////

    val sts404 = telecomRdd.filter(x => x.statusCode == 404).count()
    println(s"There are '$sts404' 404 HTTP codes are in access logs")

    /////////////////////////////////////
    // Find out how many 404 HTTP codes are in access logs
    //////////////////////////////////////

    /////////////////////////////////////
    // Find out which URLs are broken
    //////////////////////////////////////

    val brokenUrls = telecomRdd
      .filter(x => x.statusCode / 100 == 4)
      .filter(x => x.url != null)
      .map(x => x.url)
    println(s"There are total of ${brokenUrls.count()} broken")

    //////////////////////////////////////
    // Find out which URLs are broken
    //////////////////////////////////////


    /////////////////////////////////////
    // Verify there are no null columns in the original dataset
    //////////////////////////////////////

    val nullColumns = telecomRdd
      .filter(x => x.url == null)
      .map(x => x.url)
    println(s"There are total of ${nullColumns.count()} where url column is null and empty string is a null column for all rows")

    //////////////////////////////////////
    // Verify there are no null columns in the original dataset
    //////////////////////////////////////


    /////////////////////////////////////
    // Replace null values with constants
    //////////////////////////////////////

    val transformedNullCols = telecomRdd
      .map(x => {
        if (x.url == null) x.url = ""
        x.emptyCol = "0"
        x
      })
    val noNulls = transformedNullCols.filter(x => x.url == null).count()
    println(s"Replaced null values with constants. no null count: $noNulls (this should be 0)")

    //////////////////////////////////////
    // Replace null values with constants
    //////////////////////////////////////


    /////////////////////////////////////
    // Parse timestamp to readable date
    //////////////////////////////////////

    def parseToReadableDate(d: String): String = {
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

    val telecomRddDate = telecomRdd.map(x => {
      x.date = parseToReadableDate(x.time)
      x
    })
    println("Parse timestamp to readable date Done. Printing top 10 rows date")
    telecomRddDate.take(10).map(x => x.date).foreach(x => println(s"Date converted: $x"))

    /////////////////////////////////////
    // Parse timestamp to readable date
    //////////////////////////////////////


    /////////////////////////////////////
    // Describe which HTTP status values appear in data and how many
    //////////////////////////////////////

    val statusAnl = telecomRdd
      .map(x => x.statusCode)
      .filter(x => x != 0)
      .map(x => (x, 1L))
      .reduceByKey(_ + _)
      .collect()
    println("HTTP status values and its count:")
    statusAnl.foreach(x => println(s"status code: ${x._1}, count: ${x._2}"))

    /////////////////////////////////////
    // Describe which HTTP status values appear in data and how many
    //////////////////////////////////////



    /////////////////////////////////////
    // How many unique hosts are there in the entire log and their average request
    //////////////////////////////////////

    val hostsDistinct = telecomRdd
      .map(x => x.host)
      .distinct()
      .count()
    val avgReq = recordCount.toDouble / hostsDistinct
    println(s"Distinct hosts: $hostsDistinct, average request per host: $avgReq")

    /////////////////////////////////////
    // How many unique hosts are there in the entire log and their average request
    //////////////////////////////////////

  }
}
