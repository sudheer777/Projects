import org.apache.spark.sql.functions.udf
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}

import scala.collection.mutable

object Passenger {

  case class Info(from: String, to: String, date: String)

  case class Problem3(passengerId: String, max_count: Long)

  case class Problem4(passenger1: String, passenger2: String, flights_together: Long)

  case class Problem5(passenger1: String, passenger2: String, flights_together: Long, from: String, to: String)

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


    // problem 2
    flightDataDf.registerTempTable("flight_data")
    passengerDf.registerTempTable("passengers")
    sqlContext.sql("select * from (select passengerId, count(*) cnt from flight_data group by passengerId order by cnt desc limit 100) A join passengers ON A.passengerId = passengers.passengerId").show(100, false)


    // problem 3
    /**
     * Find the greatest number of countries a passenger has been in without being in the UK. For example, if the countries a passenger was in were: UK -> FR -> US -> CN -> UK -> DE -> US -> LK -> KK, the correct answer would be 3 countries.
     */
    // collate entire data based on passengerId
    // Datrafme -> RDD with df.toRdd

    val passengerRdd = flightDataDf.select("passengerId", "from", "to", "date")
      .rdd // RDD[Row]
      .map(x => (x.getString(0), Info(x.getString(1), x.getString(2), x.getString(3)))) // RDD[(String, Info)]
      .groupByKey() // RDD[(String, Iterable[Info])]
      .map(row => {
        val pid = row._1
        val seqEvents = row._2.toArray.sortBy(x => x.date)
        // assumption is that there wont be any missing pieces

        val carr = mutable.ArrayBuffer(seqEvents.head.from) // origin very first time
        seqEvents.foreach(x => carr.append(x.to))
        // carr: UK -> FR -> US -> CN -> UK -> DE -> UK
        var count = 0
        var gcount = 0
        carr.foreach(x => {
          if (x == "UK") {
            if (count > gcount) gcount = count
            count = 0
          } else {
            count += 1
          }
        })
        if (count > gcount) gcount = count
        Problem3(pid, gcount)
      }).sortBy(x => -x.max_count)

    import sqlContext.implicits._

    passengerRdd.toDF().show() // problem 3


    // problem 4
    // Find the passengers who have been on more than 3 flights together
    // flight id A, B, C, D

    def red(a1: Long, a2: Long): Long = a1 + a2


    val problem4Df = flightDataDf.select("passengerId", "flightId")
      .rdd
      .map(x => (x.getString(1), x.getString(0))) // RDD[(String, String)] key is flight id
      .groupByKey() // RDD[(String, Iterable[String])]
      .flatMap(x => {
        val fid = x._1
        val passengers = x._2.toArray.distinct.sorted // all possible passagers travelled using this flight
        // 4 passangers A,B,C,D -> (A,B),1, (A,C),1 -> flight 1
        // flight 2 -> (A, B), 1
        possiblePassengers(passengers) // if the last line is array it will flatten
      }).reduceByKey(red)
      .map(x => Problem4(x._1._1, x._1._2, x._2))
      .filter(x => x.flights_together > 3)
      .sortBy(-_.flights_together)
      .toDF()
    problem4Df.show()


    /**
     * +----------+----------+----------------+
      |passenger1|passenger2|flights_together|
      +----------+----------+----------------+
      |       701|       760|              15|
      |      3503|      3590|              14|
      |      2717|      2759|              14|
      |      2939|      5490|              13|
      |      2939|      4395|              12|
      |       366|       374|              12|
      |      3021|      9522|              12|
      |      1208|      3093|              12|
      |      1371|       975|              12|
      |      4395|      4399|              12|
      |       701|       763|              12|
      |      1673|      4827|              12|
      |       760|       763|              12|
      |      3278|      5423|              12|
      |      5481|      5491|              12|
      |      1337|      1484|              12|
      |      2550|      4441|              12|
      |      4016|      4059|              12|
      |      3590|      6813|              12|
      |      7877|      9252|              12|
      +----------+----------+----------------+
     */


    // problem 5

    def convertDateFormat(date: String): String = {
      // format dd/mm/yyyy
      // output yyyymmdd
      val d = date.split("/")
      d(2) + d(1) + d(0)
    }

    def convertBack(date: String): String = {
      val y = date.substring(0, 4)
      val m = date.substring(4, 6)
      val d = date.substring(6)
      s"$d/$m/$y"
    }
    
    // assume including from and to dates as well
    def flownTogether(flightDataDf: DataFrame, atLeastNTimes: Int, from: String, to: String): DataFrame = {
      val fromConv = convertDateFormat(from)
      val toConv = convertDateFormat(to)

      def red(a1: (String, String, Long), a2: (String, String, Long)): (String, String, Long) = {
        // 01/02/2020, 02/01/2020
        // yyyymmdd
        // 2020
        (if (a1._1 > a2._1) a2._1 else a1._1, if (a1._1 > a2._1) a1._1 else a2._1, a1._3 + a2._3)
      }

      flightDataDf.select("passengerId", "flightId", "date")
        .rdd
        .filter(x => {
          val date = x.getString(2)
          val dateC = convertDateFormat(date)
          dateC >= fromConv && dateC <= toConv
        })
        .map(x => ((x.getString(1),  convertDateFormat(x.getString(2))), x.getString(0))) // RDD[((String, String), String)] key is flight id
        .groupByKey() // RDD[((String, String), Iterable[String])]
        .flatMap(x => {
          val fid = x._1._1
          val date = x._1._2
          val passengers = x._2.toArray.distinct.sorted // all possible passagers travelled using this flight
          // 4 passangers A,B,C,D -> (A,B),1, (A,C),1 -> flight 1
          // flight 2 -> (A, B), 1
          possiblePassengers(passengers).map(x => (x._1, (date, date, x._2))) // if the last line is array it will flatten
        }).reduceByKey(red)
        .map(x => Problem5(x._1._1, x._1._2, x._2._3, convertBack(x._2._1), convertBack(x._2._2)))
        .filter(x => x.flights_together > atLeastNTimes)
        .sortBy(-_.flights_together)
        .toDF()
    }

    flownTogether(flightDataDf, 4, "01/01/2017", "31/01/2017").show()
  }

  def possiblePassengers(inp: Array[String]): Array[((String, String), Long)] = {
    val res = mutable.ArrayBuffer[((String, String), Long)]()
    var i = 0
    while (i < inp.length-1) {
      var j = i + 1
      while (j < inp.length) {
        res.append(((inp(i), inp(j)), 1L))
        j += 1
      }
      i += 1
    }
    res.toArray
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
