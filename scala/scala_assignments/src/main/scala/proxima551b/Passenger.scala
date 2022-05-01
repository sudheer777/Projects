package proxima551b

import org.apache.spark.sql.functions.udf
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}

import scala.collection.mutable

object Passenger {

  case class FromToInfo(from: String, to: String, date: String)

  case class PassengerCount(passengerId: String, max_count: Long)

  case class PassengerFlightTogether(passenger1: String, passenger2: String, flights_together: Long)

  case class PassengerFlightTogetherFromTo(passenger1: String, passenger2: String, flights_together: Long, from: String, to: String)

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc) // rdd helper
    val sqlContext = new SQLContext(sparkContext) // dataframe helper

    val passengerDf = sqlContext.read.option("header", "true").csv("/Users/sudheerpendyala/Downloads/passengers_file.csv") // |passengerId|firstName|lastName
    passengerDf.show()
    val flightDataDf = sqlContext.read.option("header", "true").csv("/Users/sudheerpendyala/Downloads/flightData_file.csv")  // |passengerId|flightId|from|to|date|
    flightDataDf.show()


    // problem1
    val flightMonthDf = computeMonth(flightDataDf).cache()
    // flightMonthDf.show()
    val res1 = flightMonthDf.select("month", "flightId").distinct()
    res1.groupBy("month").count().orderBy("month").show()


    // problem 2
    // regestiring both dataframes as tmp tables
    flightDataDf.registerTempTable("flights")
    passengerDf.registerTempTable("passengers")
    val psql =
      """
        |select A.passengerId, Number_of_Flights, firstName, lastName from
        |  (select passengerId, count(*) Number_of_Flights from flights
        |      group by passengerId order by Number_of_Flights desc limit 100) A
        |    join passengers ON
        |    A.passengerId = passengers.passengerId order by Number_of_Flights desc
        |""".stripMargin
    sqlContext.sql(psql)
      .show(100, false)


    // problem 3
    /**
     * Find the greatest number of countries a passenger has been in without being in the UK. For example, if the countries a passenger was in were: UK -> FR -> US -> CN -> UK -> DE -> US -> LK -> KK, the correct answer would be 3 countries.
     */
    // collate entire data based on passengerId
    // trying to bring all the data for a given passenger together
    val passengerRdd = flightDataDf.select("passengerId", "from", "to", "date")
      .rdd // RDD[Row]
      .map(x => (x.getString(0), FromToInfo(x.getString(1), x.getString(2), x.getString(3)))) // RDD[(String, FromToInfo)]
      .groupByKey() // RDD[(String, Iterable[FromToInfo])]
      .map(row => {
        val pid = row._1
        val seqEvents = row._2.toArray.sortBy(x => x.date) // assumption is that there wont be any missing pieces

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
        PassengerCount(pid, gcount)
      }).sortBy(x => -x.max_count)

    import sqlContext.implicits._

    passengerRdd.toDF().show(100) // problem 3


    // problem 4
    // Find the passengers who have been on more than 3 flights together
    // flight id A, B, C, D

    def red(a1: Long, a2: Long): Long = a1 + a2


    val problem4Df = flightDataDf.select("passengerId", "flightId")
      .rdd
      .map(x => (x.getString(1), x.getString(0))) // RDD[(String, String)] key is flight id
      .groupByKey() // RDD[(String, Iterable[String])] // all the passengers together for a given flight
      .flatMap(x => {
        val fid = x._1
        val passengers = x._2.toArray.distinct.sorted // all possible passagers travelled using this flight
        // 4 passangers A,B,C,D -> (A,B),1, (A,C),1 -> flight 1
        // flight 2 -> (A, B), 1
        possiblePassengers(passengers) // if this Array(1, 2, 3)
      }).reduceByKey(red)
      .map(x => PassengerFlightTogether(x._1._1, x._1._2, x._2))
      .filter(x => x.flights_together > 3)
      .sortBy(-_.flights_together)
      .toDF()
    problem4Df.show()


    // problem 5
    def convertDateFormat(date: String): String = {
      // format yyyy-mm-dd
      // output yyyymmdd
      val d = date.split("-")
      d(0) + d(1) + d(2)
    }

    def convertBack(date: String): String = {
      // format yyyymmdd
      // output yyyy-mm-dd
      val y = date.substring(0, 4)
      val m = date.substring(4, 6)
      val d = date.substring(6)
      s"$y-$m-$d"
    }

    // assuming including from and to dates as well
    def flownTogether(flightDataDf: DataFrame, atLeastNTimes: Int, from: String, to: String): DataFrame = {
      val fromConv = convertDateFormat(from)
      val toConv = convertDateFormat(to)

      def red(a1: (String, String, Long), a2: (String, String, Long)): (String, String, Long) = {
        (if (a1._1 > a2._1) a2._1 else a1._1, // min date for given pair of passengers
          if (a1._1 > a2._1) a1._1 else a2._1, // max date for given pair of passengers
          a1._3 + a2._3)
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
          possiblePassengers(passengers).map(x => (x._1, (date, date, x._2)))
        }).reduceByKey(red) // for a given passenger pair, you will get min date, max date and flights together
        .map(x => PassengerFlightTogetherFromTo(x._1._1, x._1._2, x._2._3, convertBack(x._2._1), convertBack(x._2._2)))
        .filter(x => x.flights_together > atLeastNTimes)
        .sortBy(-_.flights_together)
        .toDF()
    }

    flownTogether(flightDataDf, 5, "2017-01-01", "2017-03-31").show()
  }

  // all permutations of passengers with 1 as count
  // if there are p1,p2,p3,p4 as passengers
  // output will be [((p1, p2), 1), ((p1, p3), 1), ((p1, p4), 1), ((p2, p3), 1), ((p2, p4), 1), ((p3, p4), 1)]
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

  // Adds month column to dataframe
  def computeMonth(df: DataFrame): DataFrame = {
    val cmonth = (date: String) => {
      val d = date.split("-")
      d(1).toInt
    }
    val cmudf = udf(cmonth)
    df.withColumn("month", cmudf(df("date"))) // new column month will be added with udfs
  }
}
