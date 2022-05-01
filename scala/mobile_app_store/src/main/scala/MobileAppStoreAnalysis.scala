import org.apache.spark.sql.{Row, SQLContext}
import org.apache.spark.{SparkConf, SparkContext}

object MobileAppStoreAnalysis {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Apple Store Spark RDD")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    /////////////////////////////////////
    // Load csv into spark as a text file
    // Parse the data as csv
    //////////////////////////////////////
    // header: _c0|id|track_name|size_bytes|currency|price|rating_count_tot|rating_count_ver|user_rating|user_rating_ver|ver|cont_rating|prime_genre|sup_devices.num|ipadSc_urls.num|lang.num|vpp_lic
    val appleStoreDf = sqlContext.read
      .option("header", "true")
      .option("quote", "\"")
      .option("escape", "\"")
      .csv("sample_data_apple_store/AppleStore.csv")
      .cache()
    val appleStoreRdd = appleStoreDf.rdd
    val appleStoreColsMap = appleStoreDf.columns.zipWithIndex.toMap

    val appleStoreDescDf = sqlContext.read
      .option("header", "true")
      .option("multiLine", "true")
      .option("quote", "\"")
      .option("escape", "\"")
      .csv("sample_data_apple_store/appleStore_description.csv")
      .cache()
    val appleStoreDescRdd = appleStoreDescDf.rdd
    val appleStoreDescColsMap = appleStoreDescDf.columns.zipWithIndex.toMap

    /////////////////////////////////////
    // Load csv into spark as a text file
    // Parse the data as csv
    //////////////////////////////////////


    /////////////////////////////////////
    // Convert bytes to MB and GB in a new column
    //////////////////////////////////////
    val appleStoreRddNewColumn = appleStoreRdd
      .map(x => {
        val sizeBytes = x.getString(appleStoreColsMap("size_bytes")).toDouble
        val mbVal = sizeBytes/(1024*1024)
        val gbVal = mbVal/1024
        Row.fromSeq(x.toSeq :+ mbVal :+ gbVal)
      })
    println("Top 10 rows:")
    appleStoreRddNewColumn.take(10).foreach(println)
    /////////////////////////////////////
    // Convert bytes to MB and GB in a new column
    //////////////////////////////////////



    /////////////////////////////////////
    // List top 10 trending apps
    //////////////////////////////////////
    val trendingAppsRdd = appleStoreRdd
      .map(x => (x.getString(appleStoreColsMap("id")), x.getString(appleStoreColsMap("track_name")), x.getString(appleStoreColsMap("user_rating_ver")).toDouble, x.getString(appleStoreColsMap("rating_count_ver")).toLong))
      .sortBy(x => (-x._3, -x._4))
      .take(10)
    println("List top 10 trending apps:")
    trendingAppsRdd.foreach(x => println(s"id: ${x._1}, track_name: ${x._2}, rating for latest version: ${x._3}, number of users rated for latest version: ${x._4}"))
    /////////////////////////////////////
    // List top 10 trending apps
    //////////////////////////////////////


    /////////////////////////////////////
    // The difference in the average number of screenshots displayed of highest and lowest rating apps
    //////////////////////////////////////

    val ratingRdd = appleStoreRdd.map(x => x.getString(appleStoreColsMap("user_rating")).toDouble).cache()
    val maxRating = ratingRdd.max()
    val minRating = ratingRdd.min()
    val minRatingAppsRdd = appleStoreRdd
      .filter(x => x.getString(appleStoreColsMap("user_rating")).toDouble == minRating)
      .map(x => x.getString(appleStoreColsMap("ipadSc_urls.num")).toInt).cache()
    val minRatingsAvg = minRatingAppsRdd.sum() / minRatingAppsRdd.count()

    val maxRatingAppsRdd = appleStoreRdd
      .filter(x => x.getString(appleStoreColsMap("user_rating")).toDouble == maxRating)
      .map(x => x.getString(appleStoreColsMap("ipadSc_urls.num")).toInt).cache()
    val maxRatingsAvg = maxRatingAppsRdd.sum() / maxRatingAppsRdd.count()

    println(s"The difference in the average number of screenshots displayed of highest and lowest rating apps: ${maxRatingsAvg - minRatingsAvg}")

    /////////////////////////////////////
    // The difference in the average number of screenshots displayed of highest and lowest rating apps
    //////////////////////////////////////

    /////////////////////////////////////
    // What percentage of high rated apps support multiple languages
    //////////////////////////////////////

    val maxRateAppsRdd = appleStoreRdd
      .filter(x => x.getString(appleStoreColsMap("user_rating")).toDouble == maxRating).cache()

    val highRatedAppsCount = maxRateAppsRdd.count()
    val higheRatedWithMultLanguages = maxRateAppsRdd
      .filter(x => x.getString(appleStoreColsMap("lang.num")).toInt > 1)
      .count()
    val percent = (higheRatedWithMultLanguages.toDouble / highRatedAppsCount) * 100
    println(s"Percentage of high rated apps support multiple languages: ${percent}%")

    /////////////////////////////////////
    // What percentage of high rated apps support multiple languages
    //////////////////////////////////////


    /////////////////////////////////////
    // How does app details contribute to user ratings
    //////////////////////////////////////

    def red1(a: (Double, Long, Long), b: (Double, Long, Long)): (Double, Long, Long) = {
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)
    }
    val ratingaveragePriceAndSizeLength = appleStoreRdd
      .map(x => (x.getString(appleStoreColsMap("user_rating")).toDouble, (x.getString(appleStoreColsMap("price")).toDouble, x.getString(appleStoreColsMap("size_bytes")).toLong, 1L)))
      .reduceByKey(red1)
      .map(x => (x._1, x._2._1/x._2._3, x._2._2.toDouble/x._2._3))
      .sortBy(-_._1)
    println("Rating with average price, average size in bytes")
    ratingaveragePriceAndSizeLength.collect().foreach(println)

    /////////////////////////////////////
    // How does app details contribute to user ratings
    //////////////////////////////////////


    /////////////////////////////////////
    // Compare the statistics of different app groups/genres
    //////////////////////////////////////
    val genreRatingRdd = appleStoreRdd
      .map(x => (x.getString(appleStoreColsMap("prime_genre")), x.getString(appleStoreColsMap("user_rating")).toDouble))
      .groupByKey()
      .map(x => {
        val ratings = x._2.toArray
        (x._1, ratings.sum / ratings.length)
      }).sortBy(_._1)

    println("Average rating per app genres:")
    genreRatingRdd.collect().foreach(println)
    /////////////////////////////////////
    // Compare the statistics of different app groups/genres
    //////////////////////////////////////


    /////////////////////////////////////
    // Does length of app description contribute to the ratings?
    //////////////////////////////////////

    def red(a: (Long, Long), b: (Long, Long)): (Long, Long) = {
      (a._1 + b._1, a._2 + b._2)
    }
    val appleStoreDsRdd = appleStoreDescRdd.map(x => (x.getString(appleStoreDescColsMap("id")), x.getString(appleStoreDescColsMap("app_desc")).length))
    val appleRtngRdd = appleStoreRdd.map(x => (x.getString(appleStoreColsMap("id")), x.getString(appleStoreColsMap("user_rating")).toDouble))
    val joinRdd = appleStoreDsRdd.join(appleRtngRdd)
    val ratingaverageDescLength = joinRdd
      .map(x => (x._2._2, (x._2._1.toLong, 1L)))
      .reduceByKey(red)
      .map(x => (x._1, x._2._1.toDouble/x._2._2))
      .sortBy(-_._1)
    println("Rating with average length of description")
    ratingaverageDescLength.collect().foreach(println)

    /////////////////////////////////////
    // Does length of app description contribute to the ratings?
    //////////////////////////////////////
  }
}
