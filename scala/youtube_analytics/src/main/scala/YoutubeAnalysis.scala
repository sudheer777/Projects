import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.functions.{col, lit}
import org.apache.spark.sql.types.{BooleanType, IntegerType, LongType, StringType, StructField, StructType}
import org.apache.spark.{SparkConf, SparkContext}


object YoutubeAnalysis {
  def main(args: Array[String]): Unit = {
    val usvideos = if (args.length >= 1) args.head else "sample_data/USvideos.csv"
    val catTitle = if (args.length >= 2) args(1) else "sample_data/category_title.csv"
    // remove .setMaster("local[*]") in below line when running in cluster mode
    val sc = new SparkConf().setAppName("YoutubeAnalytics").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)
    val schema = StructType(Seq(
      StructField("video_id", StringType),
      StructField("trending_date", StringType),
      StructField("title", StringType),
      StructField("channel_title", StringType),
      StructField("category_id", IntegerType),
      StructField("publish_time", StringType),
      StructField("tags", StringType),
      StructField("views", LongType),
      StructField("likes", LongType),
      StructField("dislikes", LongType),
      StructField("comment_count", LongType),
      StructField("comments_disabled", BooleanType),
      StructField("ratings_disabled", BooleanType)
    ))

    val catSchema = StructType(Seq(
      StructField("category_id1", IntegerType),
      StructField("category", StringType)
    ))

    val vidDf = sqlContext.read.schema(schema).option("header", "true").csv(usvideos)
    val catDf = sqlContext.read.schema(catSchema).csv(catTitle)
    val df = vidDf.join(catDf, vidDf("category_id") === catDf("category_id1"), "left").filter(col("video_id") =!= lit("#NAME?")).cache()
    df.registerTempTable("videos")

    // Top 3 videos for which user interaction (views + likes + dislikes + comments) is the highest.
    println("Top 3 videos for which user interaction (views + likes + dislikes + comments) is the highest:")
    sqlContext.sql(
      """
        |select video_id from
        |(select video_id, sum(views + likes + dislikes + comment_count) tot
        |   from videos group by video_id order by tot desc limit 3)
        |""".stripMargin).show()

    // Bottom 3 videos for which user interaction (views + likes + dislikes + comments) is lowest
    println("Bottom 3 videos for which user interaction (views + likes + dislikes + comments) is lowest:")
    sqlContext.sql(
      """
        |select video_id from
        |(select video_id, sum(views + likes + dislikes + comment_count) tot
        |   from videos group by video_id order by tot limit 3)
        |""".stripMargin).show()

    // Top 3 videos of each category in each year
    // By number of views
    println("Top 3 videos of each category in each year. By number of views:")
    sqlContext.sql(
      """
        |select video_id, category, year from (
        |select video_id, category, year, dense_rank() over(partition by category, year order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[0] year, sum(views) tot
        | from videos group by video_id, category, year) as a
        | )
        |where rn <= 3
        |order by category, year
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each year
    // By number of comments
    println("Top 3 videos of each category in each year. By number of comments:")
    sqlContext.sql(
      """
        |select video_id, category, year from (
        |select video_id, category, year, dense_rank() over(partition by category, year order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[0] year, sum(comment_count) tot
        | from videos group by video_id, category, year) as a
        | )
        |where rn <= 3
        |order by category, year
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each year
    // By number of likes
    println("Top 3 videos of each category in each year. By number of likes:")
    sqlContext.sql(
      """
        |select video_id, category, year from (
        |select video_id, category, year, dense_rank() over(partition by category, year order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[0] year, sum(likes) tot
        | from videos group by video_id, category, year) as a
        | )
        |where rn <= 3
        |order by category, year
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each year
    // By Highest user interaction
    println("Top 3 videos of each category in each year. By Highest user interaction :")
    sqlContext.sql(
      """
        |select video_id, category, year from (
        |select video_id, category, year, dense_rank() over(partition by category, year order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[0] year, sum(views + likes + dislikes + comment_count) tot
        | from videos group by video_id, category, year) as a
        | )
        |where rn <= 3
        |order by category, year
        |""".stripMargin).show(1000, false)

    // Top 3 videos in each month
    // Likes or Dislikes ratio is highest
    println("Top 3 videos in each month, Likes or Dislikes ratio is highest:")
    sqlContext.sql(
      """
        |select video_id, month from (
        |select video_id, month, dense_rank() over(partition by month order by l/d desc) rn
        |from
        |(select video_id, split(publish_time, "-")[1] month, sum(likes) l, sum(dislikes) d
        | from videos group by video_id, month) as a
        | )
        |where rn <= 3
        |order by month
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each month
    // By number of views
    println("Top 3 videos of each category in each month. By number of views:")
    sqlContext.sql(
      """
        |select video_id, category, month from (
        |select video_id, category, month, dense_rank() over(partition by category, month order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[1] month, sum(views) tot
        | from videos group by video_id, category, month) as a
        | )
        |where rn <= 3
        |order by category, month
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each month
    // By number of likes
    println("Top 3 videos of each category in each month. By number of likes:")
    sqlContext.sql(
      """
        |select video_id, category, month from (
        |select video_id, category, month, dense_rank() over(partition by category, month order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[1] month, sum(likes) tot
        | from videos group by video_id, category, month) as a
        | )
        |where rn <= 3
        |order by category, month
        |""".stripMargin).show(1000, false)

    // Top 3 videos of each category in each month
    // By number of likes
    println("Top 3 videos of each category in each month. By number of dislikes:")
    sqlContext.sql(
      """
        |select video_id, category, month from (
        |select video_id, category, month, dense_rank() over(partition by category, month order by tot desc) rn
        |from
        |(select video_id, category, split(publish_time, "-")[1] month, sum(dislikes) tot
        | from videos group by video_id, category, month) as a
        | )
        |where rn <= 3
        |order by category, month
        |""".stripMargin).show(1000, false)


    //  Top 3 channels
    // By number of total views
    println("Top 3 channels. By number of total views ")
    sqlContext.sql(
      """
        |select channel_title from (
        |select channel_title, sum(views) tot from videos group by channel_title order by tot desc limit 3)
        |""".stripMargin).show(100, false)

    //  Top 3 channels
    // Likes or Dislikes ratio is highest
    println("Top 3 channels. Likes or Dislikes ratio is highest")
    sqlContext.sql(
      """
        |select channel_title from (
        |select channel_title, sum(likes) l, sum(dislikes) d from videos group by channel_title order by l/d desc limit 3)
        |""".stripMargin).show(100, false)

    //  Top 3 channels
    // By number of total comments
    println("Top 3 channels. By number of total comments")
    sqlContext.sql(
      """
        |select channel_title from (
        |select channel_title, sum(comment_count) tot from videos group by channel_title order by tot desc limit 3)
        |""".stripMargin).show(100, false)

    //  Top 3 categories
    // By number of total views
    println("Top 3 categories. By number of total views ")
    sqlContext.sql(
      """
        |select category from (
        |select category, sum(views) tot from videos group by category order by tot desc limit 3)
        |""".stripMargin).show(100, false)

    //  Top 3 categories
    // Likes or Dislikes ratio is highest
    println("Top 3 categories. Likes or Dislikes ratio is highest")
    sqlContext.sql(
      """
        |select category from (
        |select category, sum(likes) l, sum(dislikes) d from videos group by category order by l/d desc limit 3)
        |""".stripMargin).show(100, false)

    //  Top 3 categories
    // By number of total comments
    println("Top 3 categories. By number of total comments")
    sqlContext.sql(
      """
        |select category from (
        |select category, sum(comment_count) tot from videos group by category order by tot desc limit 3)
        |""".stripMargin).show(100, false)

    // Top 3 videos
    // Value calculated by below formula is highest
    // Views on most recent date / (Recent Date - Publish Date)
    println("Top 3 videos, Value calculated by below formula is highest,  Views on most recent date / (Recent Date - Publish Date)")
    sqlContext.sql(
      """
        |select video_id
        |from
        |videos order by views/((INT(unix_timestamp(trending_date, "yy.dd.MM"))-INT(unix_timestamp(split(publish_time, "T")[0], "yyyy-MM-dd")))/86400)
        | desc limit 3
        |""".stripMargin).show(100, false)

    //  Calculate any 3 videos which got at least 5 comments on every 1000 views
    println("Calculate any 3 videos which got at least 5 comments on every 1000 views:")
    sqlContext.sql(
      """
        |select video_id from videos where comment_count/5 >= views/1000 limit 3
        |""".stripMargin).show(100, false)

    // Calculate any 3 videos which got at least 4 likes on every 100 views
    println("Calculate any 3 videos which got at least 4 likes on every 100 views:")
    sqlContext.sql(
      """
        |select video_id from videos where likes/4 >= views/100 limit 3
        |""".stripMargin).show(100, false)


    // Number of videos published in each category.
    println("Number of videos published in each category:")
    sqlContext.sql(
      """
        |select category, count(distinct video_id) from videos group by category
        |""".stripMargin).show(100, false)

    sparkContext.stop()
  }
}
