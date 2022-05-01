package kb0824

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.functions.{asc, col, count, countDistinct, date_format, desc, from_unixtime, lag, lit, max, round, row_number, sum, to_date, unix_timestamp}
import org.apache.spark.sql.types.{FloatType, IntegerType, StringType, StructField, StructType}

object Problem2 {
  case class DayOfWeek(day_of_week: String, avg_count: Double)
  case class PickUp(hour_of_day: Int, Zone: String, max_count: Long)
  case class PercentChange(day: Int, percent_change: Double)

  def main(args: Array[String]): Unit = {
    def display(df: DataFrame): Unit = df.show()

    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val spark = new SQLContext(sparkContext)
    import spark.implicits._

    // COMMAND ----------

    // STARTER CODE - DO NOT EDIT THIS CELL
    val customSchema = StructType(Array(StructField("lpep_pickup_datetime", StringType, true), StructField("lpep_dropoff_datetime", StringType, true), StructField("PULocationID", IntegerType, true), StructField("DOLocationID", IntegerType, true), StructField("passenger_count", IntegerType, true), StructField("trip_distance", FloatType, true), StructField("fare_amount", FloatType, true), StructField("payment_type", IntegerType, true)))

    // COMMAND ----------

    // STARTER CODE - YOU CAN LOAD ANY FILE WITH A SIMILAR SYNTAX.
    val df = spark.read
      .format("com.databricks.spark.csv")
      .option("header", "true") // Use first line of all files as header
      .option("nullValue", "null")
      .schema(customSchema)
      .load("file:/Users/sudheerpendyala/Downloads/q2/nyc-tripdata.csv")//.load("/FileStore/tables/nyc_tripdata.csv") // the csv file which you want to work with
      .withColumn("pickup_datetime", from_unixtime(unix_timestamp(col("lpep_pickup_datetime"), "MM/dd/yyyy HH:mm")))
      .withColumn("dropoff_datetime", from_unixtime(unix_timestamp(col("lpep_dropoff_datetime"), "MM/dd/yyyy HH:mm")))
      .drop($"lpep_pickup_datetime")
      .drop($"lpep_dropoff_datetime")

    // COMMAND ----------

    // LOAD THE "taxi_zone_lookup.csv" FILE SIMILARLY AS ABOVE. CAST ANY COLUMN TO APPROPRIATE DATA TYPE IF NECESSARY.
    val customSchema2 = StructType(Array( StructField("LocationID", IntegerType, true), StructField("Borough", StringType, true), StructField("Zone", StringType, true), StructField("service_zone", StringType, true)))
    // ENTER THE CODE BELOW
    val df2 = spark.read
      .format("com.databricks.spark.csv")
      .option("header", "true") // Use first line of all files as header
      .option("nullValue", "null")
      .schema(customSchema2)
      .load("file:/Users/sudheerpendyala/Downloads/q2/taxi_zone_lookup.csv")//.load("/FileStore/tables/taxi_zone_lookup.csv") // the csv file which you want to work with

    // COMMAND ----------

    // STARTER CODE - DO NOT EDIT THIS CELL
    // Some commands that you can use to see your dataframes and results of the operations. You can comment the df.show(5) and uncomment display(df) to see the data differently. You will find these two functions useful in reporting your results.
    // display(df)
    df.show(5) // view the first 5 rows of the dataframe

    // COMMAND ----------

    // STARTER CODE - DO NOT EDIT THIS CELL
    // Filter the data to only keep the rows where "PULocationID" and the "DOLocationID" are different and the "trip_distance" is strictly greater than 2.0 (>2.0).

    // VERY VERY IMPORTANT: ALL THE SUBSEQUENT OPERATIONS MUST BE PERFORMED ON THIS FILTERED DATA

    val df_filter = df.filter($"PULocationID" =!= $"DOLocationID" && $"trip_distance" > 2.0)
    df_filter.show(5)

    // COMMAND ----------

    // PART 1a: The top-5 most popular drop locations - "DOLocationID", sorted in descending order - if there is a tie, then one with lower "DOLocationID" gets listed first
    // Output Schema: DOLocationID int, number_of_dropoffs int

    // Hint: Checkout the groupBy(), orderBy() and count() functions.

    // ENTER THE CODE BELOW
    val df_drpup=df_filter.groupBy("DOLocationID").agg(count("DOLocationID").as("number_of_dropoffs")).orderBy(col("number_of_dropoffs").desc,col("DOLocationID").asc)
    display(df_drpup.limit(5))

    // COMMAND ----------

    // PART 1b: The top-5 most popular pickup locations - "PULocationID", sorted in descending order - if there is a tie, then one with lower "PULocationID" gets listed first
    // Output Schema: PULocationID int, number_of_pickups int

    // Hint: Code is very similar to part 1a above.

    // ENTER THE CODE BELOW
    val df_pickup=df_filter.groupBy("PULocationID").agg(count("PULocationID").as("number_of_pickups")).orderBy(col("number_of_pickups").desc,col("PULocationID").asc)
    display(df_pickup.limit(5))

    // COMMAND ----------

    // PART 2: List the top-3 locations with the maximum overall activity, i.e. sum of all pickups and all dropoffs at that LocationID. In case of a tie, the lower LocationID gets listed first.
    // Output Schema: LocationID int, number_activities int

    // Hint: In order to get the result, you may need to perform a join operation between the two dataframes that you created in earlier parts (to come up with the sum of the number of pickups and dropoffs on each location).

    // ENTER THE CODE BELOW
    val joined=df_pickup.join(df_drpup,col("PULocationID")===col("DOLocationID"),"inner")
    val sumed=joined.withColumnRenamed("PULocationID","LocationID").select("LocationID","number_of_pickups","number_of_dropoffs").withColumn("number_activities",col("number_of_pickups")+col("number_of_dropoffs"))
    val res=sumed.select("LocationID","number_activities")
    display(res.limit(3))

    // COMMAND ----------

    // PART 3: List all the boroughs in the order of having the highest to lowest number of activities (i.e. sum of all pickups and all dropoffs at that LocationID), along with the total number of activity counts for each borough in NYC during that entire period of time.
    // Output Schema: Borough string, total_number_activities int

    // Hint: You can use the dataframe obtained from the previous part, and will need to do the join with the 'taxi_zone_lookup' dataframe. Also, checkout the "agg" function applied to a grouped dataframe.

    // ENTER THE CODE BELOW
    val df_res=res.withColumnRenamed("LocationID","LocationID_").join(df2,col("LocationID_") === col("LocationID"),"inner").groupBy("Borough")
      .agg(sum("number_activities").as("total_number_activities"))
      .withColumn("total_number_activities",col("total_number_activities").cast("int"))
      .orderBy(col("total_number_activities").desc)
    display(df_res)

    // COMMAND ----------

    // PART 4: List the top 2 days of week with the largest number of (daily) average pickups, along with the values of average number of pickups on each of the two days. The day of week should be a string with its full name, for example, "Monday" - not a number 1 or "Mon" instead.
    // Output Schema: day_of_week string, avg_count float

    // Hint: You may need to group by the "date" (without time stamp - time in the day) first. Checkout "to_date" function.

    // ENTER THE CODE BELOW
    val df_date1 = df_filter.select(col("pickup_datetime"), to_date(col("pickup_datetime"),"yyyy-mm-dd").as("to_date"))
      .withColumn("day_of_week",date_format(col("to_date"), "EEEEEE"))
      .select("day_of_week", "to_date").cache()

    val df_day1 = df_date1.groupBy("day_of_week").agg(countDistinct("to_date").as("day_count"))
    val df_day2 = df_date1.groupBy("day_of_week").count().withColumnRenamed("count","total_count")
    val df_day3 = df_day1.as("df1").join(df_day2.as("df2"), df_day1("day_of_week") === df_day2("day_of_week")).select("df1.day_of_week", "df1.day_count", "df2.total_count")
    val final_df = df_day3.withColumn("avg_count", ($"total_count" / $"day_count").cast("float")).orderBy(desc("avg_count"))

    display(final_df.select("day_of_week","avg_count").limit(2))


    // COMMAND ----------

    // PART 5: For each particular hour of a day (0 to 23, 0 being midnight) - in their order from 0 to 23, find the zone in Brooklyn borough with the LARGEST number of pickups.
    // Output Schema: hour_of_day int, zone string, max_count int

    // Hint: You may need to use "Window" over hour of day, along with "group by" to find the MAXIMUM count of pickups

    // ENTER THE CODE BELOW

    val df_brk = df_filter.join(df2, df_filter.col("PULocationID") === df2.col("LocationID"),"inner")
      .withColumn("hour_of_day",date_format(col("pickup_datetime"), "HH"))
      .filter(col("Borough") === lit("Brooklyn"))
      .groupBy("hour_of_day","zone").count()
    //.drop("pickup_datetime","dropoff_datetime","PULocationID","DOLocationID","passenger_count","trip_distance","fare_amount","payment_type" )
    val window1 = Window.partitionBy("hour_of_day").orderBy(desc("count")) //.groupBy("PULocationID").count()

    val df_brook = df_brk.withColumn("row",row_number.over(window1))
      .where(col("row")===1).withColumn("hour_of_day",col("hour_of_day").cast("int")).withColumn("max_count",col("count").cast("int"))
      .select("hour_of_day","zone","max_count").orderBy(asc("hour_of_day"))
    display(df_brook.limit(24))

    // COMMAND ----------

    // PART 6 - Find which 3 different days of the January, in Manhattan, saw the largest percentage increment in pickups compared to previous day, in the order from largest increment % to smallest increment %.
    // Print the day of month along with the percent CHANGE (can be negative), rounded to 2 decimal places, in number of pickups compared to previous day.
    // Output Schema: day int, percent_change float


    // Hint: You might need to use lag function, over a window ordered by day of month.

    // ENTER THE CODE BELOW
    val df_brook1 = df_filter.join(df2, df_filter.col("PULocationID") === df2.col("LocationID"),"inner")
      .withColumn("day",date_format(col("pickup_datetime"), "DD"))
      .withColumn("Month",date_format(col("pickup_datetime"), "MM"))
      .where(col("Month")==="01" && col("Borough") === "Manhattan")
      .groupBy("day").count()
    val window = Window.orderBy("day")
    val lagCol = lag(col("count"), 1).over(window)
    val lag_df = df_brook1.withColumn("LagCol", lagCol)
    val res_df = lag_df.withColumn("percent_change", ((($"count" - $"LagCol") / $"LagCol") * 100).cast("float"))
      .withColumn("day",col("day").cast("int"))
      .withColumn("percent_change", round(col("percent_change"), 2))
      .select("day", "percent_change").orderBy(desc("percent_change"))
    display(res_df.limit(3))

  }
}
