package spark_sessions

import org.apache.spark.sql.functions.udf
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StringType, StructField, StructType}

object SparkSession1 {

  /**
   * PROBLEM STATEMENT:
   *
   * we have a CSV file with 4 columns, name, age, gender, salary
   * we have to find out average salary for each gender and age group (divided into 4 groups, [0-25], [26-40], [41-59], [60+])
   *
   * input file sample:
   * name,age,gender,salary
   * t1,20,male,100
   * t4,21,male,110
   * t2,34,female,200
   *
   *
   * output should look like:
   * male, [0-25], 120
   * female, [0-25], 130
   * male, [26-40], 190
   * female, [26-40], 170
   * male, [41-59], 220
   * female, [41-59], 200
   * male, [60+], 120
   * female, [60+], 100
   *
   *
   * EXERCISE:
   * ---------
   * Please understand concepts mentioned in this file
   * Setup this project instacart_copy in intellij and add this file and try to run thi from intellij
   * Add this file to existing Git hub project
   * Try to solve additional problem given in session1 in spark as well
   * input file will look like:
   * 3 event types are possible: imp, cnv,clk
   * date is of format yyyy-MM-dd
   * event_type,date,count
   * imp,2021-10-12,120
   * imp,2021-10-10,100
   * cnv,2021-10-12,100
   * clk,2021-10-12,20
   * imp,2022-01-20,300
   * cnv,2022-01-20,100
   * clk,2022-01-20,199
   *
   * output should be like
   * for each year and event_type, need to print total count
   * 2021,imp,220
   * 2021,cnv,122
   * 2021,clk,100
   * 2022,imp,100
   */
  def main(args: Array[String]): Unit = {
    // we require spark context and sql context to be initialised
    // spark context is generally used to create RDDs
    // sql context is used for creating dataframes and also running sqls on top of them

    val sc = new SparkConf().setAppName("Test")
      .setMaster("local[*]") // if you are running in cluster mode then you need to remove
    val sparkContext = new SparkContext(sc) // spark context
    val sqlContext = new SQLContext(sparkContext) // sqlContext

    val schema = StructType(Seq(
      StructField("name", StringType),
      StructField("age", IntegerType),
      StructField("gender", StringType),
      StructField("salary", DoubleType)
    ))
    val df = sqlContext.read.option("header", "true").schema(schema).csv("session1.csv") // you can pass lot of options line delimiter, schema, header
    // spark distributes this data across all the executors and run them paralely
    df.show() // will show top 20 rows on output
    df.printSchema() // this will print schema as tree on output

    // Transforming using Dataframes
    val resDf = dataframeOperations(df).cache() // caches the dataframe in memory and avoids recomputation
    // use cache if you are performing more than 1 operation on that Dataframe
    resDf.show(false)

    val finalDf = resDf.groupBy(resDf("gender"), resDf("age_group")).avg("salary") // Dataframe operations
    println("dataframe operations to compute average salary:")
    finalDf.show()

    /**
     *  +------+---------+-----------+
        |gender|age_group|avg(salary)|
        +------+---------+-----------+
        |  male|   [0-25]|      110.0|
        |  male|  [26-40]|      160.0|
        |female|   [0-25]|      110.0|
        |female|    [60+]|       85.0|
        |female|  [26-40]|     160.04|
        |  male|    [60+]|       93.0|
        |  male|  [41-59]|      260.0|
        |female|  [41-59]|      230.0|
        +------+---------+-----------+
     */


    // running spark sqls to solve problem
    resDf.registerTempTable("table1") // register dataframe to some table name so that we can run queries on top of that table
    val finalSqlDf = sqlContext.sql("select gender, age_group, avg(salary) avg_salary from table1 group by gender, age_group")
    println("spark sql to achive same result:")
    finalSqlDf.show()

    /**
     *  +------+---------+----------+
        |gender|age_group|avg_salary|
        +------+---------+----------+
        |  male|   [0-25]|     110.0|
        |  male|  [26-40]|     160.0|
        |female|   [0-25]|     110.0|
        |female|    [60+]|      85.0|
        |female|  [26-40]|    160.04|
        |  male|    [60+]|      93.0|
        |  male|  [41-59]|     260.0|
        |female|  [41-59]|     230.0|
        +------+---------+----------+
     */

  }

  def dataframeOperations(df: DataFrame): DataFrame = {
    val ageGroupComputation = (age: Int) => {
      if (age <= 25) {
        "[0-25]"
      } else if (age <= 40) {
        "[26-40]"
      } else if (age <= 59) {
        "[41-59]"
      } else {
        "[60+]"
      }
    }

    val convertUDf = udf(ageGroupComputation)
    df.withColumn("age_group", convertUDf(df("age")))
  }
}
