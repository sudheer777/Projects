package spark_sessions

import org.apache.spark.sql.functions.udf
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StringType, StructField, StructType}
import org.apache.spark.{SparkConf, SparkContext}

object SparkBasic {

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
   */

  def main(args: Array[String]): Unit = {
    println("Started..")
    // we need to initialize spark context and sql context
    // spark context is generally used to create RDDs
    // sql context is to create Dataframes and also run spark sqls
    // RDD vs Dataframe Vs spark sql
    // big data formats: AVRO(Row optimized), PARQUET(columnar optimized)

    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    val schema = StructType(Seq(
      StructField("name", StringType),
      StructField("age", IntegerType),
      StructField("gender", StringType),
      StructField("salary", DoubleType)
    ))
    val df = sqlContext.read.option("header", "true").schema(schema).csv("session1.csv").cache()
    df.show()
    df.printSchema()

    val resDf = dataframeOperations(df).cache()

    resDf.show()

    /**
     * +------+---------+-----------+
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
    val finalDf = resDf.groupBy("gender", "age_group").avg("salary") // Dataframe operations
    finalDf.show()


    // Use spark sql to solve same problem
    /**
     * +------+---------+--------------+
      |gender|age_group|average_salary|
      +------+---------+--------------+
      |  male|   [0-25]|         110.0|
      |  male|  [26-40]|         160.0|
      |female|   [0-25]|         110.0|
      |female|    [60+]|          85.0|
      |female|  [26-40]|        160.04|
      |  male|    [60+]|          93.0|
      |  male|  [41-59]|         260.0|
      |female|  [41-59]|         230.0|
      +------+---------+--------------+
     */
    resDf.registerTempTable("table1") // register dataframe as temp table and can write spark sql queries on top of this table to create another Dataframe
    val finalDf1 = sqlContext.sql("select gender, age_group, avg(salary) average_salary from table1 group by gender, age_group")
    finalDf1.show()


  }

  def dataframeOperations(df: DataFrame): DataFrame = {
    val ageGroupComputation = (age: Int) => {
      if (age <= 25) {
        "[0-25]"
      } else if (age <= 40 ) {
        "[26-40]"
      } else if (age <= 59) {
        "[41-59]"
      } else {
        "[60+]"
      }
    }
    val convertUdf = udf(ageGroupComputation) // user defined functions
    df.withColumn("age_group", convertUdf(df("age")))
  }
}
