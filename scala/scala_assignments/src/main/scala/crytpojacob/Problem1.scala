package crytpojacob

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, Row, SQLContext}

import java.sql.{DriverManager, ResultSet}
import scala.collection.mutable
import org.apache.spark.sql.types.{StringType, StructField, StructType}

object Problem1 {

  class JDBCConnector(driver: String, url: String) {
    Class.forName(driver)
    private val connection = DriverManager.getConnection(url)

    def toDataFrame(sqlContext: SQLContext, query: String): DataFrame = {
      import sqlContext.implicits._
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(query)
      val records = mutable.ArrayBuffer[Row]()
      val md = resultSet.getMetaData
      val columns = for (i <- 1 to md.getColumnCount) yield md.getColumnName(i)
      val schema = StructType(columns.map(x => StructField(x, StringType)))
      while (resultSet.next()) {
        val row = for (i <- 1 to md.getColumnCount) yield resultSet.getString(i)
        records.append(Row.fromSeq(row))
      }
      sqlContext.createDataFrame(sqlContext.sparkContext.parallelize(records), schema)
    }

    def writeTo(sqlContext: SQLContext, query: String, format: String, outputLocation: String): Unit = {
      val df = toDataFrame(sqlContext, query)
      format.toLowerCase match {
        case "csv" =>
          val sc = df.schema
          val rdd = df.rdd.map(x => Row.fromSeq(x.toSeq.map(x => if (x == null) x else x.toString.replaceAll("\r\n", "|new line|").replaceAll("\n", "|new line|"))))
          val dfNew = sqlContext.createDataFrame(rdd, sc)
          dfNew.coalesce(1).write.mode("overwrite").option("header","true").csv(outputLocation)
        case "parquet" =>
          df.write.mode("overwrite").parquet(outputLocation)
        case _ => throw new Exception(s"Unsupported format: $format. Supported formats are CSV and PARQUET.")
      }
    }

    def close(): Unit = {
      connection.close()
    }
  }


  val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
  val sparkContext = new SparkContext(sc)
  val sqlContext = new SQLContext(sparkContext)
  import sqlContext.implicits._

  val driver = "com.netsuite.jdbc.openaccess.OpenAccessDriver"
  val url = ""//dbutils.secrets.get(scope = "NetSuite", key = "url")

  val query = """
     SELECT id as Id
      ,entityid as ProjectId
      ,companyname as ProjectName
      ,customer as CustomerId
      ,BUILTIN.DF(customer) AS Customer
      ,BUILTIN.DF(entitystatus) as Status
      ,BUILTIN.DF(jobtype) AS TYPE
      ,custentity1 as DescriptionFromSalesOrder
      ,timeremaining as RemainingWork
      ,calculatedstartdate as CalculatedStartDate
      ,scheduledenddate as ScheduledEndDate
    FROM job """
  val jdbc = new JDBCConnector(driver, url)
  // val df = jdbc.toDataFrame(sqlContext, query)
  // println(df.count())
  //df.show()
  // val df = jdbc.toDataFrame(sqlContext, query)
  // println(df.count())
  //df.show()
  val outputFolder = "abfss://raw@oedatalake.dfs.core.windows.net/tmp/parquet" // "/dbfs/FileStore/tmp/parquet2"
  jdbc.writeTo(sqlContext, query, "parquet", outputFolder)

  // val outputFolder1 = "abfss://raw@oedatalake.dfs.core.windows.net/tmp/csv1" // "/dbfs/FileStore/tmp/parquet2"
  // jdbc.writeTo(sqlContext, query, "csv", outputFolder1)

  // val csvDf = spark.read.csv(outputFolder1)
  // csvDf.show()

  val testDf = sqlContext.read.parquet(outputFolder)
  // testDf.show()
  jdbc.close()
}