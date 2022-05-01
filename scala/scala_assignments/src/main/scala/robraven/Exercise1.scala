package robraven

import org.apache.spark.sql.types.{IntegerType, StringType, StructField, StructType}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{Row, SQLContext}

import scala.util.Random

object Exercise1 {
  case class RowGenerator(attributeCount: Int) {
    private var customerId: Long = 0
    private val rand = new Random()

    private def getAttributes(): Seq[Int] = {
      (0 until attributeCount).map(x => rand.nextInt(9) + 1)
    }

    def generateSchema(): StructType = {
      StructType(
        StructField("", StringType) +: (0 until attributeCount).map(x => StructField(s"attribute${x+1}", IntegerType))
      )
    }

    def generate(): Row = {
      customerId += 1
      val cust = s"abc#$customerId"
      Row.fromSeq(cust +: getAttributes())
    }

    def generate(n: Int): Seq[Row] = {
      (0 until n).map(x => generate())
    }
  }

  def main(args: Array[String]): Unit = {
    val rowCount = args(0).toInt
    val columnCount = args(1).toInt
    val format = args(2)
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)
    val gen = RowGenerator(columnCount)
    val rdd = sparkContext.parallelize(gen.generate(rowCount))
    val df = sqlContext.createDataFrame(rdd, gen.generateSchema())
    // df.show()

    // write to parquet file
    format.toLowerCase() match {
      case "csv" =>
        df.coalesce(1).write
          .option("header", "true")
          .csv("src/main/scala/robraven/csv2/")
      case "parquet" =>
        df.coalesce(1).write.parquet("src/main/scala/robraven/parquet1/")
    }
  }
}
