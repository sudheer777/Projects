package robraven

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

object TestRead {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    val df = sqlContext.read.parquet("src/main/scala/robraven/*.parquet")

    df.printSchema()
    df.show(false)
  }
}
