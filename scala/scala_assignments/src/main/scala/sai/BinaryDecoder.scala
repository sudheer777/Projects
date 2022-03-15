package sai

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

object BinaryDecoder {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)
    import sqlContext.implicits._

    val df = sqlContext.read.parquet("file:/Users/sudheerpendyala/Downloads/part-m-00019.parquet")
    df.show(10)
    df.printSchema()
    val columnMap = df.columns.zipWithIndex.toMap
    val rdd = df.rdd.map(row => (row.getString(columnMap("EVENT_ID")),
      (row.getString(columnMap("BLOB_SEQ_NUM")).toInt, row.getAs[Array[Byte]](columnMap("BLOB_CONTENTS")))))
      .groupByKey()
      .mapValues(x => {
        x.toList.sortBy(_._1).map(y => y._2.map(_.toChar).mkString).mkString("||")
      })
    rdd.take(10).foreach(println)
  }
}
