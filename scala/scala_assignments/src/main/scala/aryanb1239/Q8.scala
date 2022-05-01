package aryanb1239

import org.apache.spark.{SparkConf, SparkContext}

object Q8 {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)

    val cities: Array[String] = Array("Banglore", "Pune", "Mumbai", "Chennai", "Kolkatta", "Kochi", "Delhi", "Blasore", "mysore")
    val rdd = sparkContext.parallelize(cities, 4)

    rdd.glom().collect().foreach(x => {
      println("Start of partition data")
      x.foreach(println)
    })

    val rdd1 = rdd.map(x => (x.head, 1))
    val rdd2 = rdd1.reduceByKey(_ + _)
    rdd2.collect().foreach(println)

    println(rdd2.filter(_._1 == 'B').collect().head)
  }
}
