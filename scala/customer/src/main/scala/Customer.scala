import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.hadoop.conf._
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import java.io.{BufferedReader, InputStreamReader}

object Customer {

  def main(args: Array[String]): Unit = {
    val file = if (args.length == 1) args.head else "Attachment_1639279522.csv"
    // remove .setMaster("local[*]") in below line when running in cluster mode
    val sc = new SparkConf().setAppName("Customer").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)

    val hconf = sparkContext.hadoopConfiguration
    val hdfs = FileSystem.get(hconf)
    val rdd = sparkContext.textFile(file)
    val header = rdd.first()
    val custRdd = rdd.filter(x => x != header && x.trim.nonEmpty)
      .map(line => {
        val a = line.split(",")
        a
      })

    // Task 1: Count the total amount ordered by each customer in Scala using RDD
    val customerSpentRdd = custRdd
      .map(x => (x(0), x(2).toDouble))
      .reduceByKey(_ + _)

    customerSpentRdd.collect().foreach(println)

    // Task 2: Find the customer who spent more
    val topCustomer = customerSpentRdd.reduce((x1, x2) => {
      if (x1._2 > x2._2) x1 else x2
    })._1
    println(s"customer who spent more: $topCustomer")

    // Task 3: Find Top 5 customers based on the amount spent.
    val top5Custs = customerSpentRdd
      .sortBy(-_._2)
      .map(_._1)
      .take(5)
    println("Top 5 customers based on the amount spent:")
    top5Custs.foreach(println)

    // Task 4: Top 5 customers based on the amount spent:
    val bottom5Custs = customerSpentRdd
      .sortBy(_._2)
      .map(_._1)
      .take(5)
    println("Bottom 5 customers based on the amount spent:")
    bottom5Custs.foreach(println)


    // Task 5: Create a function to give rewards ($5) to customers who spent more high (Top 5)
    val rewardRdd = rewardCustomers(customerSpentRdd, top5Custs)


    // Task 6:  Find out customers who spent an average amount of the total amount spent in data
    // assumed more than average spent
    val totalSpent = rewardRdd.map(_._2).sum()
    val average = totalSpent / rewardRdd.count()
    val avgSpentRdd = rewardRdd
      .filter(x => x._2 > average)
      .sortBy(-_._2)
      .map(_._1)
    println("customers who spent an average amount of the total amount spent in data:")
    avgSpentRdd.foreach(println)


    // Task 7: Sort the customers based on the amount spent (high to low)
    val custsSorted = rewardRdd
      .sortBy(-_._2)
      .map(_._1)
    println("Sort the customers based on the amount spent (high to low):")
    custsSorted.collect().foreach(println)

    // Task 8: list the product ID's of Top 5 customers who have purchased
    val custProdRdd = custRdd.map(x => (x(0), x(1)))
    val prodIds = custProdRdd
      .filter(x => top5Custs.contains(x._1))
      .map(_._2)
      .distinct()
    println("List of product ID's of Top 5 customers who have purchased:")
    prodIds.foreach(println)

    // Task 9: list most sold product ID's
    val prodIdsRdd = custRdd
      .map(x => (x(1), 1L))
      .reduceByKey(_ + _)
      .sortBy(-_._2)

    val top = prodIdsRdd.first()
    val topSoldProductIds = prodIdsRdd
      .filter(x => x._2 == top._2)
      .map(_._1)
      .collect()
    println("list most sold product ID's")
    topSoldProductIds.foreach(println)

    sparkContext.stop()
  }

  def rewardCustomers(rdd: RDD[(String, Double)], top5Custs: Array[String]): RDD[(String, Double)] = {
    val top5 = rdd
      .filter(x => top5Custs.contains(x._1))
      .map(x => (x._1, x._2 - 5))
    val rest = rdd.filter(x => !top5Custs.contains(x))
    top5.union(rest)
  }
}
