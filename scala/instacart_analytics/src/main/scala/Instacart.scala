import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.functions.{col, lit}
import org.apache.spark.sql.types.{BooleanType, IntegerType, LongType, StringType, StructField, StructType}
import org.apache.spark.{SparkConf, SparkContext}


object Instacart {
  def main(args: Array[String]): Unit = {
    val aisles = "sample_data/aisles.csv"
    val departments = "sample_data/departments.csv"
    val orderProductsPrior = "sample_data/order_products__prior.csv"
    val orderProductsTrain = "sample_data/order_products__train.csv"
    val orders = "sample_data/orders.csv"
    val products = "sample_data/products.csv"
    val sampleSubmission = "sample_data/sample_submission.csv"

    // remove .setMaster("local[*]") in below line when running in cluster mode
    val sc = new SparkConf().setAppName("InstamartAnalytics")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    ////////////////////////
    // Start: loading dataframes
    ////////////////////////
    val aislesDf = sqlContext.read.option("header", "true").csv(aisles)
    // aislesDf.show()

    val departmentsDf = sqlContext.read.option("header", "true").csv(departments)
    // departmentsDf.show()

    val orderProductsPriorDf = sqlContext.read.option("header", "true").csv(orderProductsPrior)
    // orderProductsPriorDf.show()

    val orderProductsTrainDf = sqlContext.read.option("header", "true").csv(orderProductsTrain)
    // orderProductsTrainDf.show()

    val ordersDf = sqlContext.read.option("header", "true").csv(orders)
    // ordersDf.show()

    val productsDf = sqlContext.read.option("header", "true").csv(products)
    // productsDf.show()

    // val sampleSubmissionDf = sqlContext.read.option("header", "true").csv(sampleSubmission)
    // sampleSubmissionDf.show()

    ////////////////////////
    // End: loading dataframes
    ////////////////////////


    ////////////////////////
    // Start: Merge all the data frames based on the common key and create a single DataFrame
    ////////////////////////

    val productsJoinDf = productsDf
      .join(aislesDf, productsDf("aisle_id") === aislesDf("aisle_id"), "left")
      .join(departmentsDf, productsDf("department_id") === departmentsDf("department_id"), "left")
      .select(productsDf("product_id"), productsDf("product_name"), productsDf("aisle_id"), productsDf("department_id"), aislesDf("aisle"), departmentsDf("department"))
    //productsJoinDf.show()

    val orderProductsUnion = orderProductsPriorDf.withColumn("eval_set", lit("prior"))
      .union(orderProductsTrainDf.withColumn("eval_set", lit("train")))
    //orderProductsUnion.show()

    val orderProductsUnionJoin = orderProductsUnion
      .join(productsJoinDf, orderProductsUnion("product_id") === productsJoinDf("product_id"), "left")
      .join(ordersDf, orderProductsUnion("order_id") === ordersDf("order_id") && orderProductsUnion("eval_set") === ordersDf("eval_set"), "left")
      .select(orderProductsUnion("order_id"), orderProductsUnion("product_id"), orderProductsUnion("add_to_cart_order"),
        orderProductsUnion("reordered"), orderProductsUnion("eval_set"), productsJoinDf("product_name"),
        productsJoinDf("aisle_id"), productsJoinDf("department_id"), productsJoinDf("aisle"), productsJoinDf("department"),
        ordersDf("user_id"), ordersDf("order_number"), ordersDf("order_dow"), ordersDf("order_hour_of_day"), ordersDf("days_since_prior_order")).cache()
    // orderProductsUnionJoin.show()

    ////////////////////////
    // End: Merge all the data frames based on the common key and create a single DataFrame
    ////////////////////////

    orderProductsUnionJoin.registerTempTable("order_products_final")

    ////////////////////////
    // Start: Check missing data
    ////////////////////////
    val ordersMissing = sqlContext.sql("select * from order_products_final where department = 'missing' or aisle = 'missing'").count()
    println("Missing records for department or aisle: " + ordersMissing)

    ////////////////////////
    // End: Check missing data
    ////////////////////////

    ////////////////////////
    // Start: List the most ordered products (top 10)
    ////////////////////////

    val topProductsOrdered = sqlContext.sql("select product_name, count(*) cnt from order_products_final group by product_name order by cnt desc limit 10").cache()
    topProductsOrdered.show(false)
    val prods = topProductsOrdered.collect().map(_.getString(0))
    println("Top 10 ordered products are: ")
    prods.zipWithIndex.foreach(x => println(s"${x._2 + 1}. ${x._1}"))

    ////////////////////////
    // End: List the most ordered products (top 10)
    ////////////////////////


    ////////////////////////
    // Start: Do people usually reorder the same previous ordered products?
    ////////////////////////

    val users = sqlContext.sql("select count(distinct user_id) from order_products_final").cache()
    val reorderedProds = sqlContext.sql(
      """select count(distinct user_id) from
      (select user_id, product_name, count(*) cnt
      from order_products_final
      group by user_id, product_name) where cnt > 1""").cache()
    reorderedProds.show(false)
    val reProd1 = reorderedProds.collect().head.getLong(0)
    val totUsers = users.collect().head.getLong(0)
    println(s"Number of people reorder the same previous ordered products: ${reProd1} out of total users: $totUsers")

    ////////////////////////
    // End: Do people usually reorder the same previous ordered products?
    ////////////////////////


    ////////////////////////
    // Start: List most reordered products
    ////////////////////////

    val topReorderedProds = sqlContext.sql(
      """select product_name from
      (select product_name, sum(cast(reordered as int)) cnt
      from order_products_final
      group by product_name order by cnt desc limit 10)""").cache()
    topReorderedProds.show(false)
    val reProd = topReorderedProds.collect().map(_.getString(0))
    println("Top 10 List most reordered products are: ")
    reProd.zipWithIndex.foreach(x => println(s"${x._2 + 1}. ${x._1}"))

    ////////////////////////
    // End: List most reordered products
    ////////////////////////


    ////////////////////////
    // Start: Most important department and aisle (by number of products)
    ////////////////////////

    productsJoinDf.registerTempTable("products")
    val depAisleDf = sqlContext.sql(
      """
        select department, aisle from (
          select department, aisle, count(*) cnt from products where department != 'missing' and aisle != 'missing' group by department, aisle order by cnt desc limit 1
          )
        """).cache()
    depAisleDf.show()
    val depAisle = depAisleDf.collect().head
    println(s"Most important department and aisle (by number of products): department: ${depAisle.getString(0)}, aisle: ${depAisle.getString(1)}")

    ///////////////////////
    // End: Most important department and aisle (by number of products)
    ////////////////////////


    ////////////////////////
    // Start: Get the Top 10 departments
    ////////////////////////

    val topDepartments = sqlContext.sql(
      """select department, count(*) cnt
      from order_products_final
      group by department order by cnt desc limit 10""").cache()
    topDepartments.show(false)
    val deps = topDepartments.collect().map(_.getString(0))
    println("Top 10 departments are(by number of orders): ")
    deps.zipWithIndex.foreach(x => println(s"${x._2 + 1}. ${x._1}"))

    ////////////////////////
    // End: Get the Top 10 departments
    ////////////////////////


    ////////////////////////
    // Start: List top 10 products ordered in the morning (6 AM to 11 AM)
    ////////////////////////

    val topProductsOrderedMrng = sqlContext.sql(
    """select product_name, count(*) cnt
      from order_products_final
      where order_hour_of_day >= "06" and order_hour_of_day <= "11"
      group by product_name order by cnt desc limit 10""").cache()
    topProductsOrderedMrng.show(false)
    val prods1 = topProductsOrderedMrng.collect().map(_.getString(0))
    println("Top 10 ordered products in the morning (6 AM to 11 AM) are: ")
    prods1.zipWithIndex.foreach(x => println(s"${x._2 + 1}. ${x._1}"))

    ////////////////////////
    // End: List top 10 products ordered in the morning (6 AM to 11 AM)
    ////////////////////////

    sparkContext.stop()
  }
}
