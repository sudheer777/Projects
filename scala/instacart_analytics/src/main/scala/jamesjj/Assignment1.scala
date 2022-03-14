package jamesjj

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, Dataset, SQLContext}


object Assignment1 {
  case class AddressRawData(
                             addressId: String,
                             customerId: String,
                             address: String
                           )


  //expected output case class
  case class AddressData(
                          addressId: String,
                          customerId: String,
                          address: String,
                          number: Option[Int],
                          road: Option[String],
                          city: Option[String],
                          country: Option[String],
                          isAustralian: Boolean
                        )

  def parser(address: AddressRawData): AddressData = {
    val split = address.address.split(", ")
    val country = split(3)

    AddressData(address.addressId, address.customerId, address.address,
      Some(split(0).toInt), Some(split(1)), Some(split(2)), Some(country), country.toLowerCase == "australia")
  }

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    import sqlContext.implicits._

    val addressDF: DataFrame = sqlContext.read.option("header", "true").csv("file:/Users/sudheerpendyala/Downloads/address_data.csv")//.csv("src/main/resources/address_data.csv")

    addressDF.show(false)
    val addressDS: Dataset[AddressRawData] = addressDF.as[AddressRawData]
    val addressDsEnh = addressDS.map(x => parser(x))
    val ausCount = addressDsEnh.filter(_.isAustralian).count()
    println(s"Australian count: $ausCount")
    addressDsEnh.toDF().show(100, false)
  }

}
