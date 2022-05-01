package jamesjj

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{Dataset, SQLContext}

object Assignment2 {
  case class AccountData()

  case class AddressData(
                          addressId: String,
                          customerId: String,
                          address: String,
                          number: Option[Int],
                          road: Option[String],
                          city: Option[String],
                          country: Option[String]
                        )

  case class Customer(
                       customerId: String,
                       forename: String,
                       surname: String,
                       //Accounts for this customer
                       accounts: Seq[AccountData],
                       //Addresses for this customer
                       address: Seq[AddressData]
                     )

  case class CustomerCountry(
                              customerId: String,
                              forename: String,
                              surname: String,
                              //Accounts for this customer
                              accounts: Seq[AccountData],
                              //Addresses for this customer
                              address: Seq[AddressData],
                              isAustralia: Boolean
                            )

  def convertToCustomerCountry(c: Customer): CustomerCountry = {
    val isAustralia = c.address.exists(x => x.country.getOrElse("") == "Australia")
    CustomerCountry(c.customerId, c.forename, c.surname, c.accounts, c.address, isAustralia)
  }

  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val spark = new SQLContext(sparkContext)

    import spark.implicits._

    val customerDF = spark.read.parquet("src/main/resources/customer.parquet")
    val customerDS: Dataset[Customer] = customerDF.as[Customer]

    //customerDS.printSchema()
    val customerCountry = customerDS.map(convertToCustomerCountry).orderBy("customerId")
  }

}
