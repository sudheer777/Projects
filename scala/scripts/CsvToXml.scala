import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileStatus, Path}
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.types._

import scala.collection.mutable

/**
  * README:
  * 2 inputs are needed for this program
  * filePattern: input file pattern, file:///<full_pattern>
  * outputFile: ful output file path where output should be stored
  *
  * If you want to add new fields to csv, you need to update inputHeader variable with all the columns in order
  * and identify which attribute those new fields belongs to
  * If they belong to financial information, you need to add new members to FinancialInformation class and update toXMLString method
  * Same goes for MemberInformation as well
  * And in analysis method you need to construct these class objects with all those extra information and after groupbyKey if you think something is array of nested ypu might have to aggregate them
  * Regarding fileInformation you need to figure out where those additional fields are comming from. If they are comming from filename, you need to to pattern extraction and add them to FileInformation class
  */

object CsvToXml {
  val inputHeader = Array("policy_id", "IssuerAssignedMemberTraceID", "memberfirstname", "memberlastname", "address_type", "addressline1", "addressline2", "zipcode", "financial_start_date", "financial_end_date") // input header
  val inputSchema = StructType(inputHeader.map(x => StructField(x, StringType)))
  val dateFormat = new SimpleDateFormat("yyyyMMdd'T'hh:mm:ss")

  // case class for financial information. extra information realted to finance should be added as member for this function
  case class FinancialInformation(startDate: String, endDate: String) {
    // XML string for financial information
    // if additional member is added we need to modify this with additional field
    def toXMLString: String = {
      s"""<FinancilaInformation>
         |  <FinancialEffectiveStartDate>$startDate</FinancialEffectiveStartDate>
         |  <FinancialEffectiveEndDate>$endDate</FinancialEffectiveEndDate>
         |</FinancilaInformation>""".stripMargin
    }
  }

  // case class for address.
  case class Address(adressType: String, addressLine1: String, addressLine2: String, zipcode: String) {
    def toXMLString: String = {
      s"""<AddressInformation>
         |  <AddressType>$adressType</AddressType>
         |  <AddressLine1>$addressLine1</AddressLine1>
         |  ${if (!(addressLine2 == null || addressLine2.isEmpty)) s"<AddressLine2>$addressLine2</AddressLine2>\n<ZipCode>$zipcode</ZipCode>" else s"<ZipCode>$zipcode</ZipCode>" }
         |</AddressInformation>
       """.stripMargin
    }
  }

  // case class for member information. extra information realted to member should be added as member for this function
  case class MemberInformation(issuerAssignedMemberTraceID: String, memberFirstName: String, memberLastName: String,
                               addresses: mutable.Set[Address], financialInformations: mutable.Set[FinancialInformation]) {

    // XML string for member information
    // if additional member is added we need to modify this with additional field
    def toXMLString: String = {
      val ads = addresses.map(x => "  - " + indentString(x.toXMLString, " " * 4)).mkString("\n")

      s"""<MemberInformation>
         |  <IssuerAssignedMemberTraceID>$issuerAssignedMemberTraceID</IssuerAssignedMemberTraceID>
         |  <MemberFirstName>$memberFirstName</MemberFirstName>
         |  <MemberLastName>$memberLastName</MemberLastName>
         |$ads
         |</MemberInformation>
       """.stripMargin
    }
  }

  case class FileInformation(fileId: String, fileCreateDateTime: String, var totalPolicies: Long) {
    def toXMLString: String = {
      s"""<FileInformation>
         |  <FileId>$fileId</FileId>
         |  <FileCreateDateTime>$fileCreateDateTime</FileCreateDateTime>
         |  <TotalPolicies>$totalPolicies</TotalPolicies>
         |</FileInformation>
       """.stripMargin
    }
  }

  def indentString(s: String, indent: String): String = {
    s.lines.zipWithIndex.map(x => if (x._2 == 0) x._1 else indent + x._1).mkString("\n")
  }

  def main(args: Array[String]): Unit = {

    if (args.length != 4) {
      throw new Exception("Usage of args: file:///<full_input_pattern> <output_full_path> <\t or ,> <true or false based on header exists>")
    }
    val filePattern = args(0) // format: file:///<full_path> (supports pattern as well)
    val outputFile = args(1) // format: full path
    val delimiter =  args(2) // "\t" for TSV and "," for CSV
    val hasHeader = args(3).toBoolean // true if header is present in file else false

    val conf: SparkConf = new SparkConf().setAppName("Medicx Event Parser").setMaster("local") // config for spark
    val sparkContext: SparkContext = SparkContext.getOrCreate(conf) // create spark context which will be used to construct RDD
    val sQLContext = new SQLContext(sparkContext) // create sql context to create dataframe

    val files = listFiles(filePattern)
    val writer = new PrintWriter(new File(outputFile))
    writer.write("""  <FCA xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">""")
    for (file <- files) {
      val inputDataFrame = loadCSVFiles(sQLContext, Seq(file.getPath.toString), inputSchema, skipHeader = hasHeader, delimiter = delimiter)
      val policies = analysis(inputDataFrame)
      val fileInformation = FileInformation(file.getPath.getName, getDate(file.getModificationTime), policies.length)
      writer.write(finalOutput(policies, fileInformation))
    }
    writer.write("\n  </FCA>")
    writer.close()
    println("SUCCESS!!")
  }

  // Final output
  def finalOutput(policies: Array[String], fileInformation: FileInformation): String = {
    val p = policies.map(x => "       - " + indentString(x, " " * 8)).mkString("\n")
    s"""
       |    - ${indentString(fileInformation.toXMLString, " " * 6)}
       |    - <CarrierEnrollments>
       |$p
       |      </CarrierEnrollments>""".stripMargin
  }

  def analysis(df: DataFrame): Array[String] = {
    // get header map to make use in rdd level operations
    val headerMap = inputHeader.zipWithIndex.toMap

    // Contruct RDD with tuple having policy_id as key and rest of the member information as case class MemberInformation
    // do a groupByKey operation so that we get all information
    val polRdd = df.rdd.map(row => {
      val address = Address(row.getString(headerMap("address_type")).trim, row.getString(headerMap("addressline1")).trim,
        row.getString(headerMap("addressline2")).trim, row.getString(headerMap("zipcode")).trim)
      val financialInformation = FinancialInformation(row.getString(headerMap("financial_start_date")).trim, row.getString(headerMap("financial_end_date")).trim)

      (row.getString(headerMap("policy_id")).trim,
        MemberInformation(row.getString(headerMap("IssuerAssignedMemberTraceID")).trim, row.getString(headerMap("memberfirstname")).trim,
          row.getString(headerMap("memberlastname")).trim, mutable.Set(address), mutable.Set(financialInformation)))
    }).groupByKey()

    // get all policies and its xml information with all members inside it
    polRdd.mapPartitions(partition => {
      partition.map(k => {
        val (policyId, members) = k
        val memberMap = mutable.HashMap[String, MemberInformation]()
        val financialMap = mutable.HashSet[FinancialInformation]()

        for (mem <- members) {
          financialMap ++= mem.financialInformations
          val memInfo = memberMap.get(mem.issuerAssignedMemberTraceID)
          if (memInfo.nonEmpty) {
            val mi = memInfo.get
            mem.financialInformations ++= mi.financialInformations
            mem.addresses ++= mi.addresses
            memberMap(mem.issuerAssignedMemberTraceID) = mem
          } else memberMap(mem.issuerAssignedMemberTraceID) = mem
        }

        (policyId, s"""<Policy>
                      |  <IssuerAssignedPolicyTraceId>$policyId</IssuerAssignedPolicyTraceId>
                      |${memberMap.values.toArray.sortBy(_.issuerAssignedMemberTraceID).map(x => " - " + indentString(x.toXMLString, " " * 4)).mkString("\n")}
                      |${financialMap.toSeq.sortBy(_.startDate).map(x => " - " + indentString(x.toXMLString, " " * 4)).mkString("\n")}
                      |</Policy>
         """.stripMargin)
      })
    }).collect().sortBy(_._1).map(_._2)
  }

  def getDate(timeStampInMills: Long): String = {
    val time = new Date(timeStampInMills)
    dateFormat.format(time)
  }

  def listFiles(pattern: String): Array[FileStatus] = {
    val fs = new Path(pattern).getFileSystem(new Configuration()) // get new filesystem to takle data
    fs.globStatus(new Path(pattern)).filter(_.isFile)
  }

  // load csv file into dataframe
  def loadCSVFiles(sqlContext: SQLContext, inputFiles: Seq[String], schema: StructType, delimiter: String = ",",
                   textQualifier: String = "\"", skipHeader: Boolean = false, escape: String = "\"", multiLine: Boolean = false): DataFrame = {
    sqlContext.read.format("com.databricks.spark.csv").option("delimiter", delimiter)
      .option("header", skipHeader.toString).option("quote", textQualifier).option("escape", escape).option("multiLine", multiLine.toString)
      .schema(schema).load(inputFiles: _*)
  }
}