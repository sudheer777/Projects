import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, Row, SQLContext}
import org.apache.spark.sql.types.{StringType, StructField, StructType}

import scala.collection.mutable


object DemographicAnalysis {
  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)

  val inputHeader = Array("RecordID", "Country", "Gender", "Demographics Question", "Demographics Response", "Question", "Survey Year", "Value") // input header
  val inputSchema = StructType(inputHeader.map(x => StructField(x, StringType)))
  val demographicQuestionSet = Set("Education", "Employment", "Marital status", "Residence", "Age")
  val surveyAnswersSet = Set("... if she argues with him", "... for at least one specific reason", "... if she goes out without telling him",
    "... if she neglects the children", "... if she burns the food", "... if she refuses to have sex with him")

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("args usage: file:///<file_full_path>")
    }

    val file = "file:///Users/sudheer/Downloads/makeovermonday-2020w10/20200306 Data International Women's Day Viz5 Launch.csv"

    val conf: SparkConf = new SparkConf().setAppName("Medicx Event Parser").setMaster("local")

    // val inputFileLocation: String = args(0) // Input file should be given as first argument for this code. example file:///<path in your laptop>
    // val fileSystem = getFileSystem(inputFileLocation) // create file system for the input file

    val sparkContext: SparkContext = SparkContext.getOrCreate(conf) // create spark context which will be used to construct RDD
    val sQLContext = new SQLContext(sparkContext)

    val headerMap = inputHeader.zipWithIndex.toMap // created header name to index map which can be used while accessing RDD

    val inputDataFrame = loadCSVFiles(sQLContext, Seq(file), inputSchema, skipHeader = true) //readCSVFileLines(fileSystem, inputFileLocation, skipHeader = true).toSeq // Load input data into Seq[String]
    val inputRdd = inputDataFrame.rdd // Create RDD from the filtered input Array

    demographicAnalysis(sparkContext, inputRdd, headerMap)

    sparkContext.stop() // Stop spark context once job is done
  }

  case class DemoDimensions(year: Int, country: String, demoResponse: String, question: String, value: Double)

  def demographicAnalysis(sparkContext: SparkContext, rdd: RDD[Row], headerMap: Map[String, Int]): Unit = {
    val intRdd = rdd.map(row => {
      val surveyYear = row.getString(headerMap("Survey Year")).split("/").last.toInt // get survey year
      val value = row.getString(headerMap("Value"))
      val cvalue = if (value == null || value == "") 0D else value.toDouble
      val inputDims = DemoDimensions(surveyYear, row.getString(headerMap("Country")), row.getString(headerMap("Demographics Response")),
        row.getString(headerMap("Question")), cvalue)

      ((row.getString(headerMap("Gender")), row.getString(headerMap("Demographics Question"))), inputDims)
    })

    val groupedRdd = intRdd.groupByKey
    val group = groupedRdd.collect().sortBy(_._1._2)

    println("------------Gender demo question wide analysis started---------------")
    for {
      ((gender, demoQuestion), iterator) <- group
      iterSeq = iterator.toSeq
      iterRdd = sparkContext.parallelize(iterSeq)
    } {
      val educationRdd = iterRdd
      val modRdd = educationRdd.map(row => ((row.year, row.country, row.question), (row.demoResponse, row.value)))
      def reduce(val1: (String, Double), val2: (String, Double)): (String, Double) = {
        if (val1._2 > val2._2) val1 else val2
      }
      val resRdd = modRdd.reduceByKey(reduce)
        .collect()
        .map(row => (row._1._3, row._2._1))

      val quesMap = mutable.Map[String, mutable.Map[String, Long]]()
      resRdd.foreach(row => {
        val question = row._1
        val demoResponse = row._2
        val exVal = quesMap.getOrElseUpdate(question, mutable.Map[String, Long]())
        val dVal = exVal.getOrElse(demoResponse, 0L)
        exVal(demoResponse) = dVal + 1
        quesMap(question) = exVal
      })

      println(s"----- Gender: $gender, demograhic Question: $demoQuestion -------")
      val questions = quesMap.keys.toSeq.sorted
      questions.foreach(question => {
        println(s"    $question")
        println(s"    -------------------")
        val demresMap = quesMap(question)
        val demos = demresMap.keys.toSeq.sorted
        val total = demresMap.values.sum
        demos.foreach(demo => {
          val times = demresMap(demo)
          val percent = (100D * times)/total
          println(s"        '$demo': is the segment which appeared $times out of total $total ($percent %) as maximum across all countries and surveys")
        })
      })
    }
    println("------------Gender demo question analysis ended---------------")
  }

  def getFileSystem(path: String): FileSystem = {
    val hconf = new Configuration() // initialize new hadoop configuration
    new Path(path).getFileSystem(hconf) // get new filesystem to takle data
  }

  def loadCSVFiles(sqlContext: SQLContext, inputFiles: Seq[String], schema: StructType, delimiter: String = ",",
                   textQualifier: String = "\"", skipHeader: Boolean = false, escape: String = "\"", multiLine: Boolean = false): DataFrame = {
    sqlContext.read.format("com.databricks.spark.csv").option("delimiter", delimiter)
      .option("header", skipHeader.toString).option("quote", textQualifier).option("escape", escape).option("multiLine", multiLine.toString)
      .schema(schema).load(inputFiles: _*)
  }
}