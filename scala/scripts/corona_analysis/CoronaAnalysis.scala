import java.io.{BufferedReader, InputStreamReader}

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import org.apache.spark.{SparkConf, SparkContext}

object CoronaAnalysis {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("args usage: file:///<file_full_path>")
    }

    val conf: SparkConf = new SparkConf().setAppName("Medicx Event Parser").setMaster("local")
    val inputFileLocation: String = args(0) // Input file should be given as first argument for this code. example file:///<path in your laptop>
    val fileSystem = getFileSystem(inputFileLocation) // create file system for the input file

    val sparkContext: SparkContext = SparkContext.getOrCreate(conf) // create spark context which will be used to construct RDD

    val inputHeader = Array("date", "location", "new_cases", "new_deaths", "total_cases", "total_deaths") // input header of full_data.csv
    val headerMap = inputHeader.zipWithIndex.toMap // created header name to index map which can be used while accessing RDD
    val inputData = readCSVFileLines(fileSystem, inputFileLocation, skipHeader = true).toSeq // Load input data into Seq[String]
    val inputTransData = inputData.map(_.split(",")).filter(row => row(headerMap("location")) != "World") // split each row and filter all the rows which has World in location for our analysis
    val inputRdd = sparkContext.parallelize(inputTransData) // Create RDD from the filtered input Array

    countryAnalysis(inputRdd, headerMap) // Call countryAnalysis function which does country wide analysis
    dateAnalysis(inputRdd, headerMap) // Call dateAnalysis function which does date wise analysis

    sparkContext.stop() // Stop spark context once job is done
  }

  def dateAnalysisByCountry(rdd: RDD[Array[String]], headerMap: Map[String, Int], metric: String): Map[String, (String, Long)] = {
    val intRdd = rdd.map(row => (row(headerMap("date")), (row(headerMap("location")), row(headerMap(metric)).toLong))) // Create new RDD with (date and tupple of location and given metric)

    // Reduce function which is used for reduceBY rdd
    def reduce(val1: (String, Long), val2: (String, Long)): (String, Long) = {
      if (val1._2 > val2._2) val1 else val2
    }

    val redRdd = intRdd.reduceByKey(reduce) // ReduceBy for performing analysis
    val result = redRdd.collect() // Collect results for the analysis done
    result.toMap
  }

  def dateAnalysis(rdd: RDD[Array[String]], headerMap: Map[String, Int]): Unit = {

    val newCaseMap =  dateAnalysisByCountry(rdd, headerMap, "new_cases")
    val newDeathMap =  dateAnalysisByCountry(rdd, headerMap, "new_deaths")
    val totCaseMap = dateAnalysisByCountry(rdd, headerMap, "total_cases")
    val totDeathMap = dateAnalysisByCountry(rdd, headerMap, "total_deaths")
    val dates = newCaseMap.keys.toSeq.sorted

    println("------------Date wise analysis started---------------")

    dates.foreach(x => {
      println(s"---- Date: $x ------")

      val maxDateNewCase = newCaseMap(x)
      if (maxDateNewCase._2 > 0) println(s"Maximum new cases happened in ${maxDateNewCase._1} with cases ${maxDateNewCase._2}")
      else println("No new cases happened")
      val maxDateNewDeath = newDeathMap(x)
      if (maxDateNewDeath._2 > 0) println(s"Maximum new deaths happened in ${maxDateNewDeath._1} with cases ${maxDateNewDeath._2}")
      else println("No new deaths happened")
      val maxTotCaseMap = totCaseMap(x)
      if (maxTotCaseMap._2 > 0) println(s"Maximum total cases happened in ${maxTotCaseMap._1} with cases ${maxTotCaseMap._2}")
      else println("No cases are reported till date")
      val maxTotDeathMap = totDeathMap(x)
      if (maxTotDeathMap._2 > 0) println(s"Maximum total deaths happened on ${maxTotDeathMap._1} with cases ${maxTotDeathMap._2}")
      else println("No deaths are reported till date")
      println("")
    })
    println("------------Date wise analysis ended---------------")
  }

  def countryAnalysisByDate(rdd: RDD[Array[String]], headerMap: Map[String, Int], metric: String): Map[String, (String, Long)] = {
    val intRdd = rdd.map(row => (row(headerMap("location")), (row(headerMap("date")), row(headerMap(metric)).toLong)))
    // Reduce function which is used for reduceBY rdd
    def reduce(val1: (String, Long), val2: (String, Long)): (String, Long) = {
      if (val1._2 > val2._2) val1 else val2
    }

    val redRdd = intRdd.reduceByKey(reduce) // ReduceBy for performing analysis
    val result = redRdd.collect() // Collect results for the analysis done
    result.toMap
  }

  def countryAnalysis(rdd: RDD[Array[String]], headerMap: Map[String, Int]): Unit = {
    val intRdd = rdd.map(row => (row(headerMap("location")), (row(headerMap("new_cases")).toLong, row(headerMap("new_deaths")).toLong)))
    // Reduce function which is used for reduceBY rdd
    def reduce(val1: (Long, Long), val2: (Long, Long)): (Long, Long) = {
      (val1._1 + val2._1, val1._2 + val2._2)
    }

    val redRdd = intRdd.reduceByKey(reduce) // ReduceBy for performing analysis
    val result = redRdd.map(row => (row._1, row._2._1, row._2._2)).collect() // Collect results for the analysis done

    val newCaseMap =  countryAnalysisByDate(rdd, headerMap, "new_cases")
    val newDeathMap =  countryAnalysisByDate(rdd, headerMap, "new_deaths")

    println("------------Country wide analysis started---------------")

    result.sortBy(_._1).foreach(x => {
      println(s"---- Country: ${x._1} ------")
      println(s"Total cases reported: ${x._2}")
      println(s"Total deaths reported: ${x._3}")
      val mortalityRate = ((1D * x._3) / x._2) * 100
      println(f"Mortality rate in percentage: $mortalityRate%1.2f")
      val maxDateNewCase = newCaseMap(x._1)
      println(s"Maximum new cases happened on ${maxDateNewCase._1} with cases ${maxDateNewCase._2}")
      val maxDateNewDeath = newDeathMap(x._1)
      println(s"Maximum new deaths happened on ${maxDateNewDeath._1} with cases ${maxDateNewDeath._2}")
      println("")
    })
    println("------------Country wide analysis ended---------------")
  }

  def getFileSystem(path: String): FileSystem = {
    val hconf = new Configuration() // initialize new hadoop configuration
    new Path(path).getFileSystem(hconf) // get new filesystem to takle data
  }

  def readCSVFileLines(fileSystem: FileSystem, inputFile: String, skipHeader: Boolean): Iterator[String] = {
    val inpFile = fileSystem.open(new Path(inputFile)) // open file
    val inputData = new BufferedReader(new InputStreamReader(inpFile, "UTF-8")) // read file to buffer stream
    if (skipHeader) inputData.readLine() // skip header line
    new Iterator[String] { // create iterator which has file data inside it in each row
      private var line = inputData.readLine()

      override def hasNext: Boolean = {
        line != null
      }

      override def next(): String = {
        val result = line
        line = inputData.readLine()
        if (line == null) {
          inputData.close()
        }
        result
      }
    }
  }

}