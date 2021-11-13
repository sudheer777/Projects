import java.io.{BufferedReader, InputStreamReader}
import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.PrintWriter

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}

object Customer {
  val dateFormat = new SimpleDateFormat("yyyyMMdd")

  case class Revenue(custKey: String, skuNum: String, date: String, revenue: Double)

  def main(args: Array[String]): Unit = {

    val inputFile = "file:///Users/sudheer/Downloads/rescalaasigment/data_table.csv" // give full input path here
    val outputFile = "/Users/sudheer/Downloads/rescalaasigment/report.csv" // output file location where report.csv will be produced
    val fileSystem = getFileSystem(inputFile) // get input file's file system to read the file
    val inputData = readCSVFileLines(fileSystem, inputFile, skipHeader = true).toSeq // Read input file into a Seq with one line per row
    val filtinp = inputData.filter(x => x.nonEmpty) // Consider only non empty rows as empty rows are not usefull
      .map(x => x.split(",")) // Splitting rows by , as this is CSV file
      .map(x => Revenue(x(6), x(5), x(0), x(7).toDouble)) // We only use 4 columns to compute end result, so just maintaning them in a case class

    val writer = new PrintWriter(new File(outputFile)) // Create output writer
    writer.write("Sales Date,SHIP_TO_CUST_KEY,SKU Number,EXT_NET_SLS_PMAR_AMT,Lookback Date,Average Lookback $ for the Cust/SKU,Credit Date,Average Credit $ for the Cust/SKU\n") // Header for output CSV file
    filtinp.foreach(x => {
      val (back90, avgRevenue1) = back90Average(filtinp, x) // Compute average revenue for look back 90 days group by customer key and sku number
      val (next30, avgRevenue2) = next30Average(filtinp, x) // Compute average revenue for next 31 days group by customer key and sku number
      writer.write(s"${x.date},${x.custKey},${x.skuNum},${x.revenue},${back90},${avgRevenue1},${next30},${avgRevenue2}\n") // Write row to output csv file
    })
    writer.close() // close the writer
  }

  /**
    *
    * @param filtinp input loaded in seq
    * @param revenue current row on which we want to perform analysis
    * @return
    */
  def next30Average(filtinp: Seq[Revenue], revenue: Revenue): (String, Double) = {
    val ts = getTimestamp(revenue.date) // get timestamp for current row's date
    val next30 = getDate(ts + 1L * 31 * 24 * 60 * 60 * 1000) // get date for next 31 date
    // filter the entire input which has same customer key, sku number, and date lies from current to next 31 days
    val fin = filtinp.filter(x => x.custKey == revenue.custKey && x.skuNum == revenue.skuNum && x.date > revenue.date && x.date <= next30)
      .map(_.revenue)
    // compute average of all such filtered rows satisfying above condition
    val r = if (fin.isEmpty) 0D else (fin.sum) / fin.length
    // return both next 31 date and average revenue value
    (next30, r)
  }

  /**
    *
    * @param filtinp input loaded in seq
    * @param revenue current row on which we want to perform analysis
    * @return
    */
  def back90Average(filtinp: Seq[Revenue], revenue: Revenue): (String, Double) = {
    val ts = getTimestamp(revenue.date) // get timestamp for current row's date
    val back90 = getDate(ts - 1L * 90 * 24 * 60 * 60 * 1000) // get date for loock back 90 days date
    // filter the entire input which has same customer key, sku number, and date lies from current to last 90 days
    val fin = filtinp.filter(x => x.custKey == revenue.custKey && x.skuNum == revenue.skuNum && x.date < revenue.date && x.date >= back90)
      .map(_.revenue)
    // compute average of all such filtered rows satisfying above condition
    val r = if (fin.isEmpty) 0D else (fin.sum) / fin.length
    // return lookback 90 date and average revenue value
    (back90, r)
  }

  // get timestamp for a given date
  def getTimestamp(date: String): Long = dateFormat.parse(date).getTime

  // get date for given timestamp
  def getDate(timeStampInMills: Long): String = {
    val time = new Date(timeStampInMills)
    dateFormat.format(time)
  }

  // get filesystem for given file path
  def getFileSystem(path: String): FileSystem = {
    val hconf = new Configuration() // initialize new hadoop configuration
    new Path(path).getFileSystem(hconf) // get new filesystem to takle data
  }

  /**
    * @param fileSystem filesystem of file
    * @param inputFile input file full path
    * @param skipHeader true if csv file has header
    * @return Iterator by each row
    */
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
