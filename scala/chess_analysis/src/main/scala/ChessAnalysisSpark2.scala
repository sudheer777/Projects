import ChessAnalysisSpark._
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.spark.{SparkConf, SparkContext}

object ChessAnalysisSpark2 {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Chess Analysis1")
    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")
    val filePath = if (args.length > 0) args(0) else "file:///Users/sudheer/projects/scala/chess_analysis/first100_output.csv"
    val outputPath = if (args.length > 1) args(1) else "file:///Users/sudheer/projects/scala/chess_analysis/first100_output_2.csv"

    val fileHeader = Array("opening", "total", "wins", "draws", "losses")
    val headerMap = fileHeader.zipWithIndex.toMap
    val chessRdd = sc.parallelize(Seq(filePath))
    val openingRdd = chessRdd.flatMap(file => {
      val rows = new CSVSource(file, new CSVSourceOptions(skipHeader = false, format = "UTF-8")).iterator
      rows.map(r => {
        val opening = r(headerMap("opening"))
        val winCount: Long = r(headerMap("wins")).toLong
        val lossCount: Long = r(headerMap("losses")).toLong
        val drawCount: Long = r(headerMap("draws")).toLong
        val totalCount: Long = r(headerMap("total")).toLong
        (opening, CountMetrics(winCount, lossCount, drawCount, totalCount))
      })
    })

    val totalPlays = openingRdd.map(_._2.total).reduce(_ + _)
    println(s"Total plays: $totalPlays")

    val openingAgg = openingRdd.map(x => (x._1, x._2.globalUsedPercent(totalPlays))).collect()
      .sortBy(x => (-x._2.winPercent, -x._2.globalUsedPct, x._1))

    val result = openingAgg.map(x => Array(x._1, x._2.total, x._2.wins, x._2.draws, x._2.losses, f"${x._2.globalUsedPct}%1.2f", f"${x._2.winPercent}%1.2f"))

    val csvWriter = new CSVRecordWriter(FileSystem.get(new Configuration()).create(new Path(outputPath)), textQualifier = "\"")
    val outputHeader: Array[Any] = Array("opening", "total_times_used", "win_count", "draw_count", "loss_count",
      "total_times_used_to_total_games_percent", "win_count_to_total_times_used_percent")
    csvWriter.append(outputHeader)
    result.foreach(x => csvWriter.append(x))
    csvWriter.close()
    if (!sc.isStopped) sc.stop()
  }
}
