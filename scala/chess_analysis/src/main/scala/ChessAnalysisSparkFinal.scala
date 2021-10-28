
object ChessAnalysisSparkFinal {
  def main(args: Array[String]): Unit = {
    val filePath = if (args.length > 0) args(0) else "file:///Users/sudheer/projects/scala/chess_analysis/first100.csv"
    val intermediatePath = if (args.length > 1) args(1) else "file:///Users/sudheer/projects/scala/chess_analysis/first100_output.csv"
    val outputPath = if (args.length > 2) args(1) else "file:///Users/sudheer/projects/scala/chess_analysis/first100_output_2.csv"

    ChessAnalysisSpark.main(Array(filePath, intermediatePath))
    ChessAnalysisSpark2.main(Array(intermediatePath, outputPath))
  }
}
