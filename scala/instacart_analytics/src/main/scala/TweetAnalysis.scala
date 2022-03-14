import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SQLContext

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object TweetAnalysis {
  def main(args: Array[String]): Unit = {
    val sc = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sparkContext = new SparkContext(sc)
    val sqlContext = new SQLContext(sparkContext)

    //////////////////////
    // PROBLEM 1 START
    /////////////////////
    val input = "/Users/sudheerpendyala/Downloads/tweets.tsv"
    val df = sqlContext.read.format("csv")
      .option("delimiter", "\t")
      .option("header", "true")
      .option("quote", "\"")
      .option("escape", "\"")
      .option("multiLine", "true")
      .load(input)
      .cache()
    //df.show()
    //////////////////////
    // PROBLEM 1 END
    /////////////////////


    //////////////////////
    // PROBLEM 2 START
    /////////////////////
    df.registerTempTable("tweets")
    val mtdf = sqlContext.sql("select wordle_id, count(*) cnt from tweets group by wordle_id limit 1").cache()
    mtdf.show()
    val p = mtdf.head()
    println(s"Most tweeted wordle puzzle is: ${p.getString(0)} with total tweets of: ${p.getLong(1)}")
    //////////////////////
    // PROBLEM 2 END
    /////////////////////


    //////////////////////
    // PROBLEM 3 START
    /////////////////////
    case class WordCount(var playCount: Long, var theCount: Long, var wordleCount: Long) {
      def getFinalList(): List[(String, Long)] = {
        List(("play", playCount), ("the", theCount), ("wordle", wordleCount))
          .sortBy(-_._2)
      }
    }

    def wordsCount(s: String): WordCount = {
      val wc = WordCount(0, 0, 0)
      s.toLowerCase().split(" ").foreach {
        case "play" => wc.playCount += 1
        case "the" => wc.theCount += 1
        case "wordle" => wc.wordleCount += 1
        case _ =>
      }
      wc
    }
    def red(a: WordCount, b: WordCount): WordCount = {
      WordCount(a.playCount + b.playCount, a.theCount + b.theCount, a.wordleCount + b.wordleCount)
    }
    val finalCount = df.rdd.map(x => wordsCount(x.getString(4))).reduce(red)
    println("(word,count)")
    finalCount.getFinalList().foreach(println)
    //////////////////////
    // PROBLEM 3 END
    /////////////////////

    //////////////////////
    // PROBLEM 4 START
    /////////////////////
    def convertDateToweek(d: String): String = {
      val formatter = DateTimeFormatter.ofPattern("u-M-d H:m:s+00:00")
      val outFormatter = DateTimeFormatter.ofPattern("E")
      LocalDateTime.parse(d, formatter).format(outFormatter)
    }

    val wrdd = df.select("tweet_date").rdd
      .map(x => (convertDateToweek(x.getString(0)), 1L))
      .reduceByKey(_ + _)
      .sortBy(-_._2)
      .take(1)
      .head
    println(s"Highest number of tweets happened on '${wrdd._1}' and tweet count is: ${wrdd._2}")
    //////////////////////
    // PROBLEM 4 END
    /////////////////////

  }
}
