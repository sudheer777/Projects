package vardhanbabburi

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object RDDOperations {

  // problem 1
  def wordCount(rdd: RDD[String]): RDD[(String, Long)] = {
    val toRemove = ",.“?)(".toSet
    val words = rdd
      .flatMap(line => line.split(" "))
      .filter(word => word.trim.nonEmpty)
      .map(word => {
        val w = word.toLowerCase()
        (w.filterNot(toRemove), 1L)
      })
      .reduceByKey(_ + _)
    words.sortBy(-_._2)
  }

  // problem 2
  // Returns RDD with all words which starts with prefix and ends with suffix and also word count greater than equal to thresholdCount
  def matchWords(wordCount: RDD[(String, Long)], prefix: String, suffix: String, thresholdCount: Long): RDD[(String, Long)] = {
    wordCount.filter(w => {
      val (word, count) = w
      word.startsWith(prefix) && word.endsWith(suffix) && count >= thresholdCount
    })
  }

  // problem 3
  def wordMatch(wordCount: RDD[String], prefixChar: Char, minimumLetters: Int): RDD[(String, Long)] = {
    val wcrdd = wordCount.map(x => {
      val x1 = x.drop(1).dropRight(1)
      val s = x1.split(",")
      (s.head, s(1).toLong)
    })
    wcrdd.filter(w => {
      val word = w._1
      word.head == prefixChar && word.length >= minimumLetters
    })
  }

  // problem 4
  def wordMatch1(wordCount: RDD[(String, Long)], prefixChar: Char, minimumLetters: Int): RDD[(String, Long)] = {
    wordCount.filter(w => {
      val word = w._1
      word.nonEmpty && word.head == prefixChar && word.length >= minimumLetters
    })
  }

  def main(args: Array[String]): Unit = {
    val sconf = new SparkConf().setAppName("Test").setMaster("local[*]")
    val sc = new SparkContext(sconf)

    val rdd = sc.textFile("src/main/scala/vardhanbabburi/sample_data.txt")
    //rdd.take(5).foreach(println)
    val wcrdd = wordCount(rdd)
    wcrdd.coalesce(1).saveAsTextFile("src/main/scala/vardhanbabburi/word_count")

    // problem 2
    matchWords(wcrdd, prefix = "l", suffix = "e", thresholdCount = 2).coalesce(1)
      .saveAsTextFile("src/main/scala/vardhanbabburi/match_words")

    // problem 3
    val wcRdd1 = sc.textFile("src/main/scala/vardhanbabburi/word_count")
    val wordMatchRdd = wordMatch(wcRdd1, 'a', 5)
    wordMatchRdd.saveAsTextFile("src/main/scala/vardhanbabburi/word_match")
  }

}
