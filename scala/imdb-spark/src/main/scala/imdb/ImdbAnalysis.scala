package imdb

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

case class TitleBasics(tconst: String, titleType: Option[String], primaryTitle: Option[String],
                      originalTitle: Option[String], isAdult: Int, startYear: Option[Int], endYear: Option[Int],
                      runtimeMinutes: Option[Int], genres: Option[List[String]]) {
  def getGenres(): List[String] = genres.getOrElse(List[String]())
}
case class TitleRatings(tconst: String, averageRating: Float, numVotes: Int)
case class TitleCrew(tconst: String, directors: Option[List[String]], writers: Option[List[String]])
case class NameBasics(nconst: String, primaryName: Option[String], birthYear: Option[Int], deathYear: Option[Int],
                      primaryProfession: Option[List[String]], knownForTitles: Option[List[String]])

object ImdbAnalysis {

  val conf: SparkConf = new SparkConf().setAppName("IMDB Spark").setMaster("local[*]")
  val sc: SparkContext = new SparkContext(conf)

  // Hint: use a combination of `ImdbData.titleBasicsPath` and `ImdbData.parseTitleBasics`
  val titleBasicsRDD: RDD[TitleBasics] = sc.textFile(ImdbData.titleBasicsPath).map(ImdbData.parseTitleBasics)

  // Hint: use a combination of `ImdbData.titleRatingsPath` and `ImdbData.parseTitleRatings`
  val titleRatingsRDD: RDD[TitleRatings] = sc.textFile(ImdbData.titleRatingsPath).map(ImdbData.parseTitleRatings)

  // Hint: use a combination of `ImdbData.titleCrewPath` and `ImdbData.parseTitleCrew`
  val titleCrewRDD: RDD[TitleCrew] = sc.textFile(ImdbData.titleCrewPath).map(ImdbData.parseTitleCrew)

  // Hint: use a combination of `ImdbData.nameBasicsPath` and `ImdbData.parseNameBasics`
  val nameBasicsRDD: RDD[NameBasics] = sc.textFile(ImdbData.nameBasicsPath).map(ImdbData.parseNameBasics)

  def task1(rdd: RDD[TitleBasics]): RDD[(Float, Int, Int, String)] = {
    rdd
      .flatMap(tb => {
        if (tb.genres.isEmpty || tb.runtimeMinutes.isEmpty)
          Array[(String, Int)]()
        else
          tb.genres.get.map(genre => (genre, tb.runtimeMinutes.get))
      }) // divide one row to multiple rows
      // input row: m1 comedy, horror,crime
      // output 3 rows; m1, comedy 2: m1, horro 3: m1, crime
      .groupBy(_._1)
      // grouping all the data for each genre.. comedy
      .map(x => {
      var maxRt = Int.MinValue
      var minRt = Int.MaxValue
      var sum: Int = 0
      val mvs = x._2.toArray
      mvs.foreach(rt => {
        val r = rt._2
        sum += r
        if (r > maxRt) {
          maxRt = r
        }
        if (r < minRt) {
          minRt = r
        }
      })
      val avg = 1F * sum / mvs.length
      (avg, minRt, maxRt, x._1)
    })
  }

  def task2(l1: RDD[TitleBasics], l2: RDD[TitleRatings]): RDD[String] = {
    val titleRatingsRdd = l2
      .filter(x => x.averageRating >= 7.5 && x.numVotes >= 500000)
      .map(x => (x.tconst, ""))
    val titleBasicsRdd = l1.filter(x => {
      x.titleType.nonEmpty && x.primaryTitle.nonEmpty &&
        x.titleType.get == "movie" &&
        x.startYear.nonEmpty &&
        x.startYear.get >= 1990 &&  x.startYear.get <= 2018
    }).map(x => (x.tconst, x.primaryTitle.get))
    titleBasicsRdd
      .join(titleRatingsRdd)
      .map(_._2._1)
  }

  def task3(l1: RDD[TitleBasics], l2: RDD[TitleRatings]): RDD[(Int, String, String)] = {
    val titleRatingsRdd = l2
      .map(x => (x.tconst, x.averageRating))
    val titleBasicsRdd = l1.filter(x => {
      x.titleType.nonEmpty && x.primaryTitle.nonEmpty &&
        x.titleType.get == "movie" &&
        x.startYear.nonEmpty &&
        x.startYear.get >= 1900 &&  x.startYear.get <= 1999 &&
        x.genres.nonEmpty
    }).map(x => (x.tconst, (x.startYear.get, x.genres.get, x.primaryTitle.get)))

    titleBasicsRdd.join(titleRatingsRdd)
      .flatMap(x => {
        val rating = x._2._2
        val (startYear, genres, primaryTitle) = x._2._1
        val decade = (startYear - 1900) / 10
        genres.map(genre => ((genre, decade), (primaryTitle, rating)))
    }).groupByKey()
      .map(x => {
        var topRatedMovie: String = null
        var maxRating = Float.MinValue
        x._2.foreach(mr => {
          if (mr._2 == maxRating) {
            if (mr._1 < topRatedMovie) {
              topRatedMovie = mr._1
            }
          } else if (mr._2 > maxRating) {
            maxRating = mr._2
            topRatedMovie = mr._1
          }
        })
        (x._1._2, x._1._1, topRatedMovie)
      }).sortBy(x => (x._1, x._2))
  }

  // Hint: There could be an input RDD that you do not really need in your implementation.
  def task4(l1: RDD[TitleBasics], l2: RDD[TitleCrew], l3: RDD[NameBasics]): RDD[(String, Int)] = {
    val films = sc.broadcast(l1.filter(x => {
      x.startYear.nonEmpty &&
        x.startYear.get >= 2010
    }).map(_.tconst).collect().toSet)
    l3
      .filter(nb => nb.primaryName.nonEmpty && nb.knownForTitles.nonEmpty)
      .flatMap(nb => {
        val f = films.value
        val c = nb.knownForTitles.get.count(f.contains)
        if (c >= 2) Array((nb.primaryName.get, c)) else Array[(String, Int)]()
      })
  }

  def main(args: Array[String]) {
    val durations = timed("Task 1", task1(titleBasicsRDD).collect().toList)
    val titles = timed("Task 2", task2(titleBasicsRDD, titleRatingsRDD).collect().toList)
    val topRated = timed("Task 3", task3(titleBasicsRDD, titleRatingsRDD).collect().toList)
    val crews = timed("Task 4", task4(titleBasicsRDD, titleCrewRDD, nameBasicsRDD).collect().toList)
    println(durations)
    println(titles)
    println(topRated)
    println(crews)
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
