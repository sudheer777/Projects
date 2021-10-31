package imdb

case class TitleBasics(tconst: String, titleType: Option[String], primaryTitle: Option[String],
                      originalTitle: Option[String], isAdult: Int, startYear: Option[Int], endYear: Option[Int],
                      runtimeMinutes: Option[Int], genres: Option[List[String]])
case class TitleRatings(tconst: String, averageRating: Float, numVotes: Int)
case class TitleCrew(tconst: String, directors: Option[List[String]], writers: Option[List[String]])
case class NameBasics(nconst: String, primaryName: Option[String], birthYear: Option[Int], deathYear: Option[Int],
                      primaryProfession: Option[List[String]], knownForTitles: Option[List[String]])

object ImdbAnalysis {

  // Hint: use a combination of `ImdbData.titleBasicsPath` and `ImdbData.parseTitleBasics`
  val titleBasicsList: List[TitleBasics] = {
    scala.io.Source.fromFile(ImdbData.titleBasicsPath).getLines.map(ImdbData.parseTitleBasics).toList
  }

  // Hint: use a combination of `ImdbData.titleRatingsPath` and `ImdbData.parseTitleRatings`
  val titleRatingsList: List[TitleRatings] = {
    scala.io.Source.fromFile(ImdbData.titleRatingsPath).getLines.map(ImdbData.parseTitleRatings).toList
  }

  // Hint: use a combination of `ImdbData.titleCrewPath` and `ImdbData.parseTitleCrew`
  val titleCrewList: List[TitleCrew] = {
    scala.io.Source.fromFile(ImdbData.titleCrewPath).getLines.map(ImdbData.parseTitleCrew).toList
  }

  // Hint: use a combination of `ImdbData.nameBasicsPath` and `ImdbData.parseNameBasics`
  val nameBasicsList: List[NameBasics] = {
    scala.io.Source.fromFile(ImdbData.nameBasicsPath).getLines.map(ImdbData.parseNameBasics).toList
  }

  def task1(list: List[TitleBasics]): List[(Float, Int, Int, String)] = {
    list
      .flatMap(tb => {
        if (tb.genres.isEmpty || tb.runtimeMinutes.isEmpty)
          Array[(String, Int)]()
        else
          tb.genres.get.map(genre => (genre, tb.runtimeMinutes.get))
      }) // divide one row to multiple rows
      // input row: m1 comedy, horror,crime
      // output 3 rows; m1, comedy 2: m1, horro 3: m1, crime
      .groupBy(_._1)
      // grouping all the data for each genre.. comedy -> List((comedy, m1), (comedy, m2)), horror -> List((horro, m1))
      .map(x => {
        var maxRt = Int.MinValue
        var minRt = Int.MaxValue
        var sum: Int = 0
        x._2.foreach(rt => {
          val r = rt._2
          sum += r
          if (r > maxRt) {
            maxRt = r
          }
          if (r < minRt) {
            minRt = r
          }
        })
        val avg = 1F * sum / x._2.length
        (avg, minRt, maxRt, x._1)
      }).toList
  }

  def task2(l1: List[TitleBasics], l2: List[TitleRatings]): List[String] = {
    val titleRatingsSet = l2
      .filter(x => x.averageRating >= 7.5 && x.numVotes >= 500000)
      .map(_.tconst).toSet
    l1.filter(x => {
      x.titleType.nonEmpty && x.primaryTitle.nonEmpty &&
        x.titleType.get == "movie" &&
        x.startYear.nonEmpty &&
        x.startYear.get >= 1990 &&  x.startYear.get <= 2018 &&
        titleRatingsSet.contains(x.tconst)
    }).map(_.primaryTitle.get)
  }

  def task3(l1: List[TitleBasics], l2: List[TitleRatings]): List[(Int, String, String)] = {
    val titleRatingsMap = l2.map(x => (x.tconst, x.averageRating)).toMap
    l1.filter(x => {
      x.titleType.nonEmpty && x.primaryTitle.nonEmpty &&
        x.titleType.get == "movie" &&
        x.startYear.nonEmpty &&
        x.startYear.get >= 1900 &&  x.startYear.get <= 1999 &&
        x.genres.nonEmpty
    }).flatMap(x => {
      val rating = titleRatingsMap.get(x.tconst)
      if (rating.isEmpty) {
        Array[((String, Int), String, Float)]()
      } else {
        val decade = (x.startYear.get - 1900) / 10
        x.genres.get.map(genre => ((genre, decade), x.primaryTitle.get, rating.get))
      }
    }).groupBy(_._1)
      .map(x => {
      var topRatedMovie: String = null
      var maxRating = Float.MinValue
      x._2.foreach(mr => {
        if (mr._3 == maxRating) {
          if (mr._2 < topRatedMovie) {
            topRatedMovie = mr._2
          }
        } else if (mr._3 > maxRating) {
          maxRating = mr._3
          topRatedMovie = mr._2
        }
      })
      (x._1._2, x._1._1, topRatedMovie)
    }).toList.sortBy(x => (x._1, x._2))
  }

  // Hint: There could be an input list that you do not really need in your implementation.
  def task4(l1: List[TitleBasics], l2: List[TitleCrew], l3: List[NameBasics]): List[(String, Int)] = {
    val films = l1.filter(x => {
        x.startYear.nonEmpty &&
        x.startYear.get >= 2010
    }).map(_.tconst).toSet
    for {
      nb <- l3
      if nb.primaryName.nonEmpty && nb.knownForTitles.nonEmpty
      c = nb.knownForTitles.get.count(films.contains)
      if c >= 2
    } yield {
      (nb.primaryName.get, c)
    }
  }

  def main(args: Array[String]) {
    val durations = timed("Task 1", task1(titleBasicsList))
    val titles = timed("Task 2", task2(titleBasicsList, titleRatingsList))
    val topRated = timed("Task 3", task3(titleBasicsList, titleRatingsList))
    val crews = timed("Task 4", task4(titleBasicsList, titleCrewList, nameBasicsList))
    println(durations)
    println(titles)
    println(topRated)
    println(crews)
    println(timing)
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
