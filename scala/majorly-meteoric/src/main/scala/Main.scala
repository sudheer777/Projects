import ErrorHandling.ErrorRecord
import MeteoricCfg.MeteoricRecord
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{SQLContext, SparkSession}

object Main {

  def averageMassOfaMeteor(rdd: RDD[MeteoricCfg.MeteoricRecord]): Double = {
    val (sum, count) = rdd
      .map(z => (z.massDouble, 1L))
      .reduce((x, y) => (x._1 + y._1, x._2 + y._2))
    sum / count
  }

  def highestNumberOfMeteors(rdd: RDD[MeteoricCfg.MeteoricRecord]): String = {
    val h = rdd
      .map(z => (z.year.get, 1))
      .reduceByKey(_ + _)
      .reduce((x, y) => if (x._2 > y._2) x else y)
    h._1
  }

  def loadFileContents(bucket: String, files: RDD[String]): RDD[Either[MeteoricRecord, ErrorRecord]] = {
    files.mapPartitions { iter =>
      val s3Client = new S3Client(bucket)
      iter.flatMap { file =>
        s3Client.loadFileContents(file)
      }
    }
  }

  def doWork(sqlContext: SQLContext, invalidRecOutDir: String, s3Bucket: String): Unit = {
    import sqlContext.sparkSession.implicits._
    val s3Client = new S3Client(s3Bucket)
    val rdd = sqlContext.sparkContext.parallelize(s3Client.getAllFilenames)
    val contentRdd = loadFileContents(s3Bucket, rdd).cache()
    val metRec = contentRdd.filter(x => x.isLeft).map(_.left.get)
    val invalidRec = contentRdd.filter(x => x.isRight).map(_.right.get)
    //Save the invalid records in a file.
    invalidRec.toDF()
      .coalesce(1)
      .write
      .mode("overWrite")
      .json(invalidRecOutDir)
    println("Average mass of a meteor: " + averageMassOfaMeteor(metRec))
    println("Year in which the highest number of meteors fall is: " + highestNumberOfMeteors(metRec))
  }

  //Entry point of the program
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("Usage: Main <invalid-record-out-dir> <s3-bucket>(defaults to majorly-meteoric)")
      System.exit(1)
    }
    val invalidRecOutDir = args(0)
    val s3Bucket = if (args.length >= 2) args(1) else "majorly-meteoric"
    val bld = SparkSession.builder().appName("SAMPLE APP").config("spark.master", "local[*]")
    val ss = bld.getOrCreate()
    val sc = ss.sparkContext
    val sqlContext = new SQLContext(sc)
    try {
      doWork(sqlContext, invalidRecOutDir, s3Bucket)
    } finally {
      sc.stop()
    }
  }
}
