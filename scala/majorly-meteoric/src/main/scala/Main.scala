import ErrorHandling.ErrorRecord
import MeteoricCfg.MeteoricRecord
import com.amazonaws.services.s3.AmazonS3Client
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StringType, StructField, StructType}
import org.apache.spark.sql.{DataFrame, Row, SQLContext, SparkSession}
import org.apache.spark.sql.functions.{col, input_file_name, lit}

object Main {

  def averageMassOfaMeteor(rdd: RDD[MeteoricCfg.MeteoricRecord]): Double = {
    val (sum, count) = rdd
      .map(z => (z.mass.get, 1L))
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

  def doWork(sqlContext: SQLContext): Unit = {
    val s3Bucket = "majorly-meteoric"
    val s3Client = new S3Client(s3Bucket)
    val rdd = sqlContext.sparkContext.parallelize(s3Client.getAllFilenames)
    val contentRdd = loadFileContents(s3Bucket, rdd).cache()
    val metRec = contentRdd.filter(x => x.isLeft).map(_.left.get)
    val invalidRec = contentRdd.filter(x => x.isRight).map(_.right.get)
    println("Average mass of a meteor: " + averageMassOfaMeteor(metRec))
    println("Year in which highest number of meteors fall is: " + highestNumberOfMeteors(metRec))
  }


  def main(args: Array[String]): Unit = {
    val bld = SparkSession.builder().appName("SAMPLE APP").config("spark.master", "local[*]")
    val ss = bld.getOrCreate()
    val sc = ss.sparkContext
    val sqlContext = new SQLContext(sc)
    try {
      doWork(sqlContext)
    } finally {
      sc.stop()
    }
  }
}
