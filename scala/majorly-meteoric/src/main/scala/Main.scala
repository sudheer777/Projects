import ErrorHandling.ErrorRecord
import MeteoricCfg.MeteoricRecord
import com.amazonaws.services.s3.AmazonS3Client
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StringType, StructField, StructType}
import org.apache.spark.sql.{DataFrame, Row, SQLContext, SparkSession}
import org.apache.spark.sql.functions.{col, input_file_name, lit}

object Main {

  val fileSchema: StructType = StructType(Seq(
    StructField("name", StringType, nullable = true),
    StructField("id", IntegerType, nullable = true),
    StructField("nametype", StringType, nullable = true),
    StructField("recclass", StringType, nullable = true),
    StructField("mass", DoubleType, nullable = true),
    StructField("fall", StringType, nullable = true),
    StructField("year", StringType, nullable = true),
    StructField("reclat", DoubleType, nullable = true),
    StructField("reclong", DoubleType, nullable = true),
    StructField("geolocation", StructType(
      Seq(
        StructField("latitude", DoubleType, nullable = false),
        StructField("longitude", DoubleType, nullable = false)
      )
    )),
    StructField(":@computed_region_cbhk_fwbd", IntegerType, nullable = false),
    StructField(":@computed_region_nnqa_25f4", IntegerType, nullable = false)
  ))

  def setHadoopConf(sc: SparkContext): Unit = {
    sc.hadoopConfiguration.set("fs.s3a.aws.credentials.provider", "org.apache.hadoop.fs.s3a.AnonymousAWSCredentialsProvider")
  }

  def doValidation(df: DataFrame, columns: Seq[String]): DataFrame = {
    var f = df
    for (c <- columns) f = f.filter(col(c).isNotNull)
    f
  }

  def averageMassOfaMeteor(df: DataFrame): Double = {
    val (sum, count) = df.rdd.map(z => (z.getDouble(4), 1)).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
    sum / count
  }

  def averageMassOfaMeteor(rdd: RDD[MeteoricCfg.MeteoricRecord]): Double = {
    val (sum, count) = rdd.map(z => (z.mass.get, 1L)).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
    sum / count
  }

  def highestNumberOfMeteors(df: DataFrame): String = {
    val h = df.rdd.map(z => (z.getString(6), 1)).reduceByKey(_ + _).reduce((x, y) => if (x._2 > y._2) x else y)
    h._1
  }

  def highestNumberOfMeteors(rdd: RDD[MeteoricCfg.MeteoricRecord]): String = {
    val h = rdd.map(z => (z.year.get, 1)).reduceByKey(_ + _).reduce((x, y) => if (x._2 > y._2) x else y)
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
    setHadoopConf(sc)
    val sqlContext = new SQLContext(sc)
    try {
      doWork(sqlContext)
    } finally {
      sc.stop()
    }
  }
}
