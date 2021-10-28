import MeteoricCfg.MeteoricRecord
import com.amazonaws.auth.AWSCredentials
import com.amazonaws.services.s3.AmazonS3Client

import scala.collection.mutable

class S3Client(bucket: String) {
  private val s3Client = new AmazonS3Client(null: AWSCredentials)

  def getAllFilenames: Seq[String] = {
    println(s"Listing files in $bucket bucket:")
    val objectSummaries = s3Client.listObjects(bucket).getObjectSummaries
    val files = mutable.ArrayBuffer[String]()
    for (i <- 0 until objectSummaries.size()) {
      files.append(objectSummaries.get(i).getKey)
    }
    println(s"Found filenames: ${files.mkString(", ")}")
    files
  }

  def loadFileContents(fileName: String): Seq[Either[MeteoricRecord, ErrorHandling.ErrorRecord]] = {
    println(s"Started loading data from file s3://$bucket/$fileName")
    val obj = s3Client.getObject(bucket, fileName)
    val contentStream = obj.getObjectContent
    val content = scala.io.Source.fromInputStream(contentStream).mkString
    val res = MeteoricCfg.load(fileName, content)
    println(s"Completed loading data from file s3://$bucket/$fileName")
    res
  }
}