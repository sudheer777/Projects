object AverageMass {

  def main(args: Array[String]): Unit = {
    val s3Bucket = "majorly-meteoric"
    val s3Client = new S3Client(s3Bucket)
  }
}