object RecursiveFolder {
  %scala
  import org.apache.hadoop.fs._
  val utcTimezone = java.util.TimeZone.getTimeZone("UTC")
  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  dateFormat.setTimeZone(utcTimezone)
  val THRESHOLD: Int = 100 // change this so that threshold status is updated accordingly

  def getDateTime(ts: Long): String = {
    val date = new java.util.Date(ts)
    dateFormat.format(date)
  }

  def listAllFiles(folder: Path, mdTime: Long, exclude: Seq[String], maxDepth: Int, currDepth: Int = 0): Unit = {
    val mt = getDateTime(mdTime)
    val fs = folder.getFileSystem(spark.sessionState.newHadoopConf)

    val files = fs.listStatus(folder) // this includes both files and folder
    val (subDirs, subFiles) = files.partition(x => x.isDirectory)
    val emptyStatus = if (files.isEmpty) "is empty" else "is not empty"
    val thresholdStatus = if (files.length >= THRESHOLD) "crossed threshold" else "under control"
    println(s"$folder,$mt,$emptyStatus,${subDirs.length},${subFiles.length},$thresholdStatus")
    if (currDepth >= maxDepth) return
    if (files.isEmpty) return
    val subfolders = subDirs.filter(_.getModificationTime > 0) // only filter folders and ignore files
    val excludeFromSubfolders = subfolders.filter(x => {
      val folderName = x.getPath.getName
      !(exclude.contains(x.getPath.toString) || exclude.exists(y => folderName.startsWith(y)))
    }) // only have folders which doesn't present in exclude seq
    if (excludeFromSubfolders.nonEmpty) excludeFromSubfolders.foreach(x => listAllFiles(x.getPath, x.getModificationTime, exclude, maxDepth, currDepth + 1))
  }

  println("folder name,modified date,empty folder or not,sub folders count, sub files count,crossed threshold or not")
  listAllFiles(new Path ("adl://prodrxperso.azuredatalakestore.net/rxperso/dev/"),0,
    Seq("adl://prodrxperso.azuredatalakestore.net/rxperso/dev/nba", "abc", "bcd", "assd", "qwer"), 1)
}
