name := "YoutubeAnalytics"
version := "0.1.0"
scalaVersion := "2.11.12"

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.4.4"
libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.4.4"

assemblyMergeStrategy in assembly := {
  case PathList("org", "apache", _@_*) => MergeStrategy.last
  case PathList("com", "databricks", _@_*) => MergeStrategy.last
  case PathList("javax", "servlet", _@_*) => MergeStrategy.last
  case PathList("com", "esotericsoftware", _@_*) => MergeStrategy.last
  case PathList("com", "google", _@_*) => MergeStrategy.last
  case PathList("shade", _@_*) => MergeStrategy.last
  case PathList("META-INF", "maven", _@_*) => MergeStrategy.last
  case PathList("javax", "xml", _@_*) => MergeStrategy.last
  case PathList("javax", "inject", _@_*) => MergeStrategy.last
  case PathList("javax", "activation", _@_*) => MergeStrategy.last
  case PathList("javax", "annotation", _@_*) => MergeStrategy.last
  case PathList("com", "sun", "activation", _@_*) => MergeStrategy.last
  case PathList("com", "sun", "xml", _@_*) => MergeStrategy.last
  case PathList("com", "sun", "istack", _@_*) => MergeStrategy.last
  case PathList("schemaorg_apache_xmlbeans", "system", _@_*) => MergeStrategy.last
  case PathList("org", "aopalliance", _@_*) => MergeStrategy.last
  case PathList("org", "objenesis", _@_*) => MergeStrategy.last
  case PathList("javax", "ws", "rs", _@_*) => MergeStrategy.last
  case PathList("org", "fusesource", "jansi", "internal", _@_*) => MergeStrategy.last
  case PathList("org", "w3c", "dom", "bootstrap", _@_*) => MergeStrategy.last
  case PathList("org", "xml", "sax", "helpers", _@_*) => MergeStrategy.last
  case PathList("com", "sun", "research", "ws", "wadl", _@_*) => MergeStrategy.last
  case PathList("META-INF", "native", "linux32", _@_*) => MergeStrategy.last
  case PathList("META-INF", "native", "linux64", _@_*) => MergeStrategy.last
  case PathList("META-INF", "native", "osx", _@_*) => MergeStrategy.last
  case PathList("META-INF", "native", "windows32", _@_*) => MergeStrategy.last
  case PathList("META-INF", "native", "windows64", _@_*) => MergeStrategy.last
  case PathList("org", "fusesource", "hawtjni", "runtime", _@_*) => MergeStrategy.last
  case "plugin.properties" => MergeStrategy.last
  case "log4j.properties" => MergeStrategy.last
  case "about.html" => MergeStrategy.last
  case "parquet.thrift" => MergeStrategy.last
  case "plugin.xml" => MergeStrategy.last
  case "overview.html" => MergeStrategy.last
  case "git.properties" => MergeStrategy.last
  case "codegen/config.fmpp" => MergeStrategy.last
  case "module-info.class" => MergeStrategy.last
  case ".gitkeep" => MergeStrategy.last
  case "META-INF/jpms.args" => MergeStrategy.last
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
