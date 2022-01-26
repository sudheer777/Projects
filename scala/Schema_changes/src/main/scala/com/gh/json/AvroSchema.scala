package com.gh.json

import java.io.{File, PrintWriter}
import scala.io.Source

object AvroSchema {
  def main(args: Array[String]): Unit = {
    val filePath = "/Users/sudheerpendyala/Downloads/schema.tsv" // args.head
    val outputFileLocation = "/tmp/schema.avsc"
    val bufferedSource = Source.fromFile(filePath)
    var result =
      """
        |{
        |  "type":"record",
        |  "name":"avroschema1",
        |  "alias": "avroschema",
        |  "fields":[
        |""".stripMargin
    val columns = for {
      line <- bufferedSource.getLines.drop(1)
      e = line.split("\\|")
      (columnName, columnType, possibleValues, isRequired, defaultValue) = (e(0), e(1).toLowerCase(), e(2), e(3), e(4))
    } yield {
      columnType match {
        case "string" | "long" | "double" | "boolean" | "int" =>
          getColumn(columnName, s"\"$columnType\"", isRequired.toBoolean, defaultValue)
        case "date" =>
          val typ = """{"type":"int", "logicalType":"date"}"""
          getColumn(columnName, typ, isRequired.toBoolean, defaultValue)
        case "timestamp" =>
          val typ = """{"type":"long", "logicalType":"timestamp-millis"}"""
          getColumn(columnName, typ, isRequired.toBoolean, defaultValue)
        case "enum" =>
          val symbols = possibleValues.split(",").map(x => s"\"$x\"").mkString(", ")
          val typ = s"""{"type":"enum", "name": "$columnName", "symbols":[ $symbols ]}"""
          getColumn(columnName, typ, isRequired.toBoolean, defaultValue)
        case _ => throw new Exception(s"Unsupported data type: $columnType")
      }
    }
    result += columns.mkString(",\n")
    result += "\n]}\n"
    println(result)
    bufferedSource.close()
    // write scheme to file
    val writer = new PrintWriter(new File(outputFileLocation))
    writer.write(result)
    writer.close()
  }

  def getColumn(name: String, typ: String, isRequired: Boolean, defaultValue: String): String = {
    if (defaultValue.toLowerCase() == "missing") {
      if (isRequired) {
        s"""{ "name": "$name", "type": $typ }"""
      } else {
        s"""{ "name": "$name", "type": [$typ, "null"] }"""
      }
    } else {
      if (isRequired) {
        s"""{ "name": "$name", "type": $typ, "default": "$defaultValue" }"""
      } else {
        s"""{ "name": "$name", "type": [$typ, "null"], "default": "$defaultValue" }"""
      }
    }
  }
}
