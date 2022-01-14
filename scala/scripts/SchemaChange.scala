import org.apache.spark.sql.types.{ArrayType, BooleanType, DataType, LongType, MapType, StringType, StructField, StructType, TimestampType}

object SchemaChange {
  def main(args: Array[String]): Unit = {
    val schema = StructType(Seq(
      StructField("request_id",StringType,true),
      StructField("ruleset_version",StringType,true),
      StructField("version_was_active",BooleanType,true),
      StructField("time_bucket",TimestampType,true),
      StructField("test",ArrayType(TimestampType),true),
      StructField("test2", StructType(Seq(StructField("time_bucket",TimestampType,true),
        StructField("test",ArrayType(TimestampType),true))),true),
      StructField("test3", ArrayType(StructType(Seq(StructField("time_bucket",TimestampType,true),
        StructField("test",ArrayType(TimestampType),true))),true)),
      StructField("test4", MapType(TimestampType, StructType(Seq(StructField("time_bucket",TimestampType,true),
        StructField("test",ArrayType(TimestampType),true)))), true)
    ))
    println(convertSchema(schema))

  }

  def convertSchema(inp: StructType): StructType = {
    def convertType(f: DataType): DataType = {
      f match {
        case TimestampType => LongType
        case StructType(p) => convertSchema(StructType(p))
        case MapType(key, value, m) => MapType(convertType(key), convertType(value), m)
        case ArrayType(s, a) => ArrayType(convertType(s), a)
        case _ => f
      }
    }

    def convertField(f: StructField): StructField = {
      f match {
        case StructField(name, d, n, m) => StructField(name, convertType(d), n, m)
      }
    }

    StructType(inp.map(convertField))
  }
}
