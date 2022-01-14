package com.gh.json

import org.apache.avro.Schema

import scala.collection.JavaConverters.asScalaBufferConverter

object AvroSchemaComparator {
  def compare(schema1: String, schema2: String): Boolean = {
    val avroSch1 = new Schema.Parser().parse(schema1)
    val avroSch2 = new Schema.Parser().parse(schema2)

    def compareSchema(sc1: Schema, sc2: Schema, topLevelField: String = ""): Unit = {
      if (sc2.getType == Schema.Type.UNION && sc1.getType != Schema.Type.UNION) {
        val sc2Ty = sc2.getTypes.asScala.filter(x => x.getType != Schema.Type.NULL)
        if (sc2Ty.length != 1) throw new Exception(s"Mismatch across $sc1 and $sc2 and field name: $topLevelField")
        compareSchema(sc1, sc2Ty.head, topLevelField)
      } else {
        if (sc1.getType != sc2.getType || sc1.getLogicalType != sc2.getLogicalType)
          throw new Exception(s"Mismatch across $sc1 and $sc2 and field name: $topLevelField")
        sc1.getType match {
          case Schema.Type.UNION =>
            if (sc1.getTypes.size() != sc2.getTypes.size()) throw new Exception(s"Mismatch across $sc1 and $sc2")
            sc1.getTypes.asScala.zip(sc2.getTypes.asScala).foreach(x => compareSchema(x._1, x._2, topLevelField))
          case Schema.Type.MAP => compareSchema(sc1.getValueType, sc2.getValueType, topLevelField)
          case Schema.Type.ARRAY => compareSchema(sc1.getElementType, sc2.getElementType, topLevelField)
          case Schema.Type.RECORD =>
            val sf1 = sc1.getFields.asScala.map(x => (x.name(), x)).toMap
            sc2.getFields.asScala.foreach(f => {
              val s = sf1.get(f.name())
              if (s.nonEmpty) {
                compareSchema(s.get.schema(), f.schema(), f.name())
              }
            })
          case _ =>
        }
      }
    }
    compareSchema(avroSch1, avroSch2)
    true
  }
}
