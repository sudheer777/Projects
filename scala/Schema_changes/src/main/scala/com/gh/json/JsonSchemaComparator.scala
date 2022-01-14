package com.gh.json
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.Source

object JsonSchemaComparator {
  import Model._


  def checkFields(schema1: String, schema2: String): Boolean = {
    val jsonSchema1 = Json.parse(schema1)
    val jsonSchema2 = Json.parse(schema2)
    val schemaFields1 = deserialize(jsonSchema1)
    val schemaFields2 = deserialize(jsonSchema2)

    val fields1 = schemaFields1.getAllFields
    val fields2 = schemaFields2.getAllFields

   println("All fields" + fields1)
   println("All fields" + fields2)


    fields2.foreach{ f2 =>
      if (!fields1.exists(_.name == f2.name) && f2.default.isEmpty)
        throw new Exception(s"Missing default value for field: ${f2.name} in old schema")
    }
    true
  }

  def checkFields2(schema1: String, schema2: String): Boolean = {
    val jsonSchema1 = Json.parse(schema1)
    val jsonSchema2 = Json.parse(schema2)
    val schemaFields1 = deserialize(jsonSchema1)
    val schemaFields2 = deserialize(jsonSchema2)

    val item1 = Item(schemaFields1.fields)
    val item2 = Item(schemaFields2.fields)

    println("All fields" + item1)
    println("All fields" + item2)

    def checkItem(item1: Item, item2: Item): Unit = {
      val flds1 = item1.fields
      item2.fields.foreach{ f2 =>
        val f1 = flds1.find(_.name == f2.name)
        if (f1.nonEmpty) {
          checkField(f1.get, f2)
        }
      }
    }

    def checkField(field1: Field, field2: Field): Unit = {
      if (field1.ttype.length != field2.ttype.length)
        throw new Exception(s"Missmatch type value for field: ${field1.name} compared to same field in old schema($field1 is not equal $field2)")
      field1.ttype.zip(field2.ttype).foreach(t => checkType(field1.name, t._1, t._2))
    }

    def checkType(fName: String, fType1: JsonTypeField, fType2: JsonTypeField): Unit = {
      def throwException() = throw new Exception(s"Missmatch type value for field: ${fName} compared to same field in old schema($fType1 is not equal $fType2)")

      fType1 match {
        case NullTypeField() | PrimitiveTypeField(_) if fType1 != fType2  => throwException()
        case RecordTypeField(fields) =>
          if (!fType2.isInstanceOf[RecordTypeField]) throwException()
          val f2Fields = fType2.asInstanceOf[RecordTypeField].fields
          checkItem(Item(fields), Item(f2Fields))
        case MapTypeField(values) =>
          if (!fType2.isInstanceOf[MapTypeField]) throwException()
          val f2Values = fType2.asInstanceOf[MapTypeField].values
          values.zip(f2Values).foreach(v => checkItem(v._1, v._2))
        case ArrayTypeField(items) =>
          if (!fType2.isInstanceOf[ArrayTypeField]) throwException()
          val f2Items = fType2.asInstanceOf[ArrayTypeField].items
          items.zip(f2Items).foreach(i => checkItem(i._1, i._2))
        case _ =>
      }
    }

    checkItem(item1, item2)
    true
  }

  def deserialize(jsValue: JsValue): SchemaFields = {
    jsValue.validate[SchemaFields] match {
      case JsSuccess(schemaFields, _) => schemaFields
      case JsError(errors) =>
        println("Exception: "+ errors)
        throw new Exception("Could not deserialize schema ")
    }
  }
}
