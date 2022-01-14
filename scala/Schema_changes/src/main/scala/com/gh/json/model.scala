package com.gh.json


import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
 * Classes to represent relevant fields from the Json schema
 **/


object Model {

sealed trait JsonTypeField

final case class PrimitiveTypeField(ttype: String) extends JsonTypeField

final case class NullTypeField() extends JsonTypeField

/**
 * Used to represent array json objects in the 'type' json array
 *
 * "type": [
 *   "null",
 *   {
 *     "type": "array",
 *     "items": [
 *        {"name": "carts", ...}
 *      ]
 *   }
 *  ]
 */
final case class ArrayTypeField(
  items: List[Item]
) extends JsonTypeField

final case class RecordTypeField(
  fields: List[Field]
) extends JsonTypeField

final case class MapTypeField(
  values: List[Item]
) extends JsonTypeField


/**
 * Used to represent elements in the 'items' json array
 *
 * "items": [
 *   "null",
 *   {
 *     "fields": [
 *        {"name": "carts", ...},
 *        {"name": "cart_diner_id", ...},
 *      ]
 *   }
 * ]
 */
final case class Item(
  fields: List[Field]
)

/**
 *  Represents fields in the schema, they can be simple as follows, or complex/nested
 *  {
 *     "name": "cart_line_apply_free_grub",
 *     "type": [
 *       "boolean",
 *       "null"
 *     ],
 *     "default": false
 *   }
 **/
final case class Field(
  name: String,
  ttype: List[JsonTypeField],
  default: Option[String]

  //default = None, means that the 'default' key is not present
  //default = Some(null), 'default' is present with null value
  //default = Some(whatever), 'default' is present with any value
) {

  def getFields: List[Field] = {
    Field(name, Nil, default) +: toBasicType(Nil, ttype)
  }

  def toBasicType(acc: List[Field], xs: List[JsonTypeField]): List[Field] = {
    xs match {
      case Nil => acc
      case head :: tail =>
        head match {
          case RecordTypeField(fields) =>
            toBasicType(acc ++ fields.flatMap(_.getFields), tail)

          case ArrayTypeField(items) =>
            toBasicType(acc ++ items.flatMap(_.fields.flatMap(_.getFields)), tail)

          case MapTypeField(values) =>
            toBasicType(acc ++ values.flatMap(_.fields.flatMap(_.getFields)), tail)

          case _ => List.empty
        }
    }
  }

}

/**
 * root json key to traverse the fields
 */
final case class SchemaFields(
  fields: List[Field]

) {

  def getAllFields: List[Field] = {
    fields.flatMap(_.getFields)
  }

}

  implicit val config = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  implicit class ExtendedJsPath(path: JsPath) {
    def readNullableDefaultField: Reads[Option[String]] =
      Reads[Option[String]] { json =>
        path.asSingleJson(json) match {
          case JsDefined(JsNull) =>
            JsSuccess(Some("null"))
          case JsDefined(js) =>
            JsSuccess(Some(js.toString()))
          case JsUndefined() =>
            JsSuccess(None)
        }
      }
  }


  implicit val basicFieldReads: Reads[Field] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "type").lazyRead[List[JsonTypeField]](typeFieldListReads) and
      (JsPath \ "default").readNullableDefaultField
    )(Field.apply _)


  implicit val itemReads: Reads[Item] = Json.reads[Item]

  implicit val itemListReads: Reads[List[Item]] = new Reads[List[Item]] {
    override def reads(json: JsValue): JsResult[List[Item]] = {
      json match {
        case JsArray(array) =>
          val list = array.flatMap{elem =>
            elem.validate[Item] match {
              case JsSuccess(value, _) =>
                List(value)
              case _  =>
                List.empty
            }
          }.toList
          if (list.nonEmpty) JsSuccess(list)
          else {
            val fType = array.flatMap {
              case JsString(value) => List(if (value == "null") NullTypeField() else PrimitiveTypeField(value))
              case JsObject(underlying) => List(PrimitiveTypeField(underlying.toString()))
              case _ => List.empty
            }
            JsSuccess(List(Item(List(Field("element", fType.toList, None)))))
          }
        case _ => JsSuccess(List.empty)
      }
    }
  }

  implicit val stringTypeFieldReads: Reads[PrimitiveTypeField] = Json.reads[PrimitiveTypeField]

  implicit val arrayTypeFieldReads: Reads[ArrayTypeField] =
    (JsPath \ "items").lazyRead[List[Item]](itemListReads).map(ArrayTypeField)

  implicit val recordTypeFieldReads: Reads[RecordTypeField] =
    (JsPath \ "fields").lazyRead(Reads.seq[Field](basicFieldReads)).map(seq => RecordTypeField(seq.toList))

  implicit val mapTypeFieldReads: Reads[MapTypeField] =
    (JsPath \ "values").lazyRead[List[Item]](itemListReads).map(MapTypeField)

  implicit val typeFieldListReads: Reads[List[JsonTypeField]] = new Reads[List[JsonTypeField]] {
    override def reads(json: JsValue): JsResult[List[JsonTypeField]] = {
      json match {
        case JsArray(array) =>

          val list = array.flatMap{ elem =>
            val complexTypeField = validate[ArrayTypeField](elem)
            if (complexTypeField.nonEmpty) complexTypeField
            else {
              val recordTypeField = validate[RecordTypeField](elem)
              if (recordTypeField.nonEmpty) recordTypeField
              else {
                val mapTypeField = validate[MapTypeField](elem)
                if (mapTypeField.nonEmpty) mapTypeField
                else {
                  elem match {
                    case JsString(value) => List(if (value == "null") NullTypeField() else PrimitiveTypeField(value))
                    case JsObject(underlying) => List(PrimitiveTypeField(underlying.toString()))
                    case _ => List.empty
                  }
                }
              }
            }
          }.toList
          JsSuccess(list)
        case JsString(value) =>
          val l: List[JsonTypeField] = List(PrimitiveTypeField(value))
          JsSuccess(l)
        case _ => JsSuccess(List.empty)
      }
    }

    def validate[T](jsValue: JsValue)(implicit rds: Reads[T]): List[T] = {
      jsValue.validate[T] match {
        case JsSuccess(value, _) =>
          List(value)
        case _  =>
          List.empty
      }
    }
  }


  implicit val schemaFieldsReads: Reads[SchemaFields] = Json.reads[SchemaFields]


}