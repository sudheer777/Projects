package com.gh.json

import org.scalatest.flatspec._
import org.scalatest.matchers._
import play.api.libs.json._

import scala.io.Source

class ExampleSpec extends AnyFlatSpec with should.Matchers {
  import Model._

  it should "test1" in {
    val jsonOld = Source.fromResource("schema-with-record-fields.json").getLines().mkString
    val jsonNew = Source.fromResource("schema-with-record-fields-1.json").getLines().mkString

    val str1 = """{"type":"record","name":"deleted_legacy_code","namespace":"com.grubhub.gdpschemautils.schemas.codes","fields":[{"name":"request_id","doc":"A unique identifier for the event","type":["null",{"type": "string", "logicalTyp":"uuid"}],"logical":"uuid"},{"name":"event_time","doc":"The time the event was created","type":["null","long"],"logicalType":"timestamp-millis"},{"name":"entitlement_id","type":["null","string"],"logicalType":"uuid"},{"name":"code_type","type":["null","string"]},{"name":"request_type","type":["null","string"]}]}"""
    val str2 = """{"type":"record","name":"deleted_legacy_code","namespace":"com.grubhub.gdpschemautils.schemas.codes","fields":[{"name":"request_id","doc":"A unique identifier for the event","type":["null",{"type": "string", "logicalTyp":"uuid1"}],"logical":"uuid"},{"name":"event_time","doc":"The time the event was created","type":["null","long"]},{"name":"entitlement_id","type":["null","string"],"logicalType":"uuid"},{"name":"code_type","type":["null","string"]},{"name":"request_type","type":["null","string"]}]}"""

    // println(AvroSchemaComparator.compare(jsonOld, jsonNew))
    println(AvroSchemaComparator.compare(str1, str2))
  }

}