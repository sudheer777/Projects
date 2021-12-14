import scalaj.http.{Http, HttpOptions, Token}
import spray.json._

object Utility {
  def extractUrl(obj: String): String = {
    if (obj == null) {
      null
    } else {
      val startIndex = obj.indexOf("{ ")
      val endIndex = obj.indexOf(" }")
      if (startIndex == -1 && endIndex == -1) null else obj.substring(startIndex + 1, endIndex).trim
    }
  }

  def extractProfileId(obj: String): String = {
    val res = obj.drop(4)
      .dropRight(1)
      .split(", ")
      .map(x => {
        val k = x.split(" -> ")
        if (k.length > 1) (k(0), k(1)) else (k(0), "")
      }).filter(x => x._1 == "umm_profile_id")
    if (res.isEmpty) null else res.head._2
  }

  def replaceUrl(obj: String, url: String, surl: String): String = {
    obj.replace(url, surl)
  }

  case class MappedWebhookRendererPayload(payload: String, profileAttributes: String)

  def removeQuotes(in: String): String = {
    if (in.startsWith("\"")) in.drop(1).dropRight(1) else in
  }

  def callURLShortnerAPI(payload: MappedWebhookRendererPayload): String = {
    if (payload.payload == null || payload.profileAttributes == null) {
      if (payload.payload == null) "ERROR: payload is nil" else "ERROR: profile attributes is nil"
    } else {
      val extractedUrl = extractUrl((payload.payload).toString())
      val extractedProfileId = extractProfileId((payload.profileAttributes).toString())
      if (extractedUrl == null || extractedProfileId == null) {
        if (extractedUrl == null) "ERROR: Invalid payload" else "ERROR: Invalid profile id"
      } else {
        val retrieveURLShortenerRequestValues =
          s"""
             | {"url": "$extractedUrl",
             | "profile_id": " $extractedProfileId"
             |}""".stripMargin

        //val retrieveURLShortener = Http("http://localhost:9000/generateshorturl")
        val authToken = "H5Z3oMqOPwfmSBKglSHa6HOHAWUr0aGajTZMkhVx7srW5P5FTlNLDJKpXMJI";
        val retrieveURLShortener = Http("https://api.tinyurl.com/create")
          .headers(Seq("Authorization" -> ("Bearer " + authToken), "Content-Type" -> "application/json", "accept" -> "application/json"))
          .postData(retrieveURLShortenerRequestValues)
          .header("Content-Type", "application/json")
          .header("Charset", "UTF-8")
          .option(HttpOptions.readTimeout(10000))
          .asString

        val retrieveURLShortenerjsonResult = retrieveURLShortener.body.parseJson;

        val errors = retrieveURLShortenerjsonResult.asJsObject.getFields("errors").head.toString().drop(1).dropRight(1)
        if (errors.nonEmpty) {
          "ERROR: " + removeQuotes(errors)
        } else {
          val shortenerURLres =
            retrieveURLShortenerjsonResult.asJsObject.getFields("data").head.asJsObject.getFields("tiny_url").head.toString()
          // if string is of type "abc.com" then it removes double quote at start and end of string
          val shortUrl = removeQuotes(shortenerURLres)

          payload.payload.toString().replace(extractedUrl, shortUrl)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input1 =
      "WebhookRendererPayload with creative that needs rendering WebhookRendererPayload(WebhookDeliveryPayload" +
        "(UMM (DO NOT UPDATE),dfeefb0bb02320f468270117e385ee799fceea8c,None,8342,60,c38cc4f1051db7c0218e451-842a1820" +
        "-0f32-11eb-bf35-007c928ca240,5903a73fc990d60c8f01ce83154beb19d9051fa466e0735132b0c2fca8fce92c,WebhookCreative" +
        "(4052,1471,external,post,https://staging.com/api/messages,Some(Map(authorization -> Bearer eyJhbGciOiJIUzUxM" +
        "iIsInR5cCI6IkpXVCJ9.eyJhY3RpdmVfY29tcGFueV9pZCI6ImRmNTllZTMxLTJiNjAtNDJiNy05OGEzLTc2YWUyMGJkZjNhNCIsImFjdGl2Z" +
        "V9jb21wYW55X25hbWUiOiJRQSBUZWFtIFRlc3QgQ29tcGFueSIsImF1ZCI6IlVNTSIsImV4cCI6MTYzODU1Mzg5MiwiaWF0IjoxNjM2MTM0Nj" +
        "kyLCJpc3MiOiJVTU0iLCJqdGkiOiJjYWQ0ZDhmOC03ZWRkLTQ5YjYtYTZmMi1lZjVjNTVlMTk2OGQiLCJuYmYiOjE2MzYxMzQ2OTEsInN1Yi" +
        "I6IjViZGU1MTIxLTEzMDYtNDlhMi04NDAyLWMxNWQwZTBkOTlkNyIsInR5cCI6ImFjY2VzcyIsInVzZXJfY29tcGFueV9pZCI6ImRmNTllZTM" +
        "xLTJiNjAtNDJiNy05OGEzLTc2YWUyMGJkZjNhNCIsInVzZXJfcm9sZSI6ImNvbXBhbnlfdXNlciJ9.-t74m5A6b-3rU1GhNMi2iSGAYacYtVPN" +
        "1xkuwXxkKZzI0eVOo_dc9smqRiiIjhrsoyF0peKck5Nn9JdR4_VJZA, content-type -> application/json)),{% assign img_url " +
        "= '' %}\n{% assign img_type = '' %}\n{% assign originator_id = '00001' %}\n{% capture message_body %}\ntesting" +
        "shortened url { https://google.com }\n{% endcapture %}\n{% assign prg_id = 'd125d35a-250c-4948-9493-6cef7fd518" +
        "5d' %}\n{\n {% if img_url != '' %} \n \"profile_id\": \"{{umm_profile_id}}\",\n \"from\": \"{{originator_id}}\"," +
        "\n \"body\": \"{{message_body}}\",\n \"program_id\": \"{{prg_id}}\",\n \"attachments_urls\":[\n {\n \"url\":" +
        "\"{{img_url}}\",\n \"content_type\":\"{{img_type}}\"\n }\n ]\n {% else %}\n \"profile_id\":" +
        "\"{{umm_profile_id}}\",\n \"from\": \"{{originator_id}}\",\n \"body\": \"{{message_body}}\",\n" +
        "\"program_id\": \"{{prg_id}}\"\n {% endif %}\n},None,None)))"
    val input2 = "Map(company_id -> df59ee31-2b60-42b7-98a3-76ae20bdf3a4, first_name -> Test_webform1, zipcode -> 11887," +
      "customer_id -> 40abbff2-bdfa-4311-9c51-95fa9c1614b6, email -> a@b.com, state -> CA, umm_profile_updated_at -> 2021" +
      "-11-26, monicagender -> male, street1 -> stt_oksdf, umm_profile_id -> 40abbff2-bdfa-4311-9c51-95fa9c1614b6, " +
      "raafat_name_text -> raafat, text_enabled -> [\\n 10950\\n], street2 -> wwwdfgdg, $last_name -> demo test, $first_name" +
      "-> Test_webform1, date_of_birth -> 1963-10-10, last_name -> demo test, raafat_raafat -> value, $email -> a@b.com, " +
      "postal_code -> 65432, org -> 60, umm_profile_created_at -> 2021-04-07, org_id -> 60)"
    val inp = MappedWebhookRendererPayload(input1, input2)
    println(callURLShortnerAPI(inp))
  }
}
