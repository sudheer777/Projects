import Utility.{MappedWebhookRendererPayload, callURLShortnerAPI}
import org.scalatest.FlatSpec

class UtilityTest extends FlatSpec {
  private val payload =
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

  private val profileAttributes = "Map(company_id -> df59ee31-2b60-42b7-98a3-76ae20bdf3a4, first_name -> Test_webform1, zipcode -> 11887," +
    "customer_id -> 40abbff2-bdfa-4311-9c51-95fa9c1614b6, email -> a@b.com, state -> CA, umm_profile_updated_at -> 2021" +
    "-11-26, monicagender -> male, street1 -> stt_oksdf, umm_profile_id -> 40abbff2-bdfa-4311-9c51-95fa9c1614b6, " +
    "raafat_name_text -> raafat, text_enabled -> [\\n 10950\\n], street2 -> wwwdfgdg, $last_name -> demo test, $first_name" +
    "-> Test_webform1, date_of_birth -> 1963-10-10, last_name -> demo test, raafat_raafat -> value, $email -> a@b.com, " +
    "postal_code -> 65432, org -> 60, umm_profile_created_at -> 2021-04-07, org_id -> 60)"
  val expectedOutput = "40abbff2-bdfa-4311-9c51-95fa9c1614b6"

  it should "test extractUrl method" in {
    val expectedOutput = "https://google.com"
    assert(Utility.extractUrl(payload) == expectedOutput)
  }

  it should "test extractProfileId method" in {
    assert(Utility.extractProfileId(profileAttributes) == expectedOutput)
  }

  it should "test replaceUrl method" in {
    val url = Utility.extractUrl(payload)
    val shortUrl = "https://sample.shorturl/qwre"
    val output = Utility.replaceUrl(payload, url, shortUrl)
    val expectedOutput = Utility.extractUrl(output)
    assert(shortUrl == expectedOutput)
  }

  it should("callURLShortnerAPI: replace input with tiny url") in {
    val inp = MappedWebhookRendererPayload(payload, profileAttributes)
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "https://tinyurl.com/3yhy72ax"
    assert(Utility.extractUrl(res) == expectedOutput)
  }

  it should("callURLShortnerAPI: invalid url") in {
    val pl = "test { invalidurl } test"
    val inp = MappedWebhookRendererPayload(pl, profileAttributes)
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "ERROR: Invalid URL"
    assert(res == expectedOutput)
  }

  it should("callURLShortnerAPI: payload without url") in {
    val pl = "test data without url"
    val inp = MappedWebhookRendererPayload(pl, profileAttributes)
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "ERROR: Invalid payload"
    assert(res == expectedOutput)
  }

  it should("callURLShortnerAPI: payload is null") in {
    val pl = null: String
    val inp = MappedWebhookRendererPayload(pl, profileAttributes)
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "ERROR: payload is nil"
    assert(res == expectedOutput)
  }

  it should("callURLShortnerAPI: invalid profileAttributes") in {
    val inp = MappedWebhookRendererPayload(payload, "asds")
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "ERROR: Invalid profile id"
    assert(res == expectedOutput)
  }

  it should("callURLShortnerAPI: null profileAttributes") in {
    val inp = MappedWebhookRendererPayload(payload, null)
    val res = callURLShortnerAPI(inp)
    val expectedOutput = "ERROR: profile attributes is nil"
    assert(res == expectedOutput)
  }
}
