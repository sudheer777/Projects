import org.apache.http.HttpHeaders
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.util.EntityUtils
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import java.net.URI

object URLShortner {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  case class Response(data: Data)

  case class Data(hash: String, url: String, shortener_url: String, id: String)

  def extractUrlFromResponse(response: String): String = {
    response.drop(1).dropRight(1).trim
  }

  def requestProfileAttributes(): String = {
    "dummy_profile"
  }

  def getPostRes(hc: CloseableHttpClient, url: String, json: String, timeoutInSec: Int = 45): (CloseableHttpResponse, Long) = {
    val accessToken: String = "<ACCESS TOKEN HERE>"
    val uri = new URI(url)
    val post = new HttpPost(uri)
    val conf = RequestConfig.custom().setConnectTimeout(timeoutInSec * 1000)
      .setConnectionRequestTimeout(timeoutInSec * 1000).build()
    post.setConfig(conf)
    post.addHeader(HttpHeaders.CONTENT_TYPE, "application/json")
    post.addHeader(HttpHeaders.ACCEPT, "application/json")
    post.addHeader(HttpHeaders.AUTHORIZATION, s"Bearer $accessToken") // this might be changed
    post.setEntity(new StringEntity(json))
    val st = System.currentTimeMillis()
    val chp = hc.execute(post)
    val et = System.currentTimeMillis()
    (chp, (et - st) / 1000)
  }

  def constructJsonResponse(url: String, profileId: String): String = {
    s"""
       |{
       | "link": {
       |  "url": "$url",
       |  "profile_id": "$profileId"
       | }
       |}
       |""".stripMargin
  }

  def getUrlShortResponse(hc: CloseableHttpClient, url: String, profileId: String): Response = {
    val json: String = constructJsonResponse(url, profileId)
    val (resp, t) = getPostRes(hc, "https://urlshortner.com/api/links", json)
    resp.getStatusLine.getStatusCode match {
      case x if x / 100 == 2 =>
        val t = parse(EntityUtils.toString(resp.getEntity)).extract[Response]
        resp.close()
        t
      case x =>
        println(s"HTTP response status code: $x and it took $t seconds")
        println(s"Response status line: ${resp.getStatusLine.getReasonPhrase}")
        resp.close()
        // throw new Exception(resp.getStatusLine.getReasonPhrase)
        val out =
          s"""
             |{
             |  "data": {
             |    "hash": "hash1",
             |    "id": "id1",
             |    "url": "$url",
             |    "shortener_url": "dummy short url"
             |  }
             |}
             |""".stripMargin
        parse(out).extract[Response]
    }
  }

  def main(args: Array[String]): Unit = {
    val hc = HttpClients.createDefault()
    val response: String = "{ www.google.com }"
    val url = extractUrlFromResponse(response)
    println("url extracted:" + url)
    val profileIds = requestProfileAttributes() // This should be fetched from requestProfileAttributes
    val data: Response = getUrlShortResponse(hc, url, profileIds)
    hc.close()
    println(s"Response: ${data}")
    val urlShort = data.data.shortener_url
    println(s"url short: $urlShort")
    val output = constructJsonResponse(urlShort, profileIds)
    println(s"Final output: $output")
  }
}
