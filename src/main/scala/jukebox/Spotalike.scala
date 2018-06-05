package jukebox

import java.nio.charset.Charset
import headache.AhcUtils._
import headache.JsonUtils._
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.util.Utf8UrlEncoder
import play.api.libs.json.JsObject
import scala.concurrent.Future


object Spotalike {
  def generate(ahc: AsyncHttpClient, song: String): Future[Seq[String]] = {
    request(ahc.preparePost("http://spotalike.com/").addHeader("Accept", "application/json, text/javascript").
      setHeader("Content-Type", "application/x-www-form-urlencoded").setCharset(Charset.forName("utf-8")).setBody(
        "method=query&query=" + Utf8UrlEncoder.encodeQueryElement(song)
      )) { resp =>
      val json = asDynJson(resp)
      json.tracks.extract[Seq[JsObject]].map { trackJson =>
        val track = trackJson.dyn
        track.artist.extract[String] + " " + track.name.extract[String]
      }
    }
  }
}
