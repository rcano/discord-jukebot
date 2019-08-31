package jukebox

import java.nio.charset.Charset
import headache.AhcUtils._
import headache.JsonUtils._
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.util.Utf8UrlEncoder
import play.api.libs.json.JsObject
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.control.NoStackTrace


object Spotalike {
  
  val tokenRegex = """input name="_token" type="hidden" value="([^"]+)"""".r
  def generate(ahc: AsyncHttpClient, song: String): Future[Seq[String]] = {
    request(ahc.prepareGet("https://spotalike.com/")){ r => 
      println("Basic get: " + r)
      if (r.getStatusCode != 200) throw new Exception(s"Unexpected Spotalike response ${r.getStatusCode}: ${r.getStatusText}\n${r.getResponseBody}") with NoStackTrace
      else r
    }.flatMap { landPageResp =>
      val cookies = landPageResp.getCookies
      val token = tokenRegex.findFirstMatchIn(landPageResp.getResponseBody).getOrElse(throw new IllegalStateException("could not find secret token")).group(1)
      request(ahc.preparePost("https://spotalike.com/").addHeader("Accept", "application/json, text/javascript").
              setHeader("Content-Type", "application/x-www-form-urlencoded").setCharset(Charset.forName("utf-8")).
              setCookies(cookies).setBody(
          "_token=" + token + "&method=query&query=" + Utf8UrlEncoder.encodeQueryElement(song)
        )) { resp =>
        val json = asDynJson(resp)
        json.tracks.extract[Seq[JsObject]].map { trackJson =>
          val track = trackJson.dyn
          track.artist.extract[String] + " " + track.name.extract[String]
        }
      } 
    }
  }
}
