package jukebox

import discord.AhcUtils._
import java.nio.charset.Charset
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.util.Utf8UrlEncoder
import org.json4s.JsonAST.JObject
import scala.collection.JavaConverters._
import scala.concurrent.Future

import Json4sUtils._

object Spotalike {
  private implicit val jsonFormats = org.json4s.DefaultFormats
  def generate(ahc: AsyncHttpClient, song: String): Future[Seq[String]] = {
    request(ahc.preparePost("http://spotalike.com/").addHeader("Accept", "application/json, text/javascript").
      setHeader("Content-Type", "application/x-www-form-urlencoded").setCharset(Charset.forName("utf-8")).setBody(
        "method=query&query=" + Utf8UrlEncoder.encodeQueryElement(song)
      )) { resp =>
      val json = asDynJson(resp)
      json.tracks.extract[Seq[JObject]].map { trackJson =>
        val track = trackJson.dyn
        track.artist.extract[String] + " " + track.name.extract[String]
      }
    }
  }
}
