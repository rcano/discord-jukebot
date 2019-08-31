package jukebox

import headache.AhcUtils._
import org.asynchttpclient.AsyncHttpClient
import org.jsoup.Jsoup
import scala.jdk.CollectionConverters._
import scala.concurrent.Future

object YoutubeSearch {
  def apply(ahc: AsyncHttpClient, query: String): Future[collection.Seq[(String, String)]] = {
    request(ahc.prepareGet("https://www.youtube.com/results").addQueryParam("search_query", query)) { r =>
      if ((200 until 300) contains r.getStatusCode) {
        val html = Jsoup.parse(r.getResponseBody())
        val listElements = html.select("ol.item-section").select("li").asScala.map(_.select("div.yt-lockup-dismissable"))
        listElements.flatMap { e =>
          Option(e.select("a[href]").attr("href")).filter(_.nonEmpty).map { ytId =>
            e.select("h3.yt-lockup-title").text() -> s"https://www.youtube.com$ytId"
          }
        }
      } else {
        throw new RuntimeException(s"Unexpected status ${r.getStatusCode}${if (r.getStatusText != null) " - " + r.getStatusText else ""}.\n" + r.getResponseBody())
      }
    }
  }
}
