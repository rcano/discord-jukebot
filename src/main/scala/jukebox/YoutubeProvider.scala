package jukebox

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

import javax.sound.sampled.AudioInputStream
import org.json4s._

import scala.concurrent._
import scala.sys.process._
import scala.util.Try

import Json4sUtils._

/**
 * Provider of video metadata and data from youtube
 */
object YoutubeProvider {

  /**
   * Fetch information for the given video or playlist.
   * @param url Video or playlist
   * @param progessReporter If the url is a playlist, it will take time to process, this function will be called with the counter of processed videos.
   * @return A try instance of tuple, the int part represents the amount of videos that are bieng processed, the Future holds the result.
   */
  def fetchInformation(url: String, errorReporter: String => Unit, progressReporter: Int => Unit): Try[(Int, SyncVar[Unit], Future[Seq[SongMetadata]])] = Try {
    implicit val commandExecutor = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
    runCommand("youtube-dl -q -j --flat-playlist --".split(" ").toSeq :+ url) match {
      case (Seq(), errorLog) => throw new Exception(errorLog.mkString("\n"))
      case (Seq(line), _) => (1, new SyncVar, Future.successful(Seq(extractSongMetadata(parseJson(line)))))
      case (playlist, _) =>
        //in case of a playlist, I now have to fetch eatch video's data
        val cancel = new SyncVar[Unit]()
        val urls = playlist map parseJson filter (_.dyn._type.extract[String] == "url") map (j => j.dyn.url.extract[String])
        val alreadyCompleted = new AtomicInteger()

        val urlsBeingProcessed = urls.zipWithIndex map {
          case (u, idx) => 
            if (cancel.isSet) Future.failed(CancelledException)
            Future {
              if (cancel.isSet)//need to check again here, because the future is scheduled and handled much later
                throw CancelledException

              runCommand("youtube-dl -q -j --flat-playlist --".split(" ").toSeq :+ u) match {
                case (Seq(), errorLog) =>
                  errorReporter(s"Failed processing $u\n" + errorLog.mkString)
                  None
                case (Seq(line), _) => 
                  progressReporter(alreadyCompleted.incrementAndGet)
                  Some(idx -> extractSongMetadata(parseJson(line)))
              }
            }
        }

        val process = Future.sequence(urlsBeingProcessed).map(_.flatten.sortBy(_._1).map(_._2)).andThen { case _ => commandExecutor.shutdown() }
        (playlist.size,cancel, process)
    }
  }

  def download(song: SongMetadata): Try[Array[Byte]] = {
    val res = new java.io.ByteArrayOutputStream(1024*1024 * 5)
    var errorLog = Seq.empty[String]
    val cmd = Seq("youtube-dl", "-q", "-f", "bestaudio/best", "--no-playlist", "-4", "--no-cache-dir", "-o", "-", "--") :+ song.origin
    println("running command: " + cmd)
    (cmd #> res).!(ProcessLogger(l => errorLog :+= l)) match {
      case 0 => scala.util.Success(res.toByteArray)
      case other => scala.util.Failure(new Exception(s"exit value $other. " + errorLog.mkString))
    }
  }

  private def runCommand(seq: Seq[String]) = {
    println("running command: " + seq)
    var errorLog = Seq.empty[String]
    seq.lineStream_!(ProcessLogger(l => errorLog :+= l)).force -> errorLog
  }

  private implicit val jsonFormats = DefaultFormats
  private def extractSongMetadata(jv: JValue) = {
    val json = jv.dyn
    SongMetadata(java.net.URLDecoder.decode(json.fulltitle.extract, "utf-8"),
                 json.duration.extract[Option[String]].map(_.toInt).getOrElse(-1),
                 json.webpage_url.extract)
  }

  case object CancelledException extends Exception
}

case class SongMetadata(name: String, length: Int, origin: String)