package jukebox

import java.nio.channels.FileChannel
import java.nio.file._
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

import javax.sound.sampled.AudioInputStream
import org.json4s._

import scala.collection.JavaConverters._
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
    runCommand("youtube-dl --no-call-home -q -j --flat-playlist --".split(" ").toSeq :+ url) match {
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
              if (cancel.isSet) //need to check again here, because the future is scheduled and handled much later
                throw CancelledException

              runCommand("youtube-dl --no-call-home -q -j --flat-playlist --".split(" ").toSeq :+ u) match {
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
        (playlist.size, cancel, process)
    }
  }

  def download(song: SongMetadata): Try[Path] = {
    val shmTmpDir = Files.createTempDirectory(Paths.get("/dev/shm"), "jukebox")
    var errorLog = Seq.empty[String]

    val cmd = Seq("youtube-dl", "--no-call-home", "-q", "-f", "bestaudio/best", "--no-playlist", "--no-part", "-4", "--no-cache-dir", "--") :+ song.origin
    println("running command: " + cmd)
    Process(cmd, Some(shmTmpDir.toFile)).!(ProcessLogger(l => errorLog :+= l)) match {
      case 0 => Try {
        val song = Files.list(shmTmpDir).iterator.asScala.filter { p =>
          if (p.getFileName.toString.matches(".+Frag\\d+$")) { //get rid of whatever trash youtube-dl left behind
            Files.delete(p)
            false
          } else true
        }.next
        val fileSize = Files.size(song)
        println(f"Song's size ${fileSize / 1024f / 1024}%.2fMB")
        song
      }
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
    SongMetadata(
      java.net.URLDecoder.decode(json.fulltitle.extract, "utf-8"),
      json.duration.extract[Option[String]].map(_.toInt),
      json.webpage_url.extract
    )
  }

  case object CancelledException extends Exception
}

case class SongMetadata(name: String, length: Option[Int], origin: String)
object SongMetadata {
  def fromMetadata(m: java.util.Map[String, Object]) = m.get("songMetadata").asInstanceOf[SongMetadata]
}