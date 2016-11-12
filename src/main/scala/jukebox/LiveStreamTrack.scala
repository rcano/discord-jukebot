package jukebox

import java.io._
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem

import scala.Console
import scala.util.{Failure, Try}

import sx.blah.discord.handle.audio.IAudioProvider
import sx.blah.discord.util.audio.AudioPlayer
import sx.blah.discord.util.audio.providers.AudioInputStreamProvider

import Json4sUtils._

/**
 * Discord Track implementation of a livestream using livestreamer.
 */
object LiveStreamTrack {
  class LiveStreamAudioProvider(pb: ProcessBuilder, downloadErrorReporter: Throwable => Unit) extends IAudioProvider with Closeable {
    @volatile var processInput: (Process, AudioInputStream) = _
    lazy val _provider = Try {
      val process = pb.start()
      val inputStream = AudioSystem getAudioInputStream process.getInputStream
      processInput = (process, inputStream)
      new AudioInputStreamProvider(inputStream)
    }.recoverWith {
      case e =>
        downloadErrorReporter(e)
        Failure(e)
    }
    def isReady = _provider.map(_.isReady).getOrElse(true)
    def provide = _provider.map(_.provide).getOrElse(new Array[Byte](0))
    def close() = if (processInput != null) {
      Try(processInput._2.close())
      if (processInput._1.isAlive) processInput._1.destroy()
    }
  }

  def apply(encoder: String, song: SongMetadata, livestreamerOptions: Option[String], downloadErrorReporter: Throwable => Unit): AudioPlayer.Track = {
    val opts = livestreamerOptions.getOrElse("")
    val cmd = s"""livestreamer --hls-segment-attempts 1 --hls-segment-threads 5 -O $opts '${song.origin}' worst | ffmpeg -i - -f mp3 -ac 2 -ar 48000 -map a -"""
    println(Console.CYAN + "running livestream: " + cmd + Console.RESET)
    val res = new AudioPlayer.Track(new LiveStreamAudioProvider(new ProcessBuilder("bash", "-c", cmd), downloadErrorReporter))
    res.getMetadata.put("songMetadata", song)
    res
  }

  private implicit val jsonFormats = org.json4s.DefaultFormats
  def fetchMetadata(stream: String, options: Option[String]): Try[SongMetadata] = Try {
    import scala.sys.process._
    val opts = options.getOrElse("")
    val json = parseJson(Seq("bash", "-c", s"livestreamer -j $opts $stream").lineStream_!.force.mkString("\n")).dyn
    json.error.extract[Option[String]] match {
      case Some(error) => throw new Exception(error)
      case _ => SongMetadata(stream, None, stream)
    }
  }
}
