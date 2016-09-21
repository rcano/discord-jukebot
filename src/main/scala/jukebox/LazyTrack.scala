package jukebox

import java.io._

import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem

import scala.Console

import scala.util.{Failure, Try}

import sx.blah.discord.handle.audio.IAudioProvider
import sx.blah.discord.util.audio.AudioPlayer
import sx.blah.discord.util.audio.providers.AudioInputStreamProvider

/**
 * AudioPlayer.Track implementation that is lazy with its input stream creation, allowing to save up memory.
 */
object LazyTrack {
  class LazyAudioProvider(val audioInputStream: () => AudioInputStream, downloadErrorReporter: Throwable => Unit) extends IAudioProvider {
    @volatile var _audioInputStream: AudioInputStream = _
    lazy val _provider = Try {
      _audioInputStream = audioInputStream()
      val r = new AudioInputStreamProvider(_audioInputStream)
      r
    }.recoverWith {
      case e =>
        downloadErrorReporter(e)
        Failure(e)
    }
    def isReady = _provider.map(_.isReady).getOrElse(true)
    def provide = _provider.map(_.provide).getOrElse(new Array[Byte](0))
    def close() = if (_audioInputStream != null) _audioInputStream.close()
  }
  def apply(song: SongMetadata, downloadErrorReporter: Throwable => Unit): AudioPlayer.Track = {
    lazy val inputStream = {
      println(Console.CYAN + "Getting song ready " + song + Console.RESET)
      val bytes = YoutubeProvider.download(song).get

      val process = new ProcessBuilder("avconv -i - -f mp3 -ac 2 -ar 48000 -map a -".split(" "):_*).
      redirectError(new File("/dev/null")). //stderr must be consumed, or ffmpeg won't emit output
      start()
      new Thread() {
        override def run = try {
          scala.sys.process.BasicIO.transferFully(new ByteArrayInputStream(bytes), process.getOutputStream)
          process.getOutputStream.close()
        } catch {
          case e: IOException if e.getMessage == "Broken pipe" =>
        }
      }.start()
      val inputStream = new BufferedInputStream(process.getInputStream, 1024 * 10) {  //need some buffer in order to be able to reset, which AudioSystem will attempt
        override def close = {
          super.close()
          if (process.isAlive) process.destroy()
        }
      }
      AudioSystem getAudioInputStream inputStream
    }
    val inputProvider = new LazyAudioProvider(() => inputStream, downloadErrorReporter)
    val res = new AudioPlayer.Track(inputProvider)
    res.getMetadata.put("title", song.name)
    res.getMetadata.put("duration", song.length: java.lang.Integer)
    res.getMetadata.put("origin", song.origin)
    res
  }
}
