package jukebox

import com.sedmelluq.discord.lavaplayer.player.{AudioConfiguration, AudioLoadResultHandler, DefaultAudioPlayerManager}
import com.sedmelluq.discord.lavaplayer.player.event._
import com.sedmelluq.discord.lavaplayer.source.youtube.YoutubeAudioSourceManager
import com.sedmelluq.discord.lavaplayer.source.local.LocalAudioSourceManager
import com.sedmelluq.discord.lavaplayer.track.AudioTrack
import jukebox.discord._
import org.json4s.native.JsonMethods.{pretty, render}
import scala.collection.JavaConverters._
import scala.concurrent._, duration._

import RegexExtractor._

object Bot2 extends App {

  case class Clargs(discordToken: String = null, channel: String = null)
  val clargs = new scopt.OptionParser[Clargs]("discord-jukebox") {
    opt[String]('t', "token").action((x, c) => c.copy(discordToken = x)).required.text("discord bot token")
    opt[String]('c', "channel").action((x, c) => c.copy(channel = x)).required.text("discord voice channel")
  }.parse(args, new Clargs()).getOrElse(sys.exit(0))

  val audioPlayerManager = new DefaultAudioPlayerManager()
  audioPlayerManager.getConfiguration.setResamplingQuality(AudioConfiguration.ResamplingQuality.HIGH)
  audioPlayerManager.registerSourceManager(new YoutubeAudioSourceManager())
  audioPlayerManager.registerSourceManager(new LocalAudioSourceManager())
  val ap = audioPlayerManager.createPlayer()

  object DiscordHandler extends DiscordClient.DiscordListener {
    var ourUser: User = _
    var currentGateway: DiscordClient#GatewayConnection = _
    var currentVoiceConn: DiscordClient#VoiceConnection = _
    var guild: GatewayEvents.Guild = _
    var ourVoiceState: GatewayEvents.VoiceStateUpdate = _
    def me = s"<@${ourUser.id}>"
    def musicChannel = guild.channels.find(_.name == clargs.channel)
    def generalChannel = guild.channels.find(_.name == "general")

    override def onUnexpectedGatewayOp(conn, op, data) = println(s"Received unsupported op $op: ${pretty(render(data.jv))}")
    override def onUnexpectedVoiceOp(conn, op, data) = println(Console.MAGENTA + s"Received unsupported op $op: ${pretty(render(data.jv))}" + Console.RESET)
    override def onMessageBeingSent(conn, msg) = {
      if (!conn.isInstanceOf[DiscordClient#VoiceConnection])
        println(s"Sending $msg")
    }

    override def onReconnecting(conn, reason) = println(s"reconnecting $conn because of $reason")
    override def onConnectionOpened(conn) = {
      println(s"$conn connected")
      conn match {
        case gw: DiscordClient#GatewayConnection => currentGateway = gw
        case vc: DiscordClient#VoiceConnection => currentVoiceConn = vc
      }
    }
    override def onConnectionClosed(conn) = {
      println(s"$conn closed")
      conn match {
        case gw: DiscordClient#GatewayConnection => currentGateway = null
        case vc: DiscordClient#VoiceConnection => currentVoiceConn = null
      }
    }
    override def onDisconnected(conn, code, reason) = {
      println(s"$conn disconnected, code $code due to $reason")
      conn match {
        case gw: DiscordClient#GatewayConnection => currentGateway = null
        case vc: DiscordClient#VoiceConnection => currentVoiceConn = null
      }
    }
    override def onConnectionError(conn, ex) = ex.printStackTrace()

    def onGatewayEvent(conn) = {
      case evt: GatewayEvents.Ready => ourUser = evt.user
      case GatewayEvents.GuildCreate(g) =>
        guild = g
        setupVoiceChannel()
      case GatewayEvents.Resumed =>
        setupVoiceChannel()
      case GatewayEvents.MessageCreate(msg) if msg.content startsWith me =>
        try {
          val cmd = msg.content.stripPrefix(me).replaceFirst("^[,:]?\\s+", "")
          commands.find(_.action(msg).isDefinedAt(cmd)) match {
            case Some(command) => command.action(msg)(cmd)
            case _ => messageSender.reply(msg, s"Sorry, I don't know the command: $cmd")

          }
        } catch { case e: Exception => e.printStackTrace() }

      case u: GatewayEvents.VoiceStateUpdate =>
        ourVoiceState = u

      case vsu: GatewayEvents.VoiceServerUpdate =>
        if (ourVoiceState.voiceState.channelId.isDefined) {
          println("stablishing websocket to " + ourVoiceState.voiceState.channelId.get)
          val noData = new Array[Byte](0)
          conn.client.connectToVoiceChannel(ourVoiceState, vsu, bytes => println("Received " + bytes.length), () => {
            val frame = ap.provide
            if (frame == null) noData else frame.data
          })
        }

      case _ =>
    }

    private def setupVoiceChannel(): Unit = {
      currentGateway.sendVoiceStateUpdate(guild.id, None, false, true)
      currentGateway.sendVoiceStateUpdate(guild.id, Some(musicChannel.get.id), false, true)
    }

    def leaveMusicChannel(): Unit = {
      if (currentVoiceConn != null) {
        currentVoiceConn.close()
        currentGateway.sendVoiceStateUpdate(guild.id, None, false, true)
      } else println("Already not in the music channel")
    }
    def joinMusicChannel(): Unit = if (currentVoiceConn == null) setupVoiceChannel() else println("Already in the music channel")
    def updatePresence(status: Status): Unit = {
      if (currentGateway != null) currentGateway.sendStatusUpdate(None, status)
    }
  }
  val discordClient = new DiscordClient(clargs.discordToken, DiscordHandler)

  object Playlist extends AudioEventListener {
    private val _tracks = collection.mutable.Buffer[AudioTrack]()
    def tracks: Seq[AudioTrack] = _tracks.toVector
    def queue(track: AudioTrack): Unit = {
      _tracks += track
      if (ap.getPlayingTrack == null) ap.playTrack(track)
    }
    def remove(track: AudioTrack): Unit = _tracks -= track
    def shuffle(): Unit = {
      val shuffled = scala.util.Random.shuffle(_tracks)
      _tracks.clear()
      _tracks ++= shuffled
    }
    def skip(): Unit = _tracks.headOption.fold(ap.playTrack(null))(ap.playTrack)
    def size = _tracks.size
    def onEvent(evt) = evt match {
      case evt: TrackStartEvent =>
        _tracks -= evt.track
        updatePlayingTrack()

      case evt: TrackEndEvent =>
        if (evt.endReason.mayStartNext) _tracks.headOption.fold(updatePlayingTrack())(ap.playTrack)
        else updatePlayingTrack()
      case evt: TrackExceptionEvent =>
        DiscordHandler.generalChannel foreach (c => messageSender.send(c.id, "Failed processing track " + evt.track.getInfo.title + " due to " + evt.exception))
        _tracks -= evt.track
        _tracks.headOption.fold(updatePlayingTrack())(ap.playTrack)
        evt.exception.printStackTrace()
      case other => println("other ap event: " + other)
    }
  }
  ap.addListener(Playlist)

  val messageSender = new DiscordRateHonoringSender2(discordClient)
  val commands = collection.mutable.ListBuffer[Command]()
  case class Command(name: String, description: String)(val action: Message => PartialFunction[String, Any])

  commands += Command("status", "shows what I'm currently playing and pending.")(msg => {
    case "status" =>
      messageSender.reply(msg, if (ap.getPlayingTrack == null) "Nothing is being played."
      else {
        val track = ap.getPlayingTrack
        val metadata = track.getInfo
        s"Currently playing ${metadata.title} - ${millisToString(track.getPosition)}/${millisToString(track.getDuration)}. ${Playlist.size} remaining tracks.\n${metadata.identifier}"
      })
  })

  commands += Command("list[ full]", "shows the remaining list of songs to be played.")(msg => {
    case "list" | "list full" if Playlist.size == 0 && ap.getPlayingTrack == null => messageSender.reply(msg, "Nothing in the queue")
    case cmd @ ("list" | "list full") =>
      val full = cmd.endsWith("full")
      val songs = Playlist.tracks ++ Option(ap.getPlayingTrack)
      val songsInfo = songs.zipWithIndex.map(e => e._2 + ": " + e._1.getInfo.title +
        (if (full) " - " + e._1.getInfo.identifier else ""))
      val totalTime = songs.map(_.getDuration).sum
      messageSender.reply(msg, s"Playlist total time ${millisToString(totalTime)}")
      songsInfo foreach (messageSender.reply(msg, _))
  })

  commands += Command("pause/stop", "pause the currently playing song")(msg => {
    case "pause" | "stop" =>
      ap.setPaused(true)
      messageSender.reply(msg, "_paused_")

    //        discordClient changeStatus Status.game("paused")
  })
  commands += Command("unpause/resume/play", "resumes playing the song")(msg => {
    case "unpause" | "resume" | "play" =>
      ap.setPaused(false)
      messageSender.reply(msg, "_unpaused_")
      updatePlayingTrack()
  })

  commands += Command("shuffle", "shuffles the playlist")(msg => {
    case "shuffle" =>
      Playlist.shuffle()
      messageSender.reply(msg, "_shuffled_")
  })

  commands += Command("skip", "skips this song")(msg => {
    case "skip" =>
      Playlist.skip()
      ap.getPlayingTrack match {
        case null =>
          messageSender.reply(msg, "_end of playlist_")
        case song => messageSender.reply(msg, s"_skipped to ${song.getInfo.title}_")
      }
  })
  commands += Command("skip to <index>", "skips to the specified song")(msg => {
    case regex"skip to (.+)$where" => where match {
      case regex"""(\d+)$num""" =>
        if (Playlist.size == 0) messageSender.reply(msg, "There is nothing to skip to. Try adding some songs to the playlist with the `add` command.")
        else {
          val dest = math.min(num.toInt, Playlist.size)
          val song = Playlist.tracks(dest)
          (0 until dest) foreach (i => Playlist.remove(Playlist.tracks(0)))
          Playlist.skip()
          messageSender.reply(msg, "_skipping to " + song.getInfo.title + "_")
        }
      case other => messageSender.reply(msg, "skip to command only accepts a number")
    }
  })

  //  commands += Command("add livestream <url>", "Adds a livestream to the playlist. Note that duration of this is unknown.")(msg => {
  //      case regex"""add livestream (\w+://[^ ]+)$url(?: (.+))?$options""" =>
  //        messageSender.reply(msg, "_adding " + url + " with options " + options + "_")
  //        LiveStreamTrack.fetchMetadata(url, Option(options)).map { song =>
  //          ap.queue(LiveStreamTrack(clargs.encoder, song, Option(options), error => messageSender.reply(msg, "_" + error + "_")))
  //          messageSender.reply(msg, "_added " + url + " to the queue._")
  //
  //          ensureNextTrackIsCached(ap) //if we just added a song after the currently playing, ensure it starts fetching it
  //        }.failed.foreach(e => messageSender.reply(msg, s"Failed: $e"))
  //    })

  commands += Command("add <url>", "Adds the given song to the queue.")(msg => {
    case regex"add (.+)$url" => url match {
      case regex"(https?://.+|)$url" =>
        messageSender.reply(msg, "_adding " + url + "_")
        audioPlayerManager.loadItemOrdered(ap, url, new AudioLoadResultHandler {
          override def trackLoaded(track) = {
            Playlist.queue(track)
            messageSender.reply(msg, "_added " + track.getInfo.title + " to the queue._")
          }
          override def playlistLoaded(playlist) = {
            playlist.getTracks.asScala foreach Playlist.queue
            val num = playlist.getTracks.size
            messageSender.reply(msg, s"_added $num songs to the queue._")
          }
          override def noMatches = messageSender.reply(msg, "Sorry, I couldn't find anything with that url.")
          override def loadFailed(ex) = {
            messageSender.reply(msg, "Sorry, loading fail :(. " + ex)
            ex.printStackTrace()
          }
        })
      case other => messageSender.reply(msg, "for now I only support youtube links. Sorry.")
    }
  })

  commands += Command("remove range", "Removes all the song between the two specified indeces")(msg => {
    case regex"""remove range (\d+)$n1 (\d+)$n2""" =>
      val from = n1.toInt
      val to = math.min(n2.toInt, Playlist.size)
      if (from > to) messageSender.reply(msg, s"$from is greater than $to ... :sweat:")
      else {
        val removedTracks = for (i <- from until to) yield {
          val track = Playlist.tracks(from) //removing from is on purpose
          Playlist.remove(track)
          track.getInfo.title
        }
        messageSender.reply(msg, "_removed:_")
        removedTracks foreach (t => messageSender.reply(msg, "_" + t + "_"))
      }

    case regex"""remove range .*""" => messageSender.reply(msg, "I'm sorry, remove range only accepts a pair of naturals")
  })
  commands += Command("remove <index>", "Removes the specified index from the queue. (Use list to check first)")(msg => {
    case regex"remove (.+)$what" =>
      what match {
        case regex"""(\d+)$n""" =>
          val num = n.toInt
          if (num < 0) messageSender.reply(msg, "A negative number? :sweat:")
          else if (Playlist.size == 0) messageSender.reply(msg, "The playlist is empty.")
          else if (num >= Playlist.size) messageSender.reply(msg, s"$num is larger than the playlist's size, you meant to remove the last one? it's ${Playlist.size - 1}")
          else if (num == 0) Playlist.skip()
          else {
            val track = Playlist.tracks(num)
            Playlist.remove(track)
            messageSender.reply(msg, s"_ removed ${track.getInfo.title}_")
          }

        case other =>

          Playlist.tracks.find(t => t.getInfo.title == other) match {
            case Some(track) =>
              Playlist.remove(track)
              messageSender.reply(msg, s"_ removed ${track.getInfo.title}_")
            case _ => messageSender.reply(msg, s"Sorry, there is no song named `$other`")
          }
      }
  })

  //  commands += Command("cancel", "Makes me cancel the processing of a playlist that someone requested.")(msg => {
  //      case "cancel" if processingPlaylist.isEmpty => messageSender.reply(msg, "I'm not processing anything, so there is nothing to cancel.")
  //      case "cancel" =>
  //        val (user, c) = processingPlaylist.get
  //        c.put(())
  //        messageSender.send(msg.getChannel, s"Cancelling ${user.mention} 's work as per ${msg.getAuthor.mention} 's request.")
  //    })

  commands += Command("join", s"Makes me join the voice channel ${clargs.channel}")(msg => {
    case "join" => DiscordHandler.joinMusicChannel()
  })
  commands += Command("leave", s"Makes me leave the voice channel ${clargs.channel}")(msg => {
    case "leave" => DiscordHandler.leaveMusicChannel()
  })
  commands += Command("kill yourself", s"Makes me die.")(msg => {
    case "kill yourself" =>
      messageSender.reply(msg, "Bye bye.")
      scala.util.Try(DiscordHandler.currentVoiceConn.close())
      scala.util.Try(DiscordHandler.currentGateway.close())
      sys.exit(0)
  })

  commands += Command("help", "Prints this message")(msg => {
    case "help" =>
      val maxCmdWidth = commands.map(_.name.length).max
      val helpString = new StringBuilder
      commands foreach (c => helpString.append(c.name.padTo(maxCmdWidth, ' ')).append(" - ").append(c.description).append("\n"))
      messageSender.reply(msg, "```\n" + helpString.toString + "```")
  })

  def updatePlayingTrack(): Unit = {
    ap.getPlayingTrack match {
      case null => DiscordHandler.updatePresence(Status.Empty)
      case track => DiscordHandler.updatePresence(Status.Streaming(track.getInfo.title, track.getInfo.identifier))
    }
  }

  def millisToString(millis: Long) = {
    val seconds = millis / 1000
    val hours = if (seconds >= 3600) (seconds / 3600) + ":" else ""
    f"${hours}${(seconds % 3600) / 60}%02d:${seconds % 60}%02d"
  }

  discordClient.login()
}
