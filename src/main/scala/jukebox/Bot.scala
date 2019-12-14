package jukebox

import better.files._
import com.sedmelluq.discord.lavaplayer.player.{AudioConfiguration, AudioLoadResultHandler, DefaultAudioPlayerManager}
import com.sedmelluq.discord.lavaplayer.player.event._
import com.sedmelluq.discord.lavaplayer.source.youtube.YoutubeAudioSourceManager
import com.sedmelluq.discord.lavaplayer.source.local.LocalAudioSourceManager
import com.sedmelluq.discord.lavaplayer.source.http.HttpAudioSourceManager
import com.sedmelluq.discord.lavaplayer.tools.FriendlyException
import com.sedmelluq.discord.lavaplayer.track.{AudioTrack, AudioPlaylist}
import headache._, JsonUtils._
import regex._
import scala.jdk.CollectionConverters._
import scala.concurrent.{Channel => _, _}, duration._, ExecutionContext.Implicits._

object Bot { def main(args: Array[String]): Unit = new Bot(args)}
class Bot(args: Array[String]) {

  case class Clargs(discordToken: String = null, channel: String = null, dictatorMode: Boolean = false, dictatorAssociates: Seq[String] = Seq.empty,
                    musicDirectory: Option[String] = None)
  val clargs = new scopt.OptionParser[Clargs]("discord-jukebox") {
    opt[String]('t', "token").action((x, c) => c.copy(discordToken = x)).required.text("discord bot token")
    opt[String]('c', "channel").action((x, c) => c.copy(channel = x)).required.text("discord voice channel")
    opt[Unit]("dictator").action((x, c) => c.copy(dictatorMode = true)).text("toggle dictator mode")
    opt[Seq[String]]("associates").action((x, c) => c.copy(dictatorAssociates = x)).text("dictator associates list, comma separated")
    opt[String]("musicDirectory").validate { path =>
      val f = path.toFile
      Either.cond(f.exists() && f.isDirectory(), (), s"$path does not exists or is not a directory")
    }.action((x, c) => c.copy(musicDirectory = Some(x)))
  }.parse(args, new Clargs()).getOrElse(sys.exit(0))

  val audioPlayerManager = new DefaultAudioPlayerManager()
  audioPlayerManager.getConfiguration.setResamplingQuality(AudioConfiguration.ResamplingQuality.LOW)
  audioPlayerManager.registerSourceManager(new YoutubeAudioSourceManager())
  audioPlayerManager.registerSourceManager(new LocalAudioSourceManager())
  audioPlayerManager.registerSourceManager(new HttpAudioSourceManager())
  audioPlayerManager.setPlayerCleanupThreshold(Long.MaxValue)
  audioPlayerManager.setFrameBufferDuration(123)

  val noData = new Array[Byte](0)

  object DiscordHandlerSM extends DiscordClient.DiscordListenerStateMachine[Any] {
    override def onUnexpectedGatewayOp(conn: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector) = println(s"Received unsupported op $op: ${renderJson(data.jv.get, true)}")
    //    override def onUnexpectedVoiceOp(conn, op, data) = println(Console.MAGENTA + s"Received unsupported op $op: ${pretty(render(data.jv))}" + Console.RESET)
    override def onMessageBeingSent(conn: DiscordClient#Connection, msg: String) = {
      if (!conn.isInstanceOf[DiscordClient#VoiceConnection])
        println(s"$conn Sending $msg")
    }
//    override def onGatewayOp(conn: DiscordClient#GatewayConnection, op: headache.GatewayOp, data: => DynJValueSelector) = println(s"received: $op " + renderJson(data.jv.get, true))
//    override def onVoiceOp(conn, op, data) = println(s"received: $op " + renderJson(data.jv.get, true))
    override def undefHandler(evt: DiscordClient.DiscordListenerStateMachine.Event) = evt match {
      case ConnectionError(conn, err) => println(s"$conn receive error: $err\n\t${err.getStackTrace.mkString("\n\t")}")
      case Disconnected(conn, code, reason) => println(s"Conn $conn disconnected $code: $reason")
        //      case GatewayEvent(_, evt) => println(s"Event happened while I wasn't paying attention: ${evt.tpe}. I'm sitting at state $current")
        //      case evt => println(s"Event happened while I wasn't paying attention: ${evt.getClass}. I'm sitting at state $current")
      case _ =>
    }
    def initState = awaitReady(None)
    def awaitReady(botData: Option[BotData]): Transition = transition {
      case GatewayEvent(_, GatewayEvents.ReadyEvent(evt)) => awaitGuilds(botData, evt)
    }
    def awaitGuilds(prevBotData: Option[BotData], ready: GatewayEvents.Ready, accGuilds: Seq[GatewayEvents.Guild] = Vector.empty): Transition = transition {
      case GatewayEvent(conn, GatewayEvents.GuildCreateEvent(GatewayEvents.GuildCreate(g))) =>
        val guilds = accGuilds :+ g
        if (prevBotData.isDefined || guilds.size == ready.guilds.size) {
          val botData = prevBotData.fold {
            val botData = BotData(conn, ready, guilds.view.map(g => g -> new GuildStateMachine(ready.user.id.snowflakeString, conn, g)).to(Map))
            println("just conected with " + botData)
            botData
          }(_.withUpdatedConnection(conn))

          botData.guilds.valuesIterator.foreach(_.applyIfDefined(RejoinVoiceChannel))
          messageHandling(botData)

        } else {
          println(s"Accumulating guild, so far ${guilds.size}")
          awaitGuilds(prevBotData, ready, guilds)
        }
    }

    private case object Shutdown

    private def dispatchByChannelId(data: BotData, channelId: Snowflake, evt: Any)(notFoundCase: => Any) = {
      data.guilds.find(_._1.channels.exists(_.id == channelId)).fold(notFoundCase: Unit)(e => e._2.applyIfDefined(evt))
    }
    private def dispatchByGuildId(data: BotData, guildId: Snowflake, evt: Any)(notFoundCase: => Any) = {
      data.guilds.find(_._1.id == guildId).fold(notFoundCase: Unit)(e => e._2.applyIfDefined(evt))
    }

    def messageHandling(data: BotData): Transition = transition {
      case GatewayEvent(_, GatewayEvents.MessageCreateEvent(GatewayEvents.MessageCreate(msg))) =>
        dispatchByChannelId(data, msg.channelId, msg) {
          //see if the message starts with a guildId
          gr"""guild:(\d+) .+""".findFirstMatchIn(msg.content) match {
            case Some(matchh) =>
              println(matchh)
              val guild = Snowflake(matchh.group(1))
              dispatchByGuildId(data, guild, msg.copy(content = msg.content.replaceFirst(s"guild:$guild ", "")))(messageSender.reply(msg, s"Could not find guild $guild"))
            case _ => messageSender.reply(msg, s"What guild is this?")
          }
        }
        current
        
      case GatewayEvent(_, GatewayEvents.PresenceUpdateEvent(evt)) =>
        dispatchByGuildId(data, evt.guildId, evt)(())
        current

      case GatewayEvent(_, GatewayEvents.VoiceStateUpdateEvent(voiceState)) =>
        val dispatcher = voiceState.voiceState.guildId.map(g => dispatchByGuildId(data, g, _: Any) _) orElse
        voiceState.voiceState.channelId.map(c => dispatchByChannelId(data, c, _: Any) _)
        dispatcher.fold(println(s"No guild nor channelId in VoiceStateUpdate? $voiceState"))(
          _(voiceState)(println(s"What guild is this VoiceStateUpdate from? $voiceState"))
        )
        current

      case GatewayEvent(conn, GatewayEvents.VoiceServerUpdateEvent(vsu)) =>
        dispatchByGuildId(data, vsu.guildId, vsu)(println(s"What guild is this VoiceServerUpdate from? $vsu"))
        current

      case ConnectionOpened(conn: DiscordClient#VoiceConnection) =>
        dispatchByGuildId(data, conn.guildId, conn)(println(s"What guild is this Voice connection from? $conn"))
        current

      case ConnectionError(conn: DiscordClient#VoiceConnection, err) =>
        println(s"Exception closed Voice conn $conn: $err\n" + err.getStackTrace.mkString("\n"))
        data.guilds.find(_._1.id == conn.guildId).fold {
          println(s"Could not find guild id ${conn.guildId} among guilds ${data.guilds.keys.map(g => g.name -> g.id)}")
        } {
          case (guild, gsm) =>
            println(s"pausing audio player for guild ${guild.name}")
            gsm.ap.setPaused(true) //stop the underlying player, when the connection resumes, it'll go back to the appropriate state
        }
        current
      case Disconnected(conn: DiscordClient#VoiceConnection, code, reason) =>
        println(s"Voice conn $conn disconnected $code: $reason")
        current
      case Shutdown =>
        scala.util.Try(data.gateway.close())
        done
    } orElse reconnect(data)

    def shutdown(): Unit = apply(Shutdown)

    def reconnect(data: BotData) = transition {
      case Reconnecting(conn: DiscordClient#GatewayConnection, reason) =>
        println("###reconnecting to gateway because of " + reason)
        transition {
          case GatewayEvent(conn, GatewayEvents.Resumed(())) =>
            val newBotData = data.withUpdatedConnection(conn)
            newBotData.guilds.valuesIterator.foreach(_.applyIfDefined(RejoinVoiceChannel))
            messageHandling(newBotData)
        } orElse awaitReady(Some(data))
    }
  }

  case class BotData(gateway: DiscordClient#GatewayConnection, info: GatewayEvents.Ready, guilds: Map[GatewayEvents.Guild, GuildStateMachine]) {
    override def toString = s"BotData($gateway)"
    def withUpdatedConnection(conn: DiscordClient#GatewayConnection): BotData = {
      guilds.foreach { case (g, gsm) => gsm(UpdateGatewayConnection(conn)) }
      copy(gateway = conn)
    }

  }
  private case object RejoinVoiceChannel
  private case class UpdateGatewayConnection(gateway: DiscordClient#GatewayConnection)
  class GuildStateMachine(initialMe: String, initialGateway: DiscordClient#GatewayConnection, initialGuild: GatewayEvents.Guild) extends StateMachine[Any] {
    println(Console.RED + s"State machine initiated for $initialMe, conn $initialGateway, guild ${initialGuild.name}" + Console.RESET)

    object & {
      def unapply[T](t: T) = Some(t -> t)
    }

    case class GuildData(
      myId: String,
      gateway: DiscordClient#GatewayConnection,
      guild: GatewayEvents.Guild,
      voiceConnection: Option[DiscordClient#VoiceConnection],
      subscribers: Seq[Subscription],
      paused: Boolean = false,
      processing: Option[SyncVar[Unit]] = None,
      tracking: Option[Tracking] = None
    ) {
      object MePrefx {
        val prefix = s"<@!?${myId}>".r
        def unapply(s: Message): Option[String] = prefix.findPrefixOf(s.content)
      }
    }
    def initState = messageHandling(GuildData(initialMe, initialGateway, initialGuild, None, Seq.empty))

    case class Tracking(user: Snowflake, channel: Snowflake)
    case class Subscription(msg: Message)
    case class UpdateStatus(status: String)
    case class SetPaused(paused: Boolean)
    def messageHandling(guildData: GuildData): Transition = transition {
      case (msg: Message) & guildData.MePrefx(pref) =>
        try {
          val cmd = msg.content.stripPrefix(pref).replaceFirst("^[,:]?\\s+", "")
          commands.find(_.action(guildData, msg).isDefinedAt(cmd)) match {
            case Some(command) =>
              if (!clargs.dictatorMode || clargs.dictatorAssociates.contains(msg.author.id))
                command.action(guildData, msg)(cmd)
              else
                messageSender.reply(msg, s"Sorry, running in dictator mod. Only the dictator and his associates may command me.")
            case _ => messageSender.reply(msg, s"Sorry, I don't know the command: $cmd")
          }
        } catch {
          case e: Exception => e.printStackTrace()
        }
        current
        
      case GatewayEvents.PresenceUpdate(_, user, _, _, Some(gameStatus), _) if 
        gameStatus.tpe.map(GameStatus.Type.Listening.==).getOrElse(false) &&
        guildData.tracking.map(_.user == user.id).getOrElse(false) =>
        val query = (gameStatus.state.getOrElse(" ") + gameStatus.details.getOrElse(" ")).trim
        if (query.nonEmpty) {
          YoutubeSearch(discordClient.ahc, query).onComplete {
            case scala.util.Success(seq) =>
              val channel = guildData.guild.channels.find(_.id == guildData.tracking.get.channel).get
              audioPlayerManager.loadItemOrdered(ap, seq.head._2, audioLoader(Left(channel)))
            case scala.util.Failure(ex) => messageSender.send(guildData.tracking.get.channel, s"Something went wrong: $ex")
          }
        }
        
        current

      case RejoinVoiceChannel =>
        guildData.voiceConnection foreach { vc =>
          vc.close()
          guildData.gateway.sendVoiceStateUpdate(guildData.guild.id, None, false, true)
        }
        setupVoiceChannel(guildData, c => messageHandling(guildData.copy(voiceConnection = Some(c))))
      case UpdateGatewayConnection(conn) => messageHandling(guildData.copy(gateway = conn))(RejoinVoiceChannel)

      case gd: GuildData => messageHandling(gd)

      case SetPaused(paused) => messageHandling(guildData.copy(paused = paused))

      case UpdateStatus(status) =>
        guildData.subscribers.foreach(s => messageSender.reply(s.msg, status))
        current
    }

    def setupVoiceChannel(guildData: GuildData, nextState: DiscordClient#VoiceConnection => Transition): Transition = {
      guildData.gateway.sendVoiceStateUpdate(guildData.guild.id, None, false, true)
      guildData.gateway.sendVoiceStateUpdate(guildData.guild.id, guildData.guild.channels.find(_.name.get equalsIgnoreCase clargs.channel).map(_.id), false, true)

      def bogusTransition(p: PartialFunction[Any, Transition]): Transition = transition(p) orElse transition {
        case UpdateGatewayConnection(conn) => 
          println(s"   → received bogus update grateway connection while already setting up vc? guild ${initialGuild.name}")
          current
      }

      bogusTransition {
        case ourVoiceState: GatewayEvents.VoiceStateUpdate if ourVoiceState.voiceState.channelId.isDefined =>
          println("voice state received, waiting for voice server udpate")
          bogusTransition {
            case vsu: GatewayEvents.VoiceServerUpdate if vsu.endpoint.isEmpty =>
              println("waiting for endpoint for " + ourVoiceState.voiceState.channelId.get)
              current
            case vsu: GatewayEvents.VoiceServerUpdate if vsu.endpoint.isDefined =>
              println("establishing websocket to " + ourVoiceState.voiceState.channelId.get + " " + vsu.endpoint)
              guildData.gateway.client.connectToVoiceChannel(ourVoiceState, vsu, bytes => (), { () =>
                  val frame = ap.provide()
                  if (frame == null) noData else frame.getData()
                })
              bogusTransition {
                case conn: DiscordClient#VoiceConnection =>
                  println(s"resuming player to paused state ${guildData.paused}")
                  ap.setPaused(guildData.paused)
                  nextState(conn)
              }
          }
      }
    }

    def rejoinMusicChannel(): Unit = applyIfDefined(RejoinVoiceChannel)
    def updateStatus(status: String): Unit = applyIfDefined(UpdateStatus(status))
    def updateState(gd: GuildData) = apply(gd)

    val ap = audioPlayerManager.createPlayer()
    object Playlist extends AudioEventListener {
      private val _tracks = collection.mutable.LinkedHashMap[AudioTrack, Channel Either Message]()
      def tracks: collection.Map[AudioTrack, Channel Either Message] = _tracks
      def queue(track: AudioTrack, origin: Channel Either Message): Unit = {
        _tracks += track -> origin
        if (ap.getPlayingTrack == null) ap.playTrack(track)
      }
      def remove(i: Int): AudioTrack = {
        val t = tracks.keys.drop(i).head
        _tracks -= t
        t
      }
      def remove(track: AudioTrack): Unit = _tracks -= track
      def clear(): Unit = _tracks.clear()
      def shuffle(): Unit = {
        val shuffled = scala.util.Random.shuffle(_tracks.toSeq)
        _tracks.clear()
        _tracks ++= shuffled
      }
      def skip(): Unit = _tracks.headOption.fold(ap.playTrack(null))(t => ap.playTrack(t._1))
      def size = _tracks.size
      def onEvent(evt: AudioEvent) = scala.util.Try {
        evt match {
          case evt: TrackStartEvent =>
            _tracks -= evt.track
            updatePlayingTrack()

          case evt: TrackEndEvent =>
            println("Event ended " + evt.track.getInfo.title)
            if (evt.endReason.mayStartNext) _tracks.headOption.fold(updatePlayingTrack())(t => ap.playTrack(t._1))
            else {
              println(Console.RED + "not allowed to play next track?" + Console.RESET)
              updatePlayingTrack()
            }
          case evt: TrackExceptionEvent =>
            val originalMessage = _tracks(evt.track)
            val msg = "Failed processing track " + evt.track.getInfo.title + " due to " + evt.exception
            originalMessage.fold(c => messageSender.send(c, msg), m => messageSender.reply(m, msg))
            _tracks -= evt.track
            _tracks.headOption.fold(updatePlayingTrack())(t => ap.playTrack(t._1))
            evt.exception.printStackTrace()
          case other => println(Console.CYAN + "other ap event: " + other + Console.RESET)
        }
      }.failed foreach (_.printStackTrace())
    }
    ap.addListener(Playlist)

    def updatePlayingTrack(): Unit = { //repurposed for the subscription system
      ap.getPlayingTrack match {
        case null => updateStatus("idle")
        case track => updateStatus(track.getInfo.title + "\nnow playing:" + track.getInfo.uri)
      }
    }

    val commands = collection.mutable.ListBuffer[Command]()
    case class Command(name: String, description: String)(val action: (GuildData, Message) => PartialFunction[String, Any])

    commands += Command("status", "shows what I'm currently playing and pending.")((_, msg) => {
        case "status" =>
          messageSender.reply(msg, if (ap.getPlayingTrack == null) "Nothing is being played."
                              else {
              val track = ap.getPlayingTrack
              val metadata = track.getInfo
              s"Currently playing ${metadata.title} - ${millisToString(track.getPosition)}/${millisToString(track.getDuration)}. ${Playlist.size} remaining tracks.\n${metadata.uri}"
            })
      })

    commands += Command("list[ full]", "shows the remaining list of songs to be played.")((_, msg) => {
        case "list" | "list full" if Playlist.size == 0 && ap.getPlayingTrack == null => messageSender.reply(msg, "Nothing in the queue")
        case cmd @ ("list" | "list full") =>
          val full = cmd.endsWith("full")
          val songs = Option(ap.getPlayingTrack) ++ Playlist.tracks.keys
          val songsInfo = songs.zipWithIndex.map(e => e._2 + ": " + e._1.getInfo.title +
                                                 (if (full) " - " + e._1.getInfo.uri else ""))
          val totalTime = songs.map(_.getDuration).sum
          messageSender.reply(msg, s"Playlist total time ${millisToString(totalTime)}")
          songsInfo foreach (messageSender.reply(msg, _))

        case "list for add" =>
          val songs = (Option(ap.getPlayingTrack) ++ Playlist.tracks.keys).map(_.getInfo.uri)
          messageSender.reply(msg, songs.mkString(" "))
      })

    commands += Command("pause/stop", "pause the currently playing song")((_, msg) => {
        case "pause" | "stop" =>
          ap.setPaused(true)
          applyIfDefined(SetPaused(true))
          messageSender.reply(msg, "_paused_")
          updateStatus("paused")
      })
    commands += Command("unpause/resume/play", "resumes playing the song")((_, msg) => {
        case "unpause" | "resume" | "play" =>
          ap.setPaused(false)
          applyIfDefined(SetPaused(false))
          messageSender.reply(msg, "_unpaused_")
          updatePlayingTrack()
      })

    commands += Command("shuffle", "shuffles the playlist")((_, msg) => {
        case "shuffle" =>
          Playlist.shuffle()
          messageSender.reply(msg, "_shuffled_")
      })

    commands += Command("skip", "skips this song")((_, msg) => {
        case "skip" =>
          Playlist.skip()
          ap.getPlayingTrack match {
            case null =>
              messageSender.reply(msg, "_end of playlist_")
            case song => messageSender.reply(msg, s"_skipped to ${song.getInfo.title}_")
          }
      })
    commands += Command("skip to <index>", "skips to the specified song")((_, msg) => {
        case gr"skip to $where(.+)" => where match {
            case gr"""$num(\d+)""" =>
              if (Playlist.size == 0) messageSender.reply(msg, "There is nothing to skip to. Try adding some songs to the playlist with the `add` command.")
              else if (num == 0) messageSender.reply(msg, "Already playing that song")
              else {
                val dest = math.min(num.toInt, Playlist.size) - 1
                val song = Playlist.tracks.keys.drop(dest).head
                (0 until dest) foreach (i => Playlist.remove(0))
                Playlist.skip()
                messageSender.reply(msg, "_skipping to " + song.getInfo.title + "_")
              }
            case other => messageSender.reply(msg, "skip to command only accepts a number")
          }
      })

    //  commands += Command("add livestream <url>", "Adds a livestream to the playlist. Note that duration of this is unknown.")((_, msg) => {
    //      case regex"""add livestream (\w+://[^ ]+)$url(?: (.+))?$options""" =>
    //        messageSender.reply(msg, "_adding " + url + " with options " + options + "_")
    //        LiveStreamTrack.fetchMetadata(url, Option(options)).map { song =>
    //          ap.queue(LiveStreamTrack(clargs.encoder, song, Option(options), error => messageSender.reply(msg, "_" + error + "_")))
    //          messageSender.reply(msg, "_added " + url + " to the queue._")
    //
    //          ensureNextTrackIsCached(ap) //if we just added a song after the currently playing, ensure it starts fetching it
    //        }.failed.foreach(e => messageSender.reply(msg, s"Failed: $e"))
    //    })

    commands += Command("add <urls...>", "Adds the given song(s) to the queue.")((gd, msg) => {
        case gr"""add $url(.+)""" => url.split("\\s+") foreach {
            case gr"$url(https?://.+|)" =>
              messageSender.reply(msg, "_adding <" + url + "> _")
              audioPlayerManager.loadItemOrdered(ap, url, audioLoader(Right(msg)))
        
            case gr"file://$path(.+)" if clargs.musicDirectory.isDefined =>
              val location = new java.io.File(java.net.URI.create(s"file://${clargs.musicDirectory.get}/$path")).toScala
              if (location.exists) {
                if (location.isRegularFile) audioPlayerManager.loadItemOrdered(ap, location.toString, audioLoader(Right(msg)))
                else location.listRecursively.filter(_.isRegularFile) foreach (f => audioPlayerManager.loadItemOrdered(ap, f.toString, audioLoader(Right(msg))))
              } else {
                messageSender.reply(msg, s"Sorry, no file $location")
              }
              audioPlayerManager.loadItemOrdered(ap, s"local:${clargs.musicDirectory.get}/$path", audioLoader(Right(msg)))
            case other => messageSender.reply(msg, s"for now I only support youtube and web links. Sorry. [$other]")
          }
      })
    def audioLoader(origin: Channel Either Message) = new AudioLoadResultHandler {
      override def trackLoaded(track: AudioTrack) = {
        Playlist.queue(track, origin)
        origin.fold(c => messageSender.send(c, _: String), m => messageSender.reply(m, _: String))("*added " + track.getInfo.title + " to the queue.*")
      }
      override def playlistLoaded(playlist: AudioPlaylist) = {
        playlist.getTracks.asScala foreach (Playlist.queue(_, origin))
        val num = playlist.getTracks.size
        origin.fold(c => messageSender.send(c, _: String), m => messageSender.reply(m, _: String))(s"_added $num songs to the queue._")
      }
      override def noMatches = origin.fold(c => messageSender.send(c, _: String), m => messageSender.reply(m, _: String))("Sorry, I couldn't find anything with that url.")
      override def loadFailed(ex: FriendlyException) = {
        origin.fold(c => messageSender.send(c, _: String), m => messageSender.reply(m, _: String))("Sorry, loading fail :(. " + ex)
        ex.printStackTrace()
      }
    }
    
    commands += Command("search[ and add first] <query>", "Search Youtube with the given query. Optionally, instead of returning the list, add the first entry to the queue.")((gd, msg) => {
        case gr"search $shouldAdd(and add first )?$query(.+)" =>
          messageSender.reply(msg, "*searching...*")
          YoutubeSearch(discordClient.ahc, query).onComplete {
            case scala.util.Success(seq) =>
              if (shouldAdd.isDefined) commands.find(_.name == "add <urls...>").get.action(gd, msg)("add " + seq.head._2)
              else messageSender.reply(msg, seq.take(10).map(e => e._1 + " - " + e._2).mkString("\n"))
            case scala.util.Failure(ex) => messageSender.reply(msg, s"Something went wrong: $ex")
          }
      })

    commands += Command("make playlist from <song name>", "Uses Spotalike.com to build a playlist given the starting song. The found songs are then added to the queue")((gd, msg) => {
        case gr"make playlist from $song(.+)" =>
          if (gd.processing.isDefined) {
            messageSender.reply(msg, "Sorry, I'm already processing a playlist request.")
          } else {
            messageSender.reply(msg, "*processing...*")
            val spotalikeResult = Await.ready(Spotalike.generate(discordClient.ahc, song), Duration.Inf)
            spotalikeResult.value.get match {
              case scala.util.Success(seq) =>
                val cancelProcessing = new SyncVar[Unit]()
                updateState(gd.copy(processing = Some(cancelProcessing)))
                new Thread(null, () => {
                    messageSender.reply(msg, s"*obtaining urls from youtube for\n${seq.mkString("\n")}...*")
                    val addCommand = commands.find(_.name == "add <urls...>").get
                    for (song <- seq if !cancelProcessing.isSet) {
                      println("Waiting on " + song)
                      val res = Await.ready(YoutubeSearch(discordClient.ahc, song), Duration.Inf)
                      res.value.get match {
                        case scala.util.Success(song) => addCommand.action(gd, msg)("add " + song.head._2)
                        case scala.util.Failure(ex) => messageSender.reply(msg, s"Something went wrong: $ex")
                      }
                    }
                    if (!cancelProcessing.isSet) updateState(gd.copy(processing = None))
                  }, "Processing playlist for " + song, 1024 * 200).start()
              case scala.util.Failure(ex) => messageSender.reply(msg, s"Something went wrong: $ex")
            }
          }
      })

    commands += Command("cancel processing", "Makes me cancel the playlist currently being processed (if any).")((gd, msg) => {
        case "cancel processing" =>
          gd.processing.fold[Unit] {
            messageSender.reply(msg, "nothing is being processed.")
          } { canceller =>
            canceller.put(())
            updateState(gd.copy(processing = None))
            messageSender.reply(msg, "cancelled.")
          }
      })

    commands += Command("clear", "Clears the remaining playlist.")((_, msg) => {
        case "clear" =>
          val numberOfTracks = Playlist.tracks.size
          Playlist.clear()
          messageSender.reply(msg, s"_$numberOfTracks tracks removed._")
      })

    commands += Command("remove range", "Removes all the song between the two specified indeces")((_, msg) => {
        case gr"""remove range $n1(\d+) $n2(\d+)""" =>
          val from = n1.toInt
          val to = math.min(n2.toInt, Playlist.size)
          if (from > to) messageSender.reply(msg, s"$from is greater than $to ... :sweat:")
          else {
            val from0Based = from - 1
            val removedTracks = for (i <- from0Based until to) yield {
              val track = Playlist.remove(from0Based) //removing from is on purpose
              track.getInfo.title
            }
            messageSender.reply(msg, "_removed:_")
            removedTracks foreach (t => messageSender.reply(msg, "_" + t + "_"))
          }

        case gr"""remove range .*""" => messageSender.reply(msg, "I'm sorry, remove range only accepts a pair of naturals")
      })
    commands += Command("remove <index>", "Removes the specified index from the queue. (Use list to check first)")((_, msg) => {
        case gr"remove $what(.+)" =>
          what match {
            case gr"""$n(\d+)""" =>
              val num = n.toInt
              if (num < 0) messageSender.reply(msg, "A negative number? :sweat:")
              else if (Playlist.size == 0) messageSender.reply(msg, "The playlist is empty.")
              else if (num > Playlist.size) messageSender.reply(msg, s"$num is larger than the playlist's size, you meant to remove the last one? it's ${Playlist.size}")
              else if (num == 0) Playlist.skip()
              else {
                val track = Playlist.remove(num - 1)
                messageSender.reply(msg, s"_ removed ${track.getInfo.title}_")
              }

            case other =>

              Playlist.tracks.keys.find(t => t.getInfo.title == other) match {
                case Some(track) =>
                  Playlist.remove(track)
                  messageSender.reply(msg, s"_ removed ${track.getInfo.title}_")
                case _ => messageSender.reply(msg, s"Sorry, there is no song named `$other`")
              }
          }
      })

    commands += Command("rejoin", s"Makes me rejoin the voice channel ${clargs.channel} (as sometimes Discord bugs out).")((_, msg) => {
        case "rejoin" => applyIfDefined(RejoinVoiceChannel)
      })

    commands += Command("subscribe", "(bot only) Subscribes to status updates via DM.")((gd, msg) => {
        case gr"subscribe$who(?: (.*))?" =>
          who.fold(Option(msg.author))(who => gd.guild.members.find(_.user.id == who.toLong).map(_.user)) match {
            case Some(user) => 
              messageSender.reply(msg, "subscribed")
              if (!gd.subscribers.exists(_.msg.author.id == user.id))
                updateState(gd.copy(subscribers = gd.subscribers :+ Subscription(msg.copy(author = user))))
            case _ =>
              messageSender.reply(msg, s"Could not find user $who")
          }
      })
    commands += Command("unsubscribe", "(bot only) Unsubscribes from status updates via DM.")((gd, msg) => {
        case "unsubscribe" =>
          messageSender.reply(msg, "unsubscribed")
          updateState(gd.copy(subscribers = gd.subscribers.filterNot(_.msg.author.id == msg.author.id)))
      })
    
    commands += Command("track <userId>|stop", "I'll play on the audio channel whatever a given user is listening to in spotify.")((gd, msg) => {
        case gr"""track $user(\d+)""" =>
          val targetId = Snowflake(user)
          gd.guild.members.find(_.user.id == targetId) match {
            case Some(member) =>
              val userName = member.nick.getOrElse(member.user.userName)
              updateState(gd.copy(tracking = Some(Tracking(targetId, msg.channelId))))
              messageSender.reply(msg, s"tracking $userName")
              
            case None => messageSender.reply(msg, s"no member with id $user found")
          }
        
        case "track stop" => 
          updateState(gd.copy(tracking = None))
          messageSender.reply(msg, s"I'm no longer tracking anyone.")
      })
    

    commands += Command("help", "Prints this message")((_, msg) => {
        case "help" =>
          val maxCmdWidth = commands.map(_.name.length).max
          val helpString = new StringBuilder
          commands foreach (c => helpString.append(c.name.padTo(maxCmdWidth, ' ')).append(" - ").append(c.description).append("\n"))
          messageSender.reply(msg, "```\n" + helpString.toString + "```")
      })

  }

  val discordClient = new DiscordClient("Bot " + clargs.discordToken, DiscordHandlerSM)
  val messageSender = new DiscordRateHonoringSender(discordClient) {
    override def send(channel: headache.Channel, msg: String): Future[Unit] = {
      super.send(channel, msg) andThen logErrors
    }
    override def send(channelId: Snowflake, msg: String): Future[Unit] = {
      super.send(channelId, msg) andThen logErrors
    }
    override def reply(to: Message, msg: String): Future[Unit] = {
      super.reply(to, msg) andThen logErrors
    }
    val logErrors: PartialFunction[scala.util.Try[_], Any] = {
      case scala.util.Failure(ex) => println("failed senging message due to: " + ex)
    }
  }

  def millisToString(millis: Long) = {
    val seconds = millis / 1000
    val hours = if (seconds >= 3600) (seconds / 3600) + ":" else ""
    f"${hours}${(seconds % 3600) / 60}%02d:${seconds % 60}%02d"
  }

  discordClient.login()
}
