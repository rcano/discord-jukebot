package jukebox

import java.util.concurrent.LinkedBlockingQueue

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util.Try

import sx.blah.discord.Discord4J
import sx.blah.discord.api.{ClientBuilder => DiscordClientBuilder, events}, events.{Event, IListener}
import sx.blah.discord.handle.impl.events.{DiscordDisconnectedEvent, DiscordReconnectedEvent, MessageReceivedEvent, ReadyEvent}
import sx.blah.discord.handle.obj.{IGuild, IMessage, IUser, Status}
import sx.blah.discord.util.audio.AudioPlayer
import sx.blah.discord.util.audio.events.{TrackStartEvent, TrackSkipEvent, ShuffleEvent, TrackFinishEvent}

import RegexExtractor._

object Bot extends App {

  case class Clargs(discordToken: String = null, channel: String = null, encoder: String = null)
  val clargs = new scopt.OptionParser[Clargs]("discord-jukebox") {
    opt[String]('t', "token").action((x, c) => c.copy(discordToken = x)).required.text("discord bot token")
    opt[String]('c', "channel").action((x, c) => c.copy(channel = x)).required.text("discord voice channel")
    opt[String]("encoding-tool").action((x, c) => c.copy(encoder = x)).required.text("path to binary that will perform encoding (e.g.: ffmpeg or avconv)")
  }.parse(args, new Clargs()).getOrElse(sys.exit(0))

  val discordClient = new DiscordClientBuilder().withToken(clargs.discordToken).withPingTimeout(1).withTimeout(2000).login()

//  val logger = Discord4J.LOGGER.asInstanceOf[Discord4J.Discord4JLogger]
//  logger.setLevel(Discord4J.Discord4JLogger.Level.DEBUG)

  /*
   * STATE MACHINE
   */
  discordClient.getDispatcher.registerListener(new IListener[Event] {
      /**
       * my mention string.
       */
      var me: String = _

      /**
       * this variable holds the user that requested the currently processing playlist, as well as a handle to stop it.
       */
      var processingPlaylist: Option[(IUser, SyncVar[Unit])] = None

      val messageSender = new DiscordRateHonoringSender(_.printStackTrace())

      val commands = collection.mutable.ListBuffer[Command]()
      case class Command(name: String, description: String)(val action: (IMessage, AudioPlayer) => String PartialFunction Any)

      commands += Command("status", "shows what I'm currently playing and pending.")((msg, ap) => {
          case "status" =>
            messageSender.reply(msg, if (ap.getCurrentTrack == null) "Nothing is being played."
                                else {
                val metadata = SongMetadata.fromMetadata(ap.getCurrentTrack.getMetadata)
                val transcurred = (ap.getCurrentTrack.getCurrentTrackTime / 1000).toInt
                s"Currently playing ${metadata.name} - ${secondsToString(transcurred)}/${secondsToString(metadata.length.getOrElse(0))}. ${ap.getPlaylistSize - 1} remaining tracks.\n${metadata.origin}"
              })
        })

      commands += Command("list[ full]", "shows the remaining list of songs to be played.")((msg, ap) => {
          case "list" | "list full" if ap.getPlaylistSize == 0 => messageSender.reply(msg, "Nothing in the queue" )
          case cmd@ ("list" | "list full") =>
            val full = cmd.endsWith("full")
            val songs = ap.getPlaylist.asScala map (t => SongMetadata.fromMetadata(t.getMetadata))
            val songsInfo = songs.zipWithIndex.map(e => e._2 + ": " + e._1.name +
                                                   (if (full) " - " + e._1.origin else ""))
            val totalTime = songs.map(_.length.getOrElse(0)).sum
            messageSender.reply(msg, s"Playlist total time ${secondsToString(totalTime)}")
            songsInfo foreach (messageSender.reply(msg, _))
        })

      commands += Command("pause/stop", "pause the currently playing song")((msg, ap) => {
          case "pause" | "stop" =>
            ap.setPaused(true)
            messageSender.reply(msg, "_paused_")
            discordClient changeStatus Status.game("paused")
        })
      commands += Command("unpause/resume/play", "resumes playing the song")((msg, ap) => {
          case "unpause" | "resume" | "play" =>
            ap.setPaused(false)
            messageSender.reply(msg, "_unpaused_")
            showCurrentlyPlaying(ap)
        })

      commands += Command("shuffle", "shuffles the playlist")((msg, ap) => {
          case "shuffle" =>
            val wasPaused = ap.isPaused
            if (!wasPaused) ap.togglePause()
            val playlist = ap.getPlaylist.asScala
            val shuffled = scala.util.Random.shuffle(playlist.drop(1))
            playlist.headOption foreach { h =>
              playlist.clear()
              playlist += h
              playlist ++= shuffled
            }
            if (!wasPaused) ap.togglePause()
            discordClient.getDispatcher().dispatch(new ShuffleEvent(ap))
            messageSender.reply(msg, "_shuffled_")
            ensureNextTrackIsCached(ap)
        })

      commands += Command("skip", "skips this song")((msg, ap) => {
          case "skip" =>
            val track = ap.getCurrentTrack()
            ap.skip()
            val p = track.getProvider.asInstanceOf[java.io.Closeable]
            Try(p.close())
            ap.getCurrentTrack match {
              case null => 
                messageSender.reply(msg, "_end of playlist_")
                discordClient changeStatus Status.empty
              case song => messageSender.reply(msg, s"_skipped to ${SongMetadata.fromMetadata(song.getMetadata).name}_")
            }
        })
      commands += Command("skip to <index>", "skips to the specified song")((msg, ap) => {
          case regex"skip to (.+)$where" => where match {
              case regex"""(\d+)$num""" =>
                if (ap.getPlaylistSize == 0) messageSender.reply(msg, "There is nothing to skip to. Try adding some songs to the playlist with the `add` command.")
                else {
                  val playlist = ap.getPlaylist
                  val dest = math.min(num.toInt, playlist.size - 1)
                  val song = playlist.get(dest)
                  //skip manually so as to avoid ap.skipTo logic of calling ready on the InputProvider (wihch in turn causes our logic to fetch the video)
                  for (_ <- 0 until dest) {
                    val track = playlist.remove(0)
                    val p = track.getProvider.asInstanceOf[java.io.Closeable]
                    Try(p.close())
                    discordClient.getDispatcher.dispatch(new TrackSkipEvent(ap, track))
                  }
                  messageSender.reply(msg, "_skipping to " + SongMetadata.fromMetadata(song.getMetadata).name + "_")
                }
              case other => messageSender.reply(msg, "skip to command only accepts a number")
            }
        })

      commands += Command("add livestream <url>", "Adds a livestream to the playlist. Note that duration of this is unknown.")((msg, ap) => {
          case regex"""add livestream (\w+://[^ ]+)$url(?: (.+))?$options""" =>
            messageSender.reply(msg, "_adding " + url + " with options " + options + "_")
            LiveStreamTrack.fetchMetadata(url, Option(options)).map { song =>
              ap.queue(LiveStreamTrack(clargs.encoder, song, Option(options), error => messageSender.reply(msg, "_" + error + "_")))
              messageSender.reply(msg, "_added " + url + " to the queue._")

              ensureNextTrackIsCached(ap) //if we just added a song after the currently playing, ensure it starts fetching it
            }.failed.foreach(e => messageSender.reply(msg, s"Failed: $e"))
        })

      commands += Command("add <url>", "Adds the given song to the queue.")((msg, ap) => {
          case regex"add (.+)$url" => url match {
              case regex"(https?://.+|)$url" =>
                processingPlaylist.fold {

                  messageSender.reply(msg, "_adding " + url + "_")
                  val errorReporter: SongMetadata => Throwable => Unit = song => t => {
                    t.printStackTrace()
                    messageSender.reply(msg, s"There was an error when downloading ${song.name}: $t")
                    commands.find(_.name == "skip").foreach(_.action(msg, ap).apply("skip")) //invoke skip
                  }
                  YoutubeProvider.fetchInformation(url,
                                                   error => messageSender.reply(msg, "_" + error + "_"),
                                                   i => if (i > 0 && i % 20 == 0) messageSender.reply(msg, s"_processed $i videos..._")).
                  map {
                    case (1, _, res) =>
                      val song = res.value.get.get.head
                      ap.queue(LazyTrack(clargs.encoder, song, errorReporter(song)))
                      messageSender.reply(msg, "_added " + res.value.get.get.head.name + " to the queue._")

                      ensureNextTrackIsCached(ap) //if we just added a song after the currently playing, ensure it starts fetching it
                    case (num, cancel, playlistF) =>
                      processingPlaylist = Some((msg.getAuthor, cancel))
                      messageSender.reply(msg, s"_processing $num videos in the playlist._")
                      playlistF.onComplete { res =>
                        processingPlaylist = None
                        res match {
                          case scala.util.Success(playlist) => messageSender.reply(msg, s"_added $num songs to the queue._")
                            playlist foreach (s => ap.queue(LazyTrack(clargs.encoder, s, errorReporter(s))))
                            ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                          case scala.util.Failure(YoutubeProvider.CancelledException) => messageSender.reply(msg, s"_work cancelled_")
                          case scala.util.Failure(e) => messageSender.reply(msg, s"Failed processing $url: $e.")
                        }
                      }(mainThreadExecutionContext)
                  }.failed.foreach(e => messageSender.reply(msg, s"Failed: $e"))

                }{ case (owner, _) => messageSender.reply(msg, s"Sorry, I'm still processing a playlist on ${owner.mention} 's behalf. Please try again later.")}


              case other => messageSender.reply(msg, "for now I only support youtube links. Sorry.")
            }
        })


      commands += Command("remove range", "Removes all the song between the two specified indeces")((msg, ap) => {
          case regex"""remove range (\d+)$n1 (\d+)$n2""" =>
            val from = n1.toInt
            val to = n2.toInt
            if (from > to) messageSender.reply(msg, s"$from is greater than $to ... :sweat:")
            else if (to > ap.getPlaylistSize) messageSender.reply(msg, s"Please remove songs with indices **within** the list (the maximum upper bound is currently ${ap.getPlaylistSize}).")
            else {
              val removedTracks = for (i <- from until to) yield {
                val track = ap.getPlaylist.remove(from) //removing from is on purpose
                val p = track.getProvider.asInstanceOf[java.io.Closeable]
                Try(p.close())
                val title = SongMetadata.fromMetadata(track.getMetadata).name
                title
              }
              messageSender.reply(msg, "_removed:_")
              removedTracks foreach (t => "_" + t + "_")
              ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
              if (ap.getPlaylistSize == 0) discordClient changeStatus Status.empty
            }

          case regex"""remove range .*""" => messageSender.reply(msg, "I'm sorry, remove range only accepts a pair of naturals")
        })
      commands += Command("remove <index>", "Removes the specified index from the queue. (Use list to check first)")((msg, ap) => {
          case regex"remove (.+)$what" =>
            what match {
              case regex"""(\d+)$n""" =>
                val num = n.toInt
                if (num < 0) messageSender.reply(msg, "A negative number? :sweat:")
                else if (ap.getPlaylistSize == 0) messageSender.reply(msg, "The playlist is empty.")
                else if (num >= ap.getPlaylistSize) messageSender.reply(msg, s"$num is larger than the playlist's size, you meant to remove the last one? it's ${ap.getPlaylistSize - 1}")
                else if (num == 0) ap.skip()
                else {
                  val track = ap.getPlaylist.remove(num)
                  val p = track.getProvider.asInstanceOf[java.io.Closeable]
                  Try(p.close())
                  val title = SongMetadata.fromMetadata(track.getMetadata).name
                  messageSender.reply(msg, s"_ removed ${title}_")
                  ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                }

              case other =>

                ap.getPlaylist.asScala.zipWithIndex.find(t => SongMetadata.fromMetadata(t._1.getMetadata).name == other) match {
                  case Some((track, idx)) =>
                    ap.getPlaylist.remove(idx)
                    val p = track.getProvider.asInstanceOf[java.io.Closeable]
                    Try(p.close())
                    val title = SongMetadata.fromMetadata(track.getMetadata).name
                    messageSender.reply(msg, s"_ removed ${title}_")
                    ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                  case _ => messageSender.reply(msg, s"Sorry, there is no song named `$other`")
                }
            }
            if (ap.getPlaylistSize == 0) discordClient changeStatus Status.empty
        })

      commands += Command("cancel", "Makes me cancel the processing of a playlist that someone requested.")((msg, ap) => {
          case "cancel" if processingPlaylist.isEmpty => messageSender.reply(msg, "I'm not processing anything, so there is nothing to cancel.")
          case "cancel" =>
            val (user, c) = processingPlaylist.get
            c.put(())
            messageSender.send(msg.getChannel, s"Cancelling ${user.mention} 's work as per ${msg.getAuthor.mention} 's request.")
        })


      commands += Command("join", s"Makes me join the voice channel ${clargs.channel}")((msg, ap) => {
          case "join" => discordClient.getGuilds.asScala.flatMap(_.getVoiceChannelsByName(clargs.channel).asScala.headOption) foreach { channel =>
              channel.join()
            }
        })
      commands += Command("leave", s"Makes me leave the voice channel ${clargs.channel}")((msg, ap) => {
          case "leave" => discordClient.getGuilds.asScala.flatMap(_.getVoiceChannelsByName(clargs.channel).asScala.headOption) foreach { channel =>
              channel.leave()
            }
        })
      commands += Command("kill yourself", s"Makes me die.")((msg, ap) => {
          case "kill yourself" => 
            messageSender.reply(msg, "Bye bye.")
            sys.exit(0)
        })

      commands += Command("help", "Prints this message")((msg, ap) => {
          case "help" =>
            val maxCmdWidth = commands.map(_.name.length).max
            val helpString = new StringBuilder
            commands foreach (c => helpString.append(c.name.padTo(maxCmdWidth, ' ')).append(" - ").append(c.description).append("\n"))
            messageSender.reply(msg, "```\n" + helpString.toString + "```")
        })


      def handle(e) = Future { //process all events in the main thread, so that no regard to memory cohesion has to be paid
        try {

          e match {
            case re: ReadyEvent if discordClient.getGuilds.isEmpty =>
              println("bot is not registered to any guild")
              Thread.currentThread.interrupt()

            case _: ReadyEvent | _: DiscordReconnectedEvent =>
              me = s"<@${discordClient.getOurUser.getID}>"
              println("I am " + me)
              discordClient.getGuilds.asScala.flatMap(_.getVoiceChannelsByName(clargs.channel).asScala.headOption) foreach { channel =>
                println(s"Guild ${channel.getGuild} has a channel ${clargs.channel}. Joining.")
                Try(channel.leave())
                channel.join()
              }

            case _: DiscordDisconnectedEvent =>
              println(Console.RED + "disconnected from Discord, attempting to reconnect...")
              discordClient.login()


              /*
               * handle commands
               */
            case msgEvt: MessageReceivedEvent if msgEvt.getMessage.getContent startsWith me =>
              val msg = msgEvt.getMessage
              val guild = msg.getGuild match {
                case null => //try to detect which guild the bot shares with the user, if there are many, pick the first
                  discordClient.getGuilds.asScala.find(_.getUserByID(msg.getAuthor.getID) != null)
                case guild => Some(guild)
              }
              guild match {
                case None => messageSender.reply(msg, "I could not identify the guild we share, sorry.")
                case Some(guild) =>
                  val ap = AudioPlayer.getAudioPlayerForGuild(guild)

                  val cmd = msg.getContent.stripPrefix(me).replaceFirst("^[,:]?\\s+", "")
                  commands.find(_.action(msg, ap).isDefinedAt(cmd)) match {
                    case Some(command) => command.action(msg, ap)(cmd)
                    case _ => messageSender.reply(msg, s"Sorry, I don't know the command: $cmd")

                  }
              }


              /*
               * react to audio player events
               */
            case e: TrackStartEvent =>
              println("track started")
              //when a track finishes playing, start caching the one after the current.
              ensureNextTrackIsCached(e.getPlayer)
              showCurrentlyPlaying(e.getPlayer)
              
              
            case e: TrackFinishEvent =>
              if (e.getPlayer.getCurrentTrack == null) discordClient changeStatus Status.empty
            

            case _ =>
          }
        } catch {
          case e: Exception => e.printStackTrace(Console.err)
        }
      }(mainThreadExecutionContext)

      def ensureNextTrackIsCached(ap: AudioPlayer) = {
        ap.getPlaylist.asScala.drop(1).headOption.foreach { track =>
          new Thread() {
            override def run() = {
              println("Precaching next track: " + SongMetadata.fromMetadata(track.getMetadata).name)
              track.isReady() //this causes the lazy stream to be fetch
            }
          }.start()
        }
      }

      def showCurrentlyPlaying(ap: AudioPlayer) = ap.getCurrentTrack match {
        case null => discordClient changeStatus Status.empty
        case track =>
          val metadata = SongMetadata.fromMetadata(track.getMetadata)
          discordClient changeStatus Status.stream(metadata.name, metadata.origin.asInstanceOf[String])
      }
    })

  def secondsToString(seconds: Int) = {
    val hours = if (seconds >= 3600) (seconds / 3600) + ":" else ""
    f"${hours}${(seconds % 3600) / 60}%02d:${seconds % 60}%02d"
  }

  val connectionChecker = new Thread("Network checker") {
    import java.net._
    var connectedState = true
    override def run = {
      //check every 500 to see if the newtork is up, if it isn't, disconnect the client and reconnect it when it gets back
      while (!Thread.interrupted) {
        try {
          val noLoopbackExists = NetworkInterface.getNetworkInterfaces.asScala.toArray.exists(n => !n.isLoopback && n.isUp)
          if (noLoopbackExists) {
            if (!connectedState) {
              println(Console.YELLOW + "reconning the client..." + Console.RESET)
              discordClient.login()
              connectedState = true
            }
          } else if (connectedState) {
            println(Console.YELLOW + "disconnecting the client on network down..." + Console.RESET)
            connectedState = false
            if (discordClient.isLoggedIn) discordClient.logout
          }
        } catch {
          case e: Exception => e.printStackTrace()
        }
        Thread.sleep(500)
      }
    }
  }
  connectionChecker.setDaemon(true)
  connectionChecker.start()
  /**
   * make an execution context out of the main thread.
   */
  object mainThreadExecutionContext extends ExecutionContext {
    val pendingTasks = new LinkedBlockingQueue[Runnable](200)
    def execute(r): Unit = pendingTasks put r
    def reportFailure(cause) = cause.printStackTrace(Console.err)
  }
  //cause main thread to enter loop mode
  while (!Thread.interrupted) {
    Try(mainThreadExecutionContext.pendingTasks.take().run()).failed.foreach(_.printStackTrace(Console.err))
  }
  println(Console.RED + "main thread was interrupting, aborting" + Console.RESET)
  Try(discordClient.logout)
  sys.exit(1)
}
