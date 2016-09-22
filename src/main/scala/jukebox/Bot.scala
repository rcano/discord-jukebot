package jukebox

import java.util.concurrent.LinkedBlockingQueue

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util.Try

import sx.blah.discord.api.{ClientBuilder => DiscordClientBuilder, events}, events.{Event, IListener}
import sx.blah.discord.handle.impl.events.{DiscordReconnectedEvent, MessageReceivedEvent, ReadyEvent}
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

  val discordClient = new DiscordClientBuilder().withToken(clargs.discordToken).login()


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

      val commands = collection.mutable.ListBuffer[Command]()
      case class Command(name: String, description: String)(val action: (IMessage, AudioPlayer) => String PartialFunction Any)

      commands += Command("status", "shows what I'm currently playing and pending.")((msg, ap) => {
          case "status" =>
            msg.reply(
              if (ap.getCurrentTrack == null) "Nothing is being played."
              else {
                val metadata = ap.getCurrentTrack.getMetadata
                val title = metadata.get("title")
                val duration = metadata.get("duration").asInstanceOf[Int]
                val origin = metadata.get("origin")
                val transcurred = (ap.getCurrentTrack.getCurrentTrackTime / 1000).toInt
                s"Currently playing ${title} - ${secondsToString(transcurred)}/${secondsToString(duration)}. ${ap.getPlaylistSize - 1} remaining tracks.\n$origin"
              }
            )
        })

      commands += Command("list[ full]", "shows the remaining list of songs to be played.")((msg, ap) => {
          case "list" | "list full" if ap.getPlaylistSize == 0 => msg.reply("Nothing in the queue" )
          case cmd@ ("list" | "list full") =>
            val full = cmd.endsWith("full")
            val songs = ap.getPlaylist.asScala map (t => SongMetadata.fromMetadata(t.getMetadata))
            val Seq(head, tail@_*) = songs.zipWithIndex.map(e => e._2 + ": " + e._1.name +
                                                            (if (full) " - " + e._1.origin else "")).grouped(20).toVector
            val totalTime = songs.map(_.length).sum
            msg.reply(s"Playlist total time ${secondsToString(totalTime)} :\n" + head.mkString("\n"))
            for (s <- tail) {
              Thread.sleep(200)
              msg.reply(s.mkString("\n"))
            }
        })

      commands += Command("pause/stop", "pause the currently playing song")((msg, ap) => {
          case "pause" | "stop" =>
            ap.setPaused(true)
            msg.reply("_paused_")
            discordClient changeStatus Status.game("paused")
        })
      commands += Command("unpause/resume/play", "resumes playing the song")((msg, ap) => {
          case "unpause" | "resume" | "play" =>
            ap.setPaused(false)
            msg.reply("_unpaused_")
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
            msg.reply("_shuffled_")
            ensureNextTrackIsCached(ap)
        })

      commands += Command("skip", "skips this song")((msg, ap) => {
          case "skip" =>
            ap.skip()
            ap.getCurrentTrack match {
              case null => msg.reply("_end of playlist_")
              case song => msg.reply(s"_skipped to ${song.getMetadata.get("title")}_")
            }
        })
      commands += Command("skip to <index>", "skips to the specified song")((msg, ap) => {
          case regex"skip to (.+)$where" => where match {
              case regex"""(\d+)$num""" =>
                if (ap.getPlaylistSize == 0) msg.reply("There is nothing to skip to. Try adding some songs to the playlist with the `add` command.")
                else {
                  val playlist = ap.getPlaylist
                  val dest = math.min(num.toInt, playlist.size - 1)
                  val song = playlist.get(dest)
                  //skip manually so as to avoid ap.skipTo logic of calling ready on the InputProvider (wihch in turn causes our logic to fetch the video)
                  for (_ <- 0 until dest) {
                    val track = playlist.remove(0)
                    val p = track.getProvider.asInstanceOf[LazyTrack.LazyAudioProvider]
                    Try(p.close())
                    discordClient.getDispatcher.dispatch(new TrackSkipEvent(ap, track))
                  }
                  msg.reply("_skipping to " + song.getMetadata.get("title") + "_")
                }
              case other => msg.reply("skip to command only accepts a number")
            }
        })


      commands += Command("add <url>", "Addes the given song to the queue.")((msg, ap) => {
          case regex"add (.+)$url" => url match {
              case regex"(https?://www.youtube.com.+)$url" =>
                processingPlaylist.fold {

                  msg.reply("_adding " + url + "_")
                  val errorReporter: SongMetadata => Throwable => Unit = song => t => {
                    msg.reply(s"There was an error when downloading ${song.name}: $t")
                    commands.find(_.name == "skip").foreach(_.action(msg, ap).apply("skip")) //invoke skip
                  }
                  YoutubeProvider.fetchInformation(url,
                                                   error => msg.reply("_" + error + "_"),
                                                   i => if (i > 0 && i % 20 == 0) msg.reply(s"_processed $i videos..._")).
                  map {
                    case (1, _, res) =>
                      val song = res.value.get.get.head
                      ap.queue(LazyTrack(clargs.encoder, song, errorReporter(song)))
                      msg.reply("_added " + res.value.get.get.head.name + " to the queue._")

                      ensureNextTrackIsCached(ap) //if we just added a song after the currently playing, ensure it starts fetching it
                    case (num, cancel, playlistF) =>
                      processingPlaylist = Some((msg.getAuthor, cancel))
                      msg.reply(s"_processing $num videos in the playlist._")
                      playlistF.onComplete { res =>
                        processingPlaylist = None
                        res match {
                          case scala.util.Success(playlist) => msg.reply(s"_added $num songs to the queue._")
                            playlist foreach (s => ap.queue(LazyTrack(clargs.encoder, s, errorReporter(s))))
                            ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                          case scala.util.Failure(YoutubeProvider.CancelledException) => msg.reply(s"_work cancelled_")
                          case scala.util.Failure(e) => msg.reply(s"Failed processing $url: $e.")
                        }
                      }(mainThreadExecutionContext)
                  }.failed.foreach(_.printStackTrace(Console.err))

                }{ case (owner, _) => msg.reply(s"Sorry, I'm still processing a playlist on ${owner.mention} 's behalf. Please try again later.")}


              case other => msg.reply("for now I only support youtube links. Sorry.")
            }
        })


      commands += Command("remove range", "Removes all the song between the two specified indeces")((msg, ap) => {
          case regex"""remove range (\d+)$n1 (\d+)$n2""" =>
            val from = n1.toInt
            val to = n2.toInt
            if (from > to) msg.reply(s"$from is greater than $to ... :sweat:")
            else if (to > ap.getPlaylistSize) msg.reply(s"Please remove songs with indices **within** the list (the maximum upper bound is currently ${ap.getPlaylistSize}).")
            else {
              val removedTracks = for (i <- from until to) yield {
                val track = ap.getPlaylist.remove(from) //removing from is on purpose
                val p = track.getProvider.asInstanceOf[LazyTrack.LazyAudioProvider]
                Try(p.close())
                val title = track.getMetadata.get("title")
                title
              }
              removedTracks.grouped(20) foreach { tracks =>
                msg.reply("_removed:\n" + tracks.mkString("\n"))
                Thread.sleep(200)
              }
              ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
            }

          case regex"""remove range .*""" => msg.reply("I'm sorry, remove range only accepts a pair of naturals")
        })
      commands += Command("remove <index>", "Removes the specified index from the queue. (Use list to check first)")((msg, ap) => {
          case regex"remove (.+)$what" => what match {
              case regex"""(\d+)$n""" =>
                val num = n.toInt
                if (num < 0) msg.reply("A negative number? :sweat:")
                else if (ap.getPlaylistSize == 0) msg.reply("The playlist is empty.")
                else if (num >= ap.getPlaylistSize) msg.reply(s"$num is larger than the playlist's size, you meant to remove the last one? it's ${ap.getPlaylistSize - 1}")
                else if (num == 0) ap.skip()
                else {
                  val track = ap.getPlaylist.remove(num)
                  val p = track.getProvider.asInstanceOf[LazyTrack.LazyAudioProvider]
                  Try(p.close())
                  val title = track.getMetadata.get("title")
                  msg.reply(s"_ removed ${title}_")
                  ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                }

              case other =>
                ap.getPlaylist.asScala.zipWithIndex.find(_._1.getMetadata.get("title") == other) match {
                  case Some((track, idx)) =>
                    ap.getPlaylist.remove(idx)
                    val p = track.getProvider.asInstanceOf[LazyTrack.LazyAudioProvider]
                    Try(p.close())
                    val title = track.getMetadata.get("title")
                    msg.reply(s"_ removed ${title}_")
                    ensureNextTrackIsCached(ap) //make sure the song after the currently playing is cached.
                  case _ => msg.reply(s"Sorry, there is no song named `$other`")
                }
            }
        })

      commands += Command("cancel", "Makes me cancel the processing of a playlist that someone requested.")((msg, ap) => {
          case "cancel" if processingPlaylist.isEmpty => msg.reply("I'm not processing anything, so there is nothing to cancel.")
          case "cancel" =>
            val (user, c) = processingPlaylist.get
            c.put(())
            msg.getChannel.sendMessage(s"Cancelling ${user.mention} 's work as per ${msg.getAuthor.mention} 's request.")
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

      commands += Command("help", "Prints this message")((msg, ap) => {
          case "help" =>
            val maxCmdWidth = commands.map(_.name.length).max
            val helpString = new StringBuilder
            commands foreach (c => helpString.append(c.name.padTo(maxCmdWidth, ' ')).append(" - ").append(c.description).append("\n"))
            msg.reply("```\n" + helpString.toString + "```")
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


              /*
               * handle commands
               */
            case msgEvt: MessageReceivedEvent if msgEvt.getMessage.getContent startsWith me =>
              val msg = msgEvt.getMessage
              val ap = AudioPlayer.getAudioPlayerForGuild(msg.getChannel.getGuild)

              val cmd = msg.getContent.stripPrefix(me).replaceFirst("^[,:]?\\s+", "")
              commands.find(_.action(msg, ap).isDefinedAt(cmd)) match {
                case Some(command) => command.action(msg, ap)(cmd)
                case _ => msg.reply(s"Sorry, I don't know the command: $cmd")

              }


              /*
               * react to audio player events
               */
            case e: TrackStartEvent =>
              println("track started")
              //when a track finishes playing, start caching the one after the current.
              ensureNextTrackIsCached(e.getPlayer)
              showCurrentlyPlaying(e.getPlayer)
              
              
            case e: TrackFinishEvent => discordClient changeStatus Status.empty
            

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
              println("Precaching next track: " + track.getMetadata.get("title"))
              track.isReady() //this causes the lazy stream to be fetch
            }
          }.start()
        }
      }

      def showCurrentlyPlaying(ap: AudioPlayer) = ap.getCurrentTrack match {
        case null => discordClient changeStatus Status.empty
        case track =>
          val metadata = track.getMetadata
          discordClient changeStatus Status.stream(metadata.get("title").asInstanceOf[String], metadata.get("origin").asInstanceOf[String])
      }
    })

  def secondsToString(seconds: Int) = {
    val hours = if (seconds >= 3600) (seconds / 3600) + ":" else ""
    f"${hours}${(seconds % 3600) / 60}%02d:${seconds % 60}%02d"
  }
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
