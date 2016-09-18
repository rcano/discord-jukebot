package jukebox

import java.util.concurrent.LinkedBlockingQueue

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util.Try

import sx.blah.discord.api.{ClientBuilder => DiscordClientBuilder, events}, events.{Event, IListener}
import sx.blah.discord.handle.impl.events.{DiscordReconnectedEvent, MessageReceivedEvent, ReadyEvent}
import sx.blah.discord.handle.obj.{IGuild, IMessage, IUser}
import sx.blah.discord.util.audio.AudioPlayer
import sx.blah.discord.util.audio.events.{TrackStartEvent, TrackSkipEvent}

import RegexExtractor._

object Bot extends App {

  case class Clargs(discordToken: String = null, channel: String = null)
  val clargs = new scopt.OptionParser[Clargs]("discord-jukebox") {
    opt[String]('t', "token").action((x, c) => c.copy(discordToken = x)).required.text("discord bot token")
    opt[String]('c', "channel").action((x, c) => c.copy(channel = x)).required.text("discord voice channel")
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
              msg.getContent.stripPrefix(me + " ") match {
                case "list" =>
                  if (ap.getPlaylistSize == 0) {
                    msg.reply("Nothing in the queue" )
                  } else {
                    val Seq(head, tail@_*) = ap.getPlaylist.asScala.zipWithIndex.map(e => e._2 + ": " + e._1.getMetadata.get("title")).grouped(20).toVector
                    msg.reply("Playlist:\n" + head.mkString("\n"))
                    for (s <- tail) {
                      Thread.sleep(200)
                      msg.reply(s.mkString("\n"))
                    }

                  }

                case "status" =>
                  msg.reply(
                    if (ap.getCurrentTrack == null) "Nothing is being played."
                    else s"Currently playing ${ap.getCurrentTrack.getMetadata.get("title")}. ${ap.getPlaylistSize - 1} remaining tracks."
                  )

                case "pause" =>
                  ap.setPaused(true)
                  msg.reply("_paused_")
                case "unpause" =>
                  ap.setPaused(false)
                  msg.reply("_unpaused_")

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


                case regex"add (.+)$url" => url match {
                    case regex"(https?://www.youtube.com.+)$url" =>
                      processingPlaylist.fold {
                      
                        msg.reply("_adding " + url + "_")
                        YoutubeProvider.fetchInformation(url, i => if (i > 0 && i % 20 == 0) msg.reply(s"_processed $i videos..._")).
                        map {
                          case (1, _, res) =>
                            ap.queue(LazyTrack(res.value.get.get.head))
                            msg.reply("_added " + res.value.get.get.head.name + " to the queue._")
                          case (num, cancel, playlistF) =>
                            processingPlaylist = Some((msg.getAuthor, cancel))
                            msg.reply(s"_processing $num videos in the playlist._")
                            playlistF.onComplete { res =>
                              processingPlaylist = None
                              res match {
                                case scala.util.Success(playlist) => msg.reply(s"_added $num songs to the queue._")
                                  playlist foreach (s => ap.queue(LazyTrack(s)))
                                case scala.util.Failure(YoutubeProvider.CancelledException) => msg.reply(s"_work cancelled_")
                                case scala.util.Failure(e) => msg.reply(s"Failed processing $url: $e.")
                              }
                            }(mainThreadExecutionContext)
                        }.failed.foreach(_.printStackTrace(Console.err))
                      
                      }{ case (owner, _) => msg.reply(s"Sorry, I'm still processing a playlist on ${owner.mention} 's behalf. Please try again later.")}


                    case other => msg.reply("for now I only support youtube links. Sorry.")
                  }

                case "cancel" if processingPlaylist.isEmpty => msg.reply("I'm not processing anything, so there is nothing to cancel.")
                case "cancel" =>
                  val (user, c) = processingPlaylist.get
                  c.put(())
                  msg.getChannel.sendMessage(s"Cancelling ${user.mention} 's work as per ${msg.getAuthor.mention} 's request.")
                
                case other => msg.reply(s"Sorry, I don't know the command: $other")
              }


              /*
               * react to audio player events
               */
            case e: TrackStartEvent =>
              println("track started")
              //when a track finishes playing, start caching the one after the current.
              e.getPlayer.getPlaylist.asScala.drop(1).headOption.foreach { track =>
                new Thread() {
                  override def run() = {
                    println("Precaching next track: " + track.getMetadata.get("title"))
                    track.isReady() //this causes the lazy stream to be fetch
                  }
                }.start()
              }
            

            case _ =>
          }
        } catch {
          case e: Exception => e.printStackTrace(Console.err)
        }
      }(mainThreadExecutionContext)
    })


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
