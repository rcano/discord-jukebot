package jukebox

import com.codahale.metrics.{ConsoleReporter, MetricRegistry}
import java.nio.ByteBuffer
import headache._, JsonUtils._
import scala.concurrent._, duration._

object AudioReceiveTest extends App {

  val noData = Array[Byte](0xF8.toByte, 0xFF.toByte, 0xFE.toByte)
  class DiscordClientHandler(logFunction: Any => Unit = println)(voiceProducer: () => Array[Byte] = () => noData, voiceConsumer: AudioRtpFrame => Unit = _ => ()) extends DiscordClient.DiscordListener {
    val println = logFunction
    //    override def onGatewayData(data) = println(s"Gateway ${pretty(render(data.jv))}")
    //    override def onGatewayOp(conn, op, data) = println(s"Received $op: ${pretty(render(data.jv))} from ${java.util.Objects hashCode conn}")
    //    override def onVoiceOp(conn, op, data) = println(Console.MAGENTA + s"Received $op: ${pretty(render(data.jv))}" + Console.RESET)
    override def onUnexpectedGatewayOp(conn: DiscordClient#GatewayConnection, op: Int, data: => DynJValueSelector) = println(s"Received unsupported op $op: ${renderJson(data.jv.get, true)}")
    override def onUnexpectedVoiceOp(conn: DiscordClient#VoiceConnection, op: Int, data: => DynJValueSelector) = println(Console.MAGENTA + s"Received unsupported op $op: ${renderJson(data.jv.get, true)}" + Console.RESET)
    override def onMessageBeingSent(conn: DiscordClient#Connection, msg: String) = {
      if (conn.isInstanceOf[DiscordClient#VoiceConnection])
        println(Console.MAGENTA + s"Sending $msg" + Console.RESET)
      else
        println(s"Sending $msg")
    }

    override def onReconnecting(conn: DiscordClient#Connection, reason: DiscordClient.ReconnectReason) = println(s"reconnecting $conn because of $reason")
    override def onConnectionOpened(conn: DiscordClient#Connection) = println(s"$conn connected")
    override def onConnectionClosed(conn: DiscordClient#Connection) = println(s"$conn closed")
    override def onDisconnected(conn: DiscordClient#Connection, code: Int, reason: String) = println(s"$conn disconnected, code $code due to $reason")
    override def onConnectionError(conn: DiscordClient#Connection, ex: Throwable) = ex.printStackTrace()

    def onGatewayEvent(conn: DiscordClient#GatewayConnection) = {
      case evt =>
        stateMachine.orElse[(DiscordClient#GatewayConnection, GatewayEvents.GatewayEvent), Unit] {
          case (_, evt) => println(s"${evt.getClass} happened")
        }.apply(conn -> evt)
    }

    val stateMachine = new StateMachine[(DiscordClient#GatewayConnection, GatewayEvents.GatewayEvent)] {
      def initState = ready
      def ready = transition {
        case (conn, GatewayEvents.GuildCreateEvent(GatewayEvents.GuildCreate(guild))) =>
          println(Console.CYAN + s"asking to join voice channel ${args(0)} in guild ${guild.name}" + Console.RESET)
          val musicChannel = guild.channels.find(_.name.get == args(0)).get
          conn.sendVoiceStateUpdate(guild.id, Some(musicChannel.id), false, false)
          setupVoiceChannel(guild)
      }

      def setupVoiceChannel(guild: GatewayEvents.Guild): Transition = transition {
        case (conn, GatewayEvents.VoiceStateUpdateEvent(u)) =>
          println(s"State received, waiting for voice server update")
          transition {
            case (conn, GatewayEvents.VoiceServerUpdateEvent(s)) =>
              println(s"connecting to voice channel")
              conn.client.connectToVoiceChannel(u, s, voiceConsumer, voiceProducer)
              detectFailures(guild)
          }
      }

      def detectFailures(guild: GatewayEvents.Guild) = transition {
        case (conn, GatewayEvents.Resumed(())) =>
          println(Console.CYAN + s"asking to rejoin voice channel in guild ${guild.name}" + Console.RESET)
          val musicChannel = guild.channels.find(_.name.get == args(0)).get
          conn.sendVoiceStateUpdate(guild.id, None, false, true)
          transition {
            case (conn, GatewayEvents.VoiceStateUpdateEvent(s)) =>
              conn.sendVoiceStateUpdate(guild.id, Some(musicChannel.id), false, false)
              setupVoiceChannel(guild)
          }
      } orElse ready
    }
  }

  val metrics = new MetricRegistry()
  ConsoleReporter.forRegistry(metrics).convertDurationsTo(MILLISECONDS).build().start(5, SECONDS)

  import javax.sound.sampled.{AudioSystem, AudioFormat}
  val audioFormat = new AudioFormat(48000, 16, 2, true, false)
  val sourceDataLine = AudioSystem.getSourceDataLine(audioFormat)
  println("Audio line " + sourceDataLine)
  sourceDataLine.addLineListener(evt => println("DataLine => " + evt))
  sourceDataLine.open(audioFormat)
  sourceDataLine.start()
  //  val opusOut = "out.raw".toFile.newFileChannel(File.OpenOptions.append)
  val opusDecoder = new com.sedmelluq.discord.lavaplayer.natives.opus.OpusDecoder(48000, 2)

  val jitterBuffer = new JitterBuffer(50, 20)
  val opusFrame = ByteBuffer.allocateDirect(1920) //way larger than it needs to
  val pcmOutput = ByteBuffer.allocateDirect(960 * 4)
  val pcmOutputShortBuffer = pcmOutput.asShortBuffer()
  val pcmOutputArray = new Array[Byte](960 * 4) // 960 is one sample, 2 channels mean 960*2, and each sample is 16 bits, so *2 again
  val receiver = new DiscordClient(args(1), new DiscordClientHandler(s => println(Console.BLUE + "receiver: " + s + Console.RESET))(
    voiceConsumer = f => jitterBuffer.synchronized(jitterBuffer.push(f))
  ))

  val receiverGw = Await.result(receiver.login(desiredEvents = Set(GatewayEvents.Intent.Guilds, GatewayEvents.Intent.GuildVoiceStates)), Duration.Inf)
//  receiverGw.head.sendStatusUpdate(None, Status.PlayingGame("receiving data"))

  var pendingBufferIterations = 0
  var dropped = 0
  val jitterBufferSize = metrics.histogram("jitterbuffer size")
  val compensation = metrics.histogram("compensation")
  new AccurateRecurrentTask(cancel => {
    jitterBufferSize.update(jitterBuffer.size)
    compensation.update(dropped)
    if (pendingBufferIterations > 0) {
      java.util.Arrays.fill(pcmOutputArray, 0.toByte) //write silence
      sourceDataLine.write(pcmOutputArray, 0, pcmOutputArray.length)
      pendingBufferIterations -= 1
    } else {

      jitterBuffer.synchronized {
        if (dropped > 0 && jitterBuffer.size > 3) {
          //        println("#######Compensating frame######")
          jitterBuffer.pop()
          dropped -= 1
        }
        jitterBuffer.pop()
      }.fold {
        dropped += 1
        if (dropped >= 3) {
          //          println("buffering")
          pendingBufferIterations = 8 //try to buffer up 160ms
          dropped = 0 //reset this
        }
        java.util.Arrays.fill(pcmOutputArray, 0.toByte) //write silence
        sourceDataLine.write(pcmOutputArray, 0, pcmOutputArray.length)
        ()
      } { frame =>
        try {
            opusFrame.clear()
            opusFrame.put(frame.audio).flip()
            pcmOutputShortBuffer.clear()
            val written = opusDecoder.decode(opusFrame, pcmOutputShortBuffer) * 4
            //    println("decoded " + pcmOutputShortBuffer)
            pcmOutput.position(0).limit(written)
            pcmOutput.get(pcmOutputArray)
            
            sourceDataLine.write(pcmOutputArray, 0, pcmOutputArray.length)
          } catch { case all: Exception => all.printStackTrace() }
      }
    }
  }, 20).start()
}
