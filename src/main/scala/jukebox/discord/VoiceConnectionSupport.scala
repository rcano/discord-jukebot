package jukebox
package discord

import enumeratum.values.IntEnumEntry
import java.io.{IOException}
import java.net.{DatagramSocket, InetSocketAddress, DatagramPacket, SocketTimeoutException}
import org.asynchttpclient.{ws}

import org.json4s.JsonAST.JValue
import scala.collection.JavaConverters._
import scala.concurrent._, duration._, ExecutionContext.Implicits._
import Json4sUtils._, org.json4s.JsonDSL._
import AhcUtils._
import scala.util.control.NoStackTrace
import scala.util.control.NonFatal

private[discord] object VoiceConnectionSupport {
  val UdpKeepAlive = Array[Byte](0xC9.toByte, 0, 0, 0, 0, 0, 0, 0, 0)
  val OpusFrameSize = 960
  val MaxOpusPacketSize = OpusFrameSize * 2 // two channels
}
private[discord] trait VoiceConnectionSupport { self: DiscordClient =>
  import VoiceConnectionSupport._

  protected final class VoiceConnectionImpl(
      voiceStateUpdate: GatewayEvents.VoiceStateUpdate,
      voiceServerUpdate: GatewayEvents.VoiceServerUpdate,
      voiceConsumer: Array[Byte] => Unit,
      voiceProducer: () => Array[Byte]
  ) extends VoiceConnection with ws.WebSocketTextListener with ws.WebSocketCloseCodeReasonListener {
    @volatile private[this] var active = true
    @volatile private[this] var websocket: ws.WebSocket = _

    override def isActive = active
    override def close() = if (active && websocket != null) websocket.close()

    override def onOpen(ws): Unit = {
      websocket = ws
      listener.onConnectionOpened(this)
      send(renderJson(gatewayMessage(VoiceOp.Identify, ("server_id" -> voiceServerUpdate.guildId) ~
        ("user_id" -> voiceStateUpdate.voiceState.userId) ~
        ("session_id" -> voiceStateUpdate.sessionId) ~
        ("token" -> voiceServerUpdate.token))))
    }
    override def onClose(ws): Unit = if (isActive) {
      websocket = null
      active = false
      listener.onConnectionClosed(this)
    }
    override def onClose(ws, code, reason): Unit = if (isActive) {
      websocket.close()
      websocket = null
      active = false
      listener.onDisconnected(this, code, reason)
    }
    override def toString = s"VoiceConnection(voiceStateUpdate=$voiceStateUpdate, voiceServerUpdate=$voiceServerUpdate)"

    val stateMachine = new StateMachine[(DynJValueSelector, VoiceOp)] {
      def state(f: DynJValueSelector => PartialFunction[VoiceOp, Transition]): Transition = transition {
        case (payload, op) if f(payload).isDefinedAt(op) => f(payload)(op)
      }
      def initState = handshake
      def handshake = state(payload => {
        case VoiceOp.Ready =>
          val ssrc = payload.ssrc.extract[Int]
          val port = payload.port.extract[Int]
          val serverIp = payload.ip.extract[String]
          val modes = payload.modes.extract[Array[String]]
          val heartbeatInterval = payload.heartbeat_interval.extract[Int]
          val socket = new DatagramSocket()
          socket.connect(new InetSocketAddress(serverIp, port))
          val ourIp = discoverIp(ssrc, socket)
          socket.setSoTimeout(10) //maximum of 10ms for each operation

          send(renderJson(gatewayMessage(
            VoiceOp.SelectPayload,
            ("protocol" -> "udp") ~
              ("data" -> (
                ("address" -> ourIp.getHostString) ~
                ("port" -> ourIp.getPort) ~
                ("mode" -> "xsalsa20_poly1305")
              ))
          )))

          nextHeartbeat(heartbeatInterval)
          voiceConnected(ssrc, socket)
      })
      def voiceConnected(ssrc: Int, socket: DatagramSocket) = state(payload => {
        case VoiceOp.SessionDescription =>
          val secret = payload.secret_key.extract[Array[Byte]]

          /**
           * **************************************
           * audio sending and related variables. *
           * **************************************
           */
          var sendingAudio = false
          var seq: Char = 0
          var timestamp = 0
          def sendAudio() = {
            val audio = voiceProducer()
            if (audio.length > 0) {
              if (!sendingAudio) send(renderJson(gatewayMessage(VoiceOp.Speaking, ("delay" -> 0) ~ ("speaking" -> true))))
              val data = DiscordAudioUtils.encrypt(seq, seq * 960 /*this is opus frame size*/ , ssrc, audio, secret)

              if (seq + 1 > Char.MaxValue) seq = 0
              else seq = (seq + 1).toChar
              timestamp += 960

              try socket.send(new DatagramPacket(data, data.length))
              catch { case e: IOException => listener.onConnectionError(VoiceConnectionImpl.this, e) }
              sendingAudio = true
            } else if (sendingAudio) {
              send(renderJson(gatewayMessage(VoiceOp.Speaking, ("delay" -> 0) ~ ("speaking" -> false))))
              sendingAudio = false
            }
          }
          var keepAliveCall = 0l
          def keepAliveNatPort() = {
            if (keepAliveCall % (5000 / 20) == 0) { //every 5 seconds
              try socket.send(new DatagramPacket(UdpKeepAlive, UdpKeepAlive.length))
              catch { case e: IOException => listener.onConnectionError(VoiceConnectionImpl.this, e) }
            }
            keepAliveCall += 1
          }
          val receiveBuffer = new Array[Byte](MaxOpusPacketSize + 12) //header size
          def receiveAudio() = {
            try {
              val in = new DatagramPacket(receiveBuffer, receiveBuffer.length)
              socket.receive(in)
              voiceConsumer(DiscordAudioUtils.decrypt(receiveBuffer.take(in.getLength), secret))
            } catch {
              case to: SocketTimeoutException =>
              case e: IOException => listener.onConnectionError(VoiceConnectionImpl.this, e)
            }
          }

          val senderTask = new AccurateRecurrentTask({ cancelTask =>
            if (isActive) {
              sendAudio()
              keepAliveNatPort()
              receiveAudio()
            } else {
              cancelTask.put(())
              scala.util.Try(socket.close())
            }

          }, 20)
          senderTask.setName("DiscordAudio-" + voiceStateUpdate.voiceState.channelId)
          senderTask.start()

          done
      })
    }

    override def onError(ex): Unit = listener.onConnectionError(this, ex)
    override def onMessage(msg: String): Unit = {
      val payload = parseJson(msg).dyn

      try {
        VoiceOp.withValueOpt(payload.op.extract).fold {
          listener.onUnexpectedVoiceOp(this, payload.op.extract, payload)
        } { op =>
          listener.onVoiceOp(this, op, payload)
          stateMachine.applyIfDefined(payload.d -> op)
        }
      } catch { case NonFatal(e) => listener.onConnectionError(this, e) }
    }

    def send(msg: String) = {
      if (!active) throw new IllegalStateException(s"$this is closed!")
      listener.onMessageBeingSent(this, msg)
      websocket.sendMessage(msg)
    }
    def gatewayMessage(op: IntEnumEntry, data: JValue, eventType: Option[String] = None): JValue = {
      ("op" -> op.value) ~
        ("t" -> eventType.orNull) ~
        ("d" -> data)
    }

    def nextHeartbeat(interval: Int): Unit = {
      timer.newTimeout(timeout => if (isActive) {
        Future { // don't hog the timer thread
          send(renderJson(gatewayMessage(VoiceOp.Heartbeat, System.currentTimeMillis)))
          //after sending the heartbeat, change the current behaviour to detect the answer
          //if no answer is received in 5 seconds, reconnect.
          val now = System.currentTimeMillis
          val prevBehaviour = stateMachine.current

          val timeout = timer.newTimeout({ timeout =>
            if (!timeout.isCancelled) {
              listener.onConnectionError(this, new RuntimeException("Did not receive a HeartbeatAck in 5 seconds!") with NoStackTrace)
              close()
              onClose(websocket)
            }
          }, 5, SECONDS)

          lazy val detectHeartbeatAck: stateMachine.Transition = stateMachine.transition {
            case (_, VoiceOp.Heartbeat) =>
              timeout.cancel()
              prevBehaviour
            case other if prevBehaviour.isDefinedAt(other) =>
              prevBehaviour(other)
              detectHeartbeatAck
          }

          stateMachine.switchTo(detectHeartbeatAck)
          nextHeartbeat(interval)
        }
      }, interval, MILLISECONDS)

    }
    def discoverIp(ssrc: Int, socket: DatagramSocket): InetSocketAddress = {
      val data = java.nio.ByteBuffer.allocate(70).putInt(ssrc).array()
      socket.send(new DatagramPacket(data, data.length))
      val response = new DatagramPacket(new Array[Byte](70), 70)
      socket.receive(response)
      val receivedData = response.getData
      val ip = new String(receivedData.slice(4, 68)).trim()
      val port = (receivedData(69).toInt & 0xff) << 8 | (receivedData(68).toInt & 0xff)
      InetSocketAddress.createUnresolved(ip, port)
    }
  }
}
