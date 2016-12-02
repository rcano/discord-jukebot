package jukebox
package discord

import enumeratum.values.IntEnumEntry
import java.io.{ByteArrayInputStream, BufferedReader, InputStreamReader}
import java.time.Instant
import java.util.Arrays
import java.util.zip.InflaterInputStream
import org.asynchttpclient.ws

import org.json4s.JInt
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import scala.collection.JavaConverters._
import scala.concurrent._, duration._, ExecutionContext.Implicits._
import Json4sUtils._
import AhcUtils._
import scala.util.control.NoStackTrace
import scala.util.control.NonFatal

private[discord] trait GatewayConnectionSupport { self: DiscordClient =>
  import DiscordClient._

  protected def startShard(gw: String, shard: Int, totalShards: Int, lastSession: Option[LastSessionState] = None): Future[GatewayConnection] = try {
    val res = new GatewayConnectionImpl(gw, shard, totalShards, lastSession)
    val websocketFuture = ahc.prepareGet(gw).execute(new ws.WebSocketUpgradeHandler(Arrays.asList(res)))
    val ready = Promise[GatewayConnection]()
    websocketFuture.toCompletableFuture.handle[Unit] {
      case (_, null) =>
        ready.success(res); ()
      case (_, ex) => ready.failure(ex); ()
    }
    ready.future
  } catch {
    case NonFatal(ex) => Future.failed(ex)
  }

  protected case class SessionData(id: String, gatewayProtocol: Int)
  protected case class LastSessionState(data: SessionData, seq: Long, connectionAttempt: Int)
  protected final class GatewayConnectionImpl(val gateway: String, val shardNumber: Int, val totalShards: Int,
      lastSession: Option[LastSessionState]) extends GatewayConnection with ws.WebSocketTextListener with ws.WebSocketCloseCodeReasonListener with ws.WebSocketByteListener {
    @volatile private[this] var active = true
    @volatile private[this] var websocket: ws.WebSocket = _
    @volatile private[this] var seq = 0l
    @volatile private[this] var session: Option[SessionData] = None

    override def isActive = active
    override def close() = if (active && websocket != null) websocket.close()

    override def onOpen(ws): Unit = {
      websocket = ws
      listener.onConnectionOpened(this)
    }
    override def onClose(ws): Unit = if (isActive) {
      websocket = null
      active = false
      listener.onConnectionClosed(this)
    }
    override def onClose(ws, code, reason): Unit = if (isActive) { //after a break down, netty will eventually realize that the socket broke, and even though we already called websocket.close(), it will eventually invoke this method.
      listener.onDisconnected(this, code, reason)
      reconnect(DisconnectedByServer)
    }
    override def toString = s"GatewayConnection(gw=$gateway, shardNumber=$shardNumber, totalShards=$totalShards, seq=$seq, session=${session.map(_.id)})"

    /**
     * **************************************************
     * Definition of possible states for the connection *
     * **************************************************
     */

    val stateMachine = new StateMachine[(DynJValueSelector, GatewayOp)] {
      private[this] val identityMsg = renderJson {
        gatewayMessage(
          GatewayOp.Identify,
          ("token" -> s"Bot $token") ~
            ("properties" -> (
              ("$os" -> System.getProperty("os.name")) ~
              ("$browser" -> "strife v1.0") ~
              ("$device" -> "strife") ~
              ("$referring_domain" -> "") ~
              ("$referrer" -> "")
            )) ~
              ("compress" -> true) ~
              ("large_threshold" -> 50) ~
              ("shard" -> Seq(shardNumber, totalShards))
        )
      }

      def state(f: DynJValueSelector => PartialFunction[GatewayOp, Transition]): Transition = transition {
        case (payload, op) if f(payload).isDefinedAt(op) => f(payload)(op)
      }
      def initState = hello
      def hello = state(payload => {
        case GatewayOp.Hello =>
          nextHeartbeat(payload.d.heartbeat_interval.extract)
          lastSession match {
            case None =>
              send(identityMsg)
              handshake
            case Some(lastSession) =>
              send(renderJson(gatewayMessage(GatewayOp.Resume, ("token" -> s"Bot $token") ~
                ("session_id" -> lastSession.data.id) ~
                ("seq" -> lastSession.seq))))
              resume
          }
      })

      def handshake = state(payload => {
        case GatewayOp.Dispatch if payload.t.extract[String] == "READY" =>
          session = Some(SessionData(payload.d.session_id.extract, payload.d.v.extract))
          dispatcher((payload, GatewayOp.Dispatch))
          dispatcher

        case GatewayOp.InvalidSession =>
          listener.onConnectionError(GatewayConnectionImpl.this, new IllegalStateException("Received an invalid session after sending identification.") with NoStackTrace)
          close()
          done
      })

      def resume: Transition = state(payload => {
        case GatewayOp.InvalidSession =>
          send(identityMsg)
          handshake

        case GatewayOp.Dispatch if payload.t.extract[String] == "RESUMED" =>
          session = lastSession.map(_.data) //only session needs to be assgined, seq is obtained from the resumed message
          dispatcher((payload, GatewayOp.Dispatch))
          dispatcher

        case GatewayOp.Dispatch => //replayed messages
          dispatcher((payload, GatewayOp.Dispatch))
          resume //continue resuming
      })

      def dispatcher: Transition = state(payload => {
        case GatewayOp.Dispatch =>
          try {
            val a = this
            GatewayEventParser.parse(payload) foreach listener.onGatewayEvent(GatewayConnectionImpl.this)
          } catch { case NonFatal(e) => listener.onConnectionError(GatewayConnectionImpl.this, e) }
          dispatcher
        case GatewayOp.Reconnect =>
          reconnect(RequestedByServer)
          done
      })
    }

    override def onError(ex): Unit = listener.onConnectionError(this, ex)
    override def onMessage(msg: String): Unit = {
      val payload = parseJson(msg).dyn

      try {

        payload.s.extract[Option[Long]] foreach (seq = _)
        GatewayOp.withValueOpt(payload.op.extract).fold {
          listener.onUnexpectedGatewayOp(this, payload.op.extract, payload)
        } { op =>

          listener.onGatewayOp(this, op, payload)
          stateMachine.orElse[(DynJValueSelector, GatewayOp), Unit] {
            case (_, GatewayOp.Heartbeat) => send(renderJson(("op" -> GatewayOp.Heartbeat.value) ~ ("d" -> seq)))
            case (_, GatewayOp.Dispatch) =>
            case _ =>
          }.apply(payload -> op)
        }

      } catch { case NonFatal(e) => listener.onConnectionError(this, e) }
    }
    override def onMessage(bytes: Array[Byte]): Unit = {
      val reader = new BufferedReader(new InputStreamReader(new InflaterInputStream(new ByteArrayInputStream(bytes))))
      val msg = reader.lines.collect(java.util.stream.Collectors.joining())
      onMessage(msg)
    }

    def send(msg: String) = {
      if (!active) throw new IllegalStateException(s"Shard $shardNumber is closed!")
      listener.onMessageBeingSent(this, msg)
      websocket.sendMessage(msg)
    }

    def gatewayMessage(op: IntEnumEntry, data: JValue, eventType: Option[String] = None): JValue = {
      ("op" -> op.value) ~
        ("t" -> eventType.orNull) ~
        ("s" -> seq) ~
        ("d" -> data)
    }

    def sendStatusUpdate(idleSince: Option[Instant], status: Status): Unit = {
      send(renderJson(
        gatewayMessage(GatewayOp.StatusUpdate, ("idle_since" -> idleSince.map(e => JInt(e.toEpochMilli)).orNull) ~ ("game" -> (status match {
          case Status.PlayingGame(game) => ("name" -> game): JValue
          case Status.Streaming(name, url) => ("name" -> name) ~ ("url" -> url)
          case Status.Empty => null
        })), Some("STATUS_UPDATE"))
      ))
    }
    def sendVoiceStateUpdate(guildId: String, channelId: Option[String], selfMute: Boolean, selfDeaf: Boolean): Unit = {
      send(renderJson(
        gatewayMessage(GatewayOp.VoiceStateUpdate, ("guild_id" -> guildId) ~ ("channel_id" -> channelId.orNull) ~
          ("self_mute" -> selfMute) ~ ("self_deaf" -> selfDeaf), Some("VOICE_STATE_UPDATE"))
      ))
    }

    def nextHeartbeat(interval: Int): Unit = {
      timer.newTimeout(timeout => if (isActive) { // don't hog the timer thread
        Future {
          send(renderJson(("op" -> GatewayOp.Heartbeat.value) ~ ("d" -> seq)))
          //after sending the heartbeat, change the current behaviour to detect the answer
          //if no answer is received in 5 seconds, reconnect.
          val now = System.currentTimeMillis
          val prevBehaviour = stateMachine.current

          val timeout = timer.newTimeout({ timeout =>
            if (!timeout.isCancelled && isActive) {
              listener.onConnectionError(this, new RuntimeException("Did not receive a HeartbeatAck in 5 seconds!") with NoStackTrace)
              reconnect(HeartbeatMissed)
            }
          }, 5, SECONDS)

          lazy val detectHeartbeatAck: stateMachine.Transition = stateMachine.transition {
            case (_, GatewayOp.HeartbeatAck) =>
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

    def reconnect(reason: ReconnectReason): Unit = {
      websocket.close()
      onClose(websocket)
      listener.onReconnecting(this, reason)
      val reconnectInstance = if (session.isDefined) 0 else lastSession.map(_.connectionAttempt + 1).getOrElse(0)
      val newLastSession = session.map(s => LastSessionState(s, seq, reconnectInstance))
      def reconnectAttempt(duration: FiniteDuration): Unit = {
        timer.newTimeout(
          _ =>
          startShard(gateway, shardNumber, totalShards, newLastSession).failed.foreach(_ => reconnectAttempt(5.seconds)),
          duration.length, duration.unit
        )
      }
      if (reconnectInstance > 0) reconnectAttempt(5.seconds)
      else reconnectAttempt(0.seconds)
    }
  }
}
