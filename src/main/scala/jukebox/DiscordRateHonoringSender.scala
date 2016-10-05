package jukebox

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent._, duration._
import scala.util.control.NonFatal
import sx.blah.discord.handle.obj.{IChannel, IMessage, IUser}
import sx.blah.discord.util.RateLimitException

/**
 * Helper utility class that follows a rate/s buffering system to send messages.
 */
class DiscordRateHonoringSender(val exceptionLogger: Throwable => Unit = null) {
  private final val MaxSize = 2000
  private val scheduler = Executors.newSingleThreadScheduledExecutor()
  private val pendingMessages = new java.util.HashMap[(IChannel, Option[IUser]), java.util.LinkedList[(String, Promise[Unit])]]().asScala.withDefaultValue(new java.util.LinkedList)
  scheduler.scheduleWithFixedDelay(new Runnable {
      def run = pendingMessages.synchronized {
        if (pendingMessages.nonEmpty) println("pending messages:\n  " + pendingMessages.map(e => (e._1._1.getName -> e._1._2.map(_.getName)) + " -> " + e._2.size).mkString("\n  "))
        pendingMessages.headOption foreach {
          case ((channel, author), msgs) =>
            @tailrec def clampMessages(acc: Int = 0, i: Int = 0): (Int, Int) = {
              val newAcc = msgs.get(i)._1.length + acc
              if (newAcc > MaxSize) (acc, i) //reached cutoff
              else if ((i + 1) == msgs.size) (newAcc, i)
              else clampMessages(newAcc, i + 1)
            }
            try {

              val (targetSize, lastIndexToSend) = clampMessages()
              println("sending " + (targetSize -> lastIndexToSend))
              if (targetSize == 0) { //first message is too large, have to split it up
                val (msg, promise) = msgs.remove(0)
                val (targetMsg, rest) = msg.splitAt(MaxSize)
                try {
                  channel.sendMessage(targetMsg)
                  msgs.addFirst((author.map(_.mention + ", ").getOrElse("") + rest, promise))
                } catch {
                  case e: RateLimitException => //do nothing, will reattempt the next cycle
                  case NonFatal(e) =>
                    if (exceptionLogger != null) exceptionLogger(e)
                    promise.failure(e)
                }
              } else {
                val (finalMsg, promises) = (0 to lastIndexToSend).foldLeft((new StringBuilder, Vector.empty[Promise[Unit]])) {
                  case ((sb, promises), i) =>
                    val (msg, promise) = msgs.get(i)
                    sb.append(msg).append("\n") -> (promises :+ promise)
                }
                try {
                  channel.sendMessage(finalMsg.toString)
                  promises foreach (_.success(()))
                  (0 to lastIndexToSend) foreach (_ => msgs.removeFirst)
                } catch {
                  case e: RateLimitException => //do nothing, will reattempt the next cycle
                  case NonFatal(e) =>
                    if (exceptionLogger != null) exceptionLogger(e)
                    promises foreach (_.failure(e))
                    (0 to lastIndexToSend) foreach (_ => msgs.removeFirst)
                }
              }
              if (msgs.isEmpty) pendingMessages.remove((channel, author))
            } catch {
              case NonFatal(e) => if (exceptionLogger != null) exceptionLogger(e)
            }
        }
      }
    }, 300, 300, MILLISECONDS)

  def send(channel: IChannel, msg: String): Future[Unit] = {
    val res = Promise[Unit]()
    pendingMessages.synchronized {
      val key = (channel, None)
      val deque = pendingMessages(key)
      deque.addLast(msg -> res)
      pendingMessages(key) = deque
    }
    res.future
  }
  def reply(to: IMessage, msg: String): Future[Unit] = {
    val res = Promise[Unit]()
    pendingMessages.synchronized {
      val key = (to.getChannel, Some(to.getAuthor))
      val deque = pendingMessages(key)
      deque.addLast((to.getAuthor.mention + ", " + msg) -> res)
      pendingMessages(key) = deque
    }
    res.future
  }
}
