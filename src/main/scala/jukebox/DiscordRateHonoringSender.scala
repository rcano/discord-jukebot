package jukebox

import java.util.concurrent.Executors
import jukebox.discord._
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent._, duration._

/**
 * Helper utility class that follows a rate/s buffering system to send messages.
 */
class DiscordRateHonoringSender(client: DiscordClient) {
  private final val MaxSize = 1500
  private val scheduler = Executors.newSingleThreadScheduledExecutor()
  private type ChannelId = String
  private type UserId = String
  private val pendingMessages = new java.util.HashMap[(ChannelId, Option[User]), java.util.LinkedList[(String, Promise[Unit])]]().asScala.withDefaultValue(new java.util.LinkedList)
  scheduler.scheduleWithFixedDelay(new Runnable {
    def run = pendingMessages.synchronized {
      if (pendingMessages.nonEmpty) println("pending messages:\n  " + pendingMessages.map(e => (e._1._1 -> e._1._2.map(_.userName)) + " -> " + e._2.size).mkString("\n  "))
      pendingMessages.headOption foreach {
        case ((channelId, author), msgs) =>
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
              promise completeWith client.channels.createMessage(channelId, targetMsg).asInstanceOf[Future[Unit]]
              msgs.addFirst((author.map(u => mention(u) + ", ").getOrElse("") + rest, promise))
            } else {
              val (finalMsg, promises) = (0 to lastIndexToSend).foldLeft((new StringBuilder, Vector.empty[Promise[Unit]])) {
                case ((sb, promises), i) =>
                  val (msg, promise) = msgs.get(i)
                  sb.append(msg).append("\n") -> (promises :+ promise)
              }
              val f = client.channels.createMessage(channelId, finalMsg.toString).asInstanceOf[Future[Unit]]
              promises foreach (_ completeWith f)
              (0 to lastIndexToSend) foreach (_ => msgs.removeFirst)
            }
            if (msgs.isEmpty) pendingMessages.remove((channelId, author))
          } catch { case e: Exception => println("oh shit " + e.printStackTrace()) }
      }
    }
  }, 300, 300, MILLISECONDS)
  private def mention(user: User) = s"<@!${user.id}>"
  private def mention(channel: discord.Channel) = s"<#${channel.id}>"

  def send(channel: discord.Channel, msg: String): Future[Unit] = send(channel.id, msg)
  def send(channelId: ChannelId, msg: String): Future[Unit] = {
    val res = Promise[Unit]()
    pendingMessages.synchronized {
      val key = (channelId, None)
      val deque = pendingMessages(key)
      deque.addLast(msg -> res)
      pendingMessages(key) = deque
    }
    res.future
  }
  def reply(to: Message, msg: String): Future[Unit] = {
    val res = Promise[Unit]()
    pendingMessages.synchronized {
      val key = (to.channelId, Some(to.author))
      val deque = pendingMessages(key)
      deque.addLast((mention(to.author) + ", " + msg) -> res)
      pendingMessages(key) = deque
    }
    res.future
  }
}
