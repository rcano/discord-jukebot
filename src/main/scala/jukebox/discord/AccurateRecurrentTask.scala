package jukebox.discord

import scala.concurrent.SyncVar

class AccurateRecurrentTask(task: SyncVar[Unit] => Unit, everyMillis: Int) extends Thread {
  private[this] final val sleepInterval = everyMillis * 1000000
  private[this] val cancelled = new SyncVar[Unit]()
  def cancel(): Unit = cancelled.put(())
  override def run(): Unit = {
    while (!cancelled.isSet) {
      val now = System.nanoTime()
      task(cancelled)
      val total = System.nanoTime() - now
      val nextTarget = now + sleepInterval - total
      sleepFor(sleepInterval - total - 100000) //sleep until 100 us before the target deadline
      while (nextTarget > System.nanoTime()) {} //consume cycles
    }
  }

  private def sleepFor(nanos: Long): Unit = {
    if (nanos > 0) {
      val ms = nanos / 1000000
      val nanosRem = nanos % 1000000
      Thread.sleep(ms, nanosRem.toInt)
    }
  }
}
