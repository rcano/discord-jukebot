package jukebox.discord

import language.{higherKinds, existentials}

/**
 * Simple implementation of a state machine. Notice that state keeping is not thread safe (it wouldn't make much sense either). If you
 * need to use the state machine from different threads, override apply and synchronize it calling super.apply
 */
trait StateMachine[A] extends PartialFunction[A, Unit] {

  case class Transition(f: PartialFunction[A, Transition]) extends PartialFunction[A, Transition] {
    def apply(a: A) = f(a)
    def isDefinedAt(a: A) = f.isDefinedAt(a)
    def orElse(t: Transition) = Transition(f orElse t)
  }
  def transition(f: PartialFunction[A, Transition]) = Transition(f)

  private[this] var curr: Transition = initState
  def initState: Transition
  def apply(a: A) = curr = curr(a)
  def isDefinedAt(a: A) = curr.isDefinedAt(a)

  /**
   * @return the current transition
   */
  def current = curr
  /**
   * Imperative change of state. Discouraged but unavoidable sometimes.
   */
  def switchTo(t: Transition) = curr = t

  /**
   * Done state, which is defined for no payload
   */
  lazy val done = Transition(PartialFunction.empty)
}