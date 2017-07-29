package jukebox

import language.dynamics

import org.json4s._
import org.json4s.native.JsonMethods
import prickle._
import scala.annotation.unchecked.uncheckedVariance

object Json4sUtils {

  type Unpickler[+T] = prickle.Unpickler[T @uncheckedVariance]

  implicit class jValue2Dyn(val jv: JValue) extends AnyVal {
    def dyn = new DynJValueSelector(jv)
  }
  class DynJValueSelector(val jv: JValue) extends AnyVal with Dynamic {
    def selectDynamic(field: String) = new DynJValueSelector(jv \ field)
    def extract[T](implicit unpickler: Unpickler[T], conf: PConfig[JValue]): T = Unpickle[T].from(jv)(conf).get
    override def toString = jv.toString
  }

  def parseJson(s: String) = JsonMethods.parse(s)
}