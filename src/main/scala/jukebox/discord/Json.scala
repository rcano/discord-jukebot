package jukebox
package discord

import java.time.Instant
import json4sadvser.AdvancedSerializer
import org.json4s._, native.JsonMethods._, JsonDSL._
import scala.annotation.unchecked.uncheckedVariance
import Json4sUtils._

object Json {

  val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSXXXXX")
  val instantSerializer = new CustomSerializer[Instant](
    implicit formats => (
      { case JString(s) => Instant from dateTimeFormatter.parse(s) },
      { case i: Instant => JString(i.toString) }
    )
  )

  val channelSerializer = new CustomSerializer[Channel](
    implicit formats => (
      {
        case jv if jv.dyn.tpe.extract[String] == "text" => jv.extract[TextChannel]
        case jv => jv.extract[VoiceChannel]
      },
      {
        case c: TextChannel => Extraction.decompose(c) merge (("type" -> "text"): JValue)
        case c: VoiceChannel => Extraction.decompose(c) merge (("type" -> "voice"): JValue)
      }
    )
  )

  val userSerializer = new AdvancedSerializer.ForType[User] {
    renameField(_.userName)("username")
    mapField(_.bot)((user, v) => Option(v))((jv, v) => v.getOrElse(false))
  }.build()
  //  val userRenamedFields = FieldSerializer[User](
  //    FieldSerializer.renameTo("userName", "username"),
  //    FieldSerializer.renameFrom("username", "userName")
  //  )

  implicit val formats = DefaultFormats + instantSerializer + channelSerializer + userSerializer

  type Manifest[+T] = scala.reflect.Manifest[T @uncheckedVariance]
  def extract[T: Manifest](json: JValue): T = json.camelizeKeys.transformField {
    case ("type", x) => ("tpe", x)
  }.extract[T]
  def parse[T: Manifest](json: String): T =
    native.JsonMethods.parse(json).camelizeKeys.transformField {
      case ("type", x) => ("tpe", x)
    }.extract[T]

  def decompose(value: Any) = Extraction.decompose(value).snakizeKeys.transformField {
    case ("tpe", x) => ("type", x)
  }
}
