package json4sadvser

import AdvancedSerializer._
import org.json4s._
import JsonDSL._

object AdvancedSerializerTest extends App {

  case class Foo(a: String, b: Int, c: Option[String], d: Float, oldName: String,
      e: String, f: Int, g: Option[String], h: Float) {
    def z = 23
  }

  val fooSerializer = new AdvancedSerializer.ForType[Foo] {
    ignoreField(_.a)("Hello")
    renameField(_.b)("bb")
    mapFieldSimple(_.c)(_ getOrElse null)(Option.apply)
    mapRenameFieldSimple(_.oldName)("newName")(_.toInt)(_.toString)
    mapField(_.e)((elem, value) => value)((jv, parsed) => jv.toString)
    mapRenameField(_.h)("hhh")((e, v) => v)((jv, p) => p)
    //    debugGeneratedTree
  }.build()
  implicit val formats = DefaultFormats + fooSerializer

  val foo = Foo("Hello", 42, None, 42, "21", "Hello", 42, None, 42)
  val ser = fooSerializer.serialize(DefaultFormats)(foo)
  println(ser)
  val newFoo = Extraction.extract[Foo](ser)
  assert(newFoo == foo.copy(e = ser.toString), s"$foo != $newFoo")
}
