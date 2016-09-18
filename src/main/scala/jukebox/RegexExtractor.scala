package jukebox

object RegexExtractor {

  implicit class RegexStringContext(val sc: StringContext) {
    object regex {
      def apply(args: String*) = sc.raw(args)
      def unapplySeq(s: String) = sc.parts.mkString.r.unapplySeq(s)
    }
  }
}
