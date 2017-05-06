name := "discord-jukebox"

version := "0.1"

scalaVersion := "2.12.2"

fork := true

scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Ypartial-unification", "-Xlint", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.0.0",
  "org.json4s" %% "json4s-native" % "3.5.2",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.asynchttpclient" % "async-http-client" % "2.0.31",
  "com.beachape" %% "enumeratum" % "1.5.10",
  "com.sedmelluq" % "lavaplayer" % "1.2.34",
  "io.dropwizard.metrics" % "metrics-core" % "3.2.2",
  "io.github.soc" %% "regextractor" % "0.2",
  "org.jsoup" % "jsoup" % "1.10.2"
)
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("jukebox.Bot")
javaOptions in Universal ++= Seq("-J-Xmx24m")

scalariformPreferences := scalariformPreferences.value.setPreference(scalariform.formatter.preferences.SpacesAroundMultiImports, false)

resolvers += "jcenter" at "http://jcenter.bintray.com"
resolvers += "jitpack.io" at "https://jitpack.io"
resolvers += "sedmelluq" at "http://maven.sedmelluq.com/"

dependsOn(`json4s-advanced-serializers`)

lazy val `json4s-advanced-serializers` = project

assemblyShadeRules in assembly := Seq(ShadeRule.keep("jukebox.**").inAll)
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.filterDistinctLines
  case other => 
   val prev = (assemblyMergeStrategy in assembly).value
   prev(other)
}
