name := "discord-jukebox"

version := "0.1"

scalaVersion := "2.11.8"

fork := true

scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Xlint")

libraryDependencies ++= Seq(
  "com.github.austinv11" % "Discord4j" % "2.6.1",
  "org.json4s" %% "json4s-native" % "3.4.1",
  "com.github.scopt" %% "scopt" % "3.5.0"
)
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("jukebox.Bot")

resolvers += "jcenter" at "http://jcenter.bintray.com"
resolvers += "jitpack.io" at "https://jitpack.io"
