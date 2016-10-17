name := "discord-jukebox"

version := "0.1"

scalaVersion := "2.11.8"

fork := true

scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Xlint")

libraryDependencies ++= Seq(
  //"com.github.austinv11" % "Discord4j" % "2.6.1",
  //"com.github.austinv11" % "Discord4j" % "websocket-rewrite-SNAPSHOT",
  "org.json4s" %% "json4s-native" % "3.4.1",
  "com.github.scopt" %% "scopt" % "3.5.0"
)
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("jukebox.Bot")

resolvers += "jcenter" at "http://jcenter.bintray.com"
resolvers += "jitpack.io" at "https://jitpack.io"

dependsOn(Discord4J)

lazy val Discord4J = project.settings(
  name := "discord4j",
  organization := "sx.blah",
  version := "ws-rewrite",
  publishArtifact in (Compile, packageDoc) := false,
  crossPaths := false,
  libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "1.7.21",
    "org.apache.httpcomponents" % "httpcore" % "4.4.5",
    "org.apache.httpcomponents" % "httpclient" % "4.5.2",
    "org.apache.httpcomponents" % "httpmime" % "4.5.2",
    "commons-io" % "commons-io" % "2.5",
    "org.eclipse.jetty.websocket" % "websocket-client" % "9.3.11.v20160721",
    "net.jodah" % "typetools" % "0.4.7",
    "org.apache.commons" % "commons-lang3" % "3.4",
    "com.google.code.gson" % "gson" % "2.7",
    "junit" % "junit" % "4.12" % "test",
    "net.java.dev.jna" % "jna" % "4.2.2",
    "com.googlecode.soundlibs" % "mp3spi" % "1.9.5-2",
    "org.jcraft" % "jorbis" % "0.0.17",
    "jflac" % "jflac" % "1.3",
    "com.googlecode.soundlibs" % "tritonus-share" % "0.3.7-3",
    "org.tritonus" % "tritonus-dsp" % "0.3.6"
  )
)
