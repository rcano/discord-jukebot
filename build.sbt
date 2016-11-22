name := "discord-jukebox"

version := "0.1"

scalaVersion := "2.12.0"

fork := true

scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Ypartial-unification", "-Xlint", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.5.0",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.asynchttpclient" % "async-http-client" % "2.0.21",
  "com.beachape" %% "enumeratum" % "1.4.17",
  "com.sedmelluq" % "lavaplayer" % "1.1.22"
)
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("jukebox.Bot2")
javaOptions in Universal ++= Seq("-J-Xmx24m")

scalariformPreferences := scalariformPreferences.value.setPreference(scalariform.formatter.preferences.SpacesAroundMultiImports, false)

resolvers += "jcenter" at "http://jcenter.bintray.com"
resolvers += "jitpack.io" at "https://jitpack.io"
resolvers += "sedmelluq" at "http://maven.sedmelluq.com/"

dependsOn(`json4s-advanced-serializers`)

lazy val `json4s-advanced-serializers` = project
