name := "discord-jukebox"
version := "0.1"

scalaVersion := "2.13.6"

fork := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-opt:l:inline", "-opt-inline-from:scala.**", "-opt-warnings:_")
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:_", "-opt:_", "-Xlint")

dependsOn(headache)
dependsOn(RootProject(uri("git://github.com/JamesGallicchio/regextractor")))
lazy val headache = RootProject(file("../headache"))

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.sedmelluq" % "lavaplayer" % "1.3.77",
  "io.dropwizard.metrics" % "metrics-core" % "4.2.3",
  //"io.github.soc" %% "regextractor" % "0.2",
  "org.jsoup" % "jsoup" % "1.10.3",
  "org.slf4j" % "slf4j-simple" % "1.7.30" % "runtime",
)
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("jukebox.Bot")
javaOptions in Universal ++= Seq("-J-Xmx24m")
mappings in (Compile, packageDoc) := Seq()

//coursierChecksums := Nil

resolvers += "sedmelluq" at "https://m2.dv8tion.net/releases"
resolvers += "jitpack.io" at "https://jitpack.io"

