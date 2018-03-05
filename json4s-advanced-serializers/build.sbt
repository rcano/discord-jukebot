name := "json4s-advanced-serializers"

organization := "json4s-adv-ser"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

//coursierChecksums := Nil

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.json4s" %% "json4s-core" % "3.5.2"
)

