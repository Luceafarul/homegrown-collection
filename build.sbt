version := "0.0.1-SNAPSHOT"

organization := "example-devinsideyou"

scalaVersion := "2.12.4"

triggeredMessage := Watched.clearWhenTriggered

initialCommands in console := "import homegrown.collections"

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)