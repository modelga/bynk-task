name := "bynk-task"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
mainClass in assembly := Some("se.bynk.task.Application")
assemblyJarName in assembly := "search.jar"
