name := "goose"

version := "0.1-SNAPSHOT"

organization := "net.rafaelferreira"

scalaVersion := "2.10.0-M7"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.2" intransitive()

libraryDependencies += "org.specs2" % "specs2_2.10.0-M7" % "1.12.1.1"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5-rc1"

libraryDependencies +=  "com.github.scala-incubator.io" % "scala-io-file_2.10" % "0.4.1" intransitive()

libraryDependencies += "com.github.scala-incubator.io" % "scala-io-core_2.10" % "0.4.1" intransitive()

sbtVersion := "0.13.0"
