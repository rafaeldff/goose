name := "goose"

version := "0.1-SNAPSHOT"

organization := "net.rafaelferreira"

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

scalaVersion := "2.10.0-RC1"

libraryDependencies +=  "org.scala-lang" % "scala-reflect" % "2.10.0-RC1"

libraryDependencies += "com.jsuereth" % "scala-arm_2.10.0-RC1" % "1.2" intransitive()

libraryDependencies += "org.specs2" %% "specs2" % "1.12.2"                       

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5-rc1"

libraryDependencies +=  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1" intransitive()

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1" intransitive()

sbtVersion := "0.13.0"
