name := "bitset-scala-offheap"

organization := "eu.unicredit"

scalaVersion := "2.11.7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
	"sh.den" %% "scala-offheap" % "0.1-SNAPSHOT",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test"
	)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)


