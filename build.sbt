
val common: sbt.Project.SettingsDefinition = Seq(
	organization := "eu.unicredit",
	version := "0.0.1-SNAPSHOT",
	scalaVersion := "2.11.7"
  )

lazy val core = (project in file("core")).
	settings(common : _*).
	settings(
	name := "bitset-scala-offheap",
	libraryDependencies ++= Seq(
		"sh.den" %% "scala-offheap" % "0.1-SNAPSHOT",
		"org.scalatest" %% "scalatest" % "2.2.4" % "test"
	),
	resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
	)

lazy val jmh = (project in file("jmh")).
	settings(common : _*).
	settings(
		name := "jmh"
	).enablePlugins(JmhPlugin).
	dependsOn(core)

lazy val root = (project in file(".")).
  				aggregate(core, jmh)