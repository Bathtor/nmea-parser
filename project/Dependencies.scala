import sbt._

object Dependencies {
	val jvmDeps = Def.setting(Seq[ModuleID](
		"com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
		"org.typelevel"  %% "squants"  % "1.3.0",
		"com.lihaoyi" %% "fastparse" % "0.4.4"
	))
	val testDeps = Def.setting(Seq[ModuleID](
		"org.scalatest" %% "scalatest" % "3.0.3" % Test
	))
}
