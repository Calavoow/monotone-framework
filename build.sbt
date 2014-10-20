name := "monotone-frameworks"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
			"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
			"org.scalatest" %% "scalatest" % "2.2.1" % "test",
			"com.chuusai" %% "shapeless" % "2.0.0"
)
