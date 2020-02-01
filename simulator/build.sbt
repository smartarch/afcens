name := "afcens-demo-simulator"

version := "1.0"

scalaVersion := "2.13.1"

mainClass in (Compile, run) := Some("afcens.TraceRecord")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "org.choco-solver" % "choco-solver" % "4.10.0",
  "com.typesafe.akka" %% "akka-actor" % "2.5.23",
  "com.typesafe.akka" %% "akka-stream" % "2.5.23",
  "com.typesafe.akka" %% "akka-http" % "10.1.8",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.8",
  "org.junit.jupiter" % "junit-jupiter-api" % "5.5.1",
  "org.junit.jupiter" % "junit-jupiter-engine" % "5.5.1",
  "org.hamcrest" % "hamcrest-all" % "1.3"
)
