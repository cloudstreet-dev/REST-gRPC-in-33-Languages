name := "task-rest-client"
version := "1.0.0"
scalaVersion := "2.13.12"

val AkkaVersion = "2.8.5"
val AkkaHttpVersion = "10.5.3"
val CirceVersion = "0.14.6"

libraryDependencies ++= Seq(
  // Akka HTTP for HTTP client
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  
  // JSON handling with Circe
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",
  
  // Command line parsing
  "com.github.scopt" %% "scopt" % "4.1.0",
  
  // Logging
  "ch.qos.logback" % "logback-classic" % "1.4.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// Compiler options
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked"
)

// Assembly settings
assembly / assemblyMergeStrategy := {
  case "reference.conf" => MergeStrategy.concat
  case "application.conf" => MergeStrategy.concat
  case x if x.contains("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

assembly / mainClass := Some("com.example.taskapi.client.Main")
assembly / assemblyJarName := "task-rest-client.jar"