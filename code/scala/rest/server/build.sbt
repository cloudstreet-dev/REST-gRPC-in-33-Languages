name := "task-rest-server"
version := "1.0.0"
scalaVersion := "2.13.12"

val AkkaVersion = "2.8.5"
val AkkaHttpVersion = "10.5.3"
val CirceVersion = "0.14.6"
val SlickVersion = "3.4.1"

libraryDependencies ++= Seq(
  // Akka HTTP for REST API
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  
  // JSON handling with Circe
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",
  
  // Database with Slick
  "com.typesafe.slick" %% "slick" % SlickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % SlickVersion,
  "com.h2database" % "h2" % "2.2.224",
  
  // Logging
  "ch.qos.logback" % "logback-classic" % "1.4.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  
  // Testing
  "com.typesafe.akka" %% "akka-http-testkit" % AkkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.17" % Test
)

// Compiler options
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

// Assembly settings for creating fat JAR
assembly / assemblyMergeStrategy := {
  case "reference.conf" => MergeStrategy.concat
  case "application.conf" => MergeStrategy.concat
  case x if x.endsWith(".proto") => MergeStrategy.first
  case x if x.contains("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

assembly / mainClass := Some("com.example.taskapi.Main")
assembly / assemblyJarName := "task-rest-server.jar"