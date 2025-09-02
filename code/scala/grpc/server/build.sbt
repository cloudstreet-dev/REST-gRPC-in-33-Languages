name := "task-grpc-server"
version := "1.0.0"
scalaVersion := "2.13.12"

// ScalaPB and gRPC versions
val grpcVersion = "1.58.0"
val scalapbVersion = "0.11.14"

// Enable ScalaPB compilation
Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)

// Proto file location
Compile / PB.protoSources := Seq(
  baseDirectory.value / ".." / ".." / ".." / "shared" / "protos"
)

libraryDependencies ++= Seq(
  // ScalaPB and gRPC
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
  "io.grpc" % "grpc-netty-shaded" % grpcVersion,
  "io.grpc" % "grpc-services" % grpcVersion,
  
  // Logging
  "ch.qos.logback" % "logback-classic" % "1.4.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// Assembly settings
assembly / assemblyMergeStrategy := {
  case "META-INF/io.netty.versions.properties" => MergeStrategy.first
  case "META-INF/native/libnetty_transport_native_epoll_x86_64.so" => MergeStrategy.first
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case x if x.contains("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

assembly / mainClass := Some("com.example.taskapi.grpc.server.Main")
assembly / assemblyJarName := "task-grpc-server.jar"