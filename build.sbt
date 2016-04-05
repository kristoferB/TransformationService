enablePlugins(JavaAppPackaging, SbtOsgi)

organization := "sekvensa"

name := "AreusSunrise"

version := "0.1"

scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV       = "2.4.1"
  val akkaStreamV = "2.0.1"
  val scalaTestV  = "2.2.5"
  Seq(
    "com.typesafe"       % "config"                               % "1.3.0",
    "com.typesafe.akka" %% "akka-actor"                           % akkaV,
//    "com.typesafe.akka" %% "akka-stream-experimental"             % akkaStreamV,
//    "com.typesafe.akka" %% "akka-http-core-experimental"          % akkaStreamV,
//    "com.typesafe.akka" %% "akka-http-experimental"               % akkaStreamV,
//    "com.typesafe.akka" %% "akka-http-spray-json-experimental"    % akkaStreamV,
//    "com.typesafe.akka" %% "akka-http-testkit-experimental"       % akkaStreamV,
    "com.codemettle.reactivemq" %% "reactivemq"                   % "0.5.4",
    "org.apache.activemq" % "activemq-client"                     % "5.13.1",
    "org.json4s" %% "json4s-native"                               % "3.3.0",
    "org.json4s" %% "json4s-ext"                                  % "3.3.0",
    "com.github.nscala-time" %% "nscala-time"                     % "1.8.0",
    "org.scalatest"     %% "scalatest"                            % scalaTestV % "test"
  )
}


osgiSettings

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra :=
  <url>http://</url>
    <licenses>
      <license>
        <name>Apache-2.0</name>
        <url>http://opensource.org/licenses/Apache-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/kristoferB</url>
      <connection>scm:git:git@github.com:kristoferB/REPO.git</connection>
    </scm>
    <developers>
      <developer>
        <name>Kristofer Bengtsson</name>
      </developer>
    </developers>
    