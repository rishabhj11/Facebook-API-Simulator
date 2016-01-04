name := "FB_REST_API"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

//resolvers ++= Seq("spray repo" at "http://repo.spray.io/")
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  val akkaV = "2.3.13"
  val sprayV = "1.3.3"
  val sprayJsonV = "1.3.2"
  val specs2V = "3.6.5"
  val scalazV = "7.1.5"

  Seq(
    "io.spray"            %%  "spray-can"     % sprayV withSources() withJavadoc(),
    "io.spray"            %%  "spray-routing" % sprayV withSources() withJavadoc(),
    "io.spray"            %%  "spray-json"    % sprayJsonV withSources() withJavadoc(),
    "io.spray"            %%  "spray-testkit" % sprayV  % "test",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
    "com.typesafe.akka" %% "akka-http-experimental" % "0.7",
    "org.specs2"          %%  "specs2-core"   % specs2V % "test",
    "org.scalaz"          %%  "scalaz-core"   % scalazV,
    "org.json4s" %% "json4s-native" % "3.2.10",
    "com.typesafe.play"   %%  "play-json"     % "2.3.0",
  "io.spray" %% "spray-client" % "1.3.2"
  )
}