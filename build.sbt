name := """HealthcareRestApi"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"

libraryDependencies ++= Seq(
  "net.debasishg" %% "redisclient" % "3.2",
  "com.eclipsesource" %% "play-json-schema-validator" % "0.8.5",
  jdbc,
  filters,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)



fork in run := true