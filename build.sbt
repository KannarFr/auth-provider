name := """auth-provider"""
organization := "com.orangeade"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
    jdbc,
    guice,
    "org.playframework.anorm" %% "anorm" % "2.6.0",
    "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
    "org.postgresql" % "postgresql" % "42.2.1"
)