val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "buckshot-roulette-assistant",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
  )
