val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "buckshot-roulette-assistant",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    assembly / mainClass := Some("com.zkerriga.buckshot.Main"),
    assembly / assemblyJarName := s"${name.value}.jar",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.6.3",
      "org.typelevel" %% "cats-effect-cps" % "0.5.0",
      "org.typelevel" %% "spire" % "0.18.0",
      "com.googlecode.lanterna" % "lanterna" % "3.1.3",
      "io.7mind.izumi" %% "logstage-core" % "1.2.22",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
  )
