val commonSettings = Seq(
  scalaVersion := "2.12.8"
)

lazy val root = (project in file("."))
  .aggregate(exercises)
  .settings(commonSettings)
  .settings(
    name := "root"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    Compile / mainClass := Some("fpinscala.ch15.streamio.Main"),
    name := "exercises"
  )
