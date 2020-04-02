
val commonSettings = Seq(
  version            := "0.1",
  scalaVersion       := "2.13.1",
  organization       := "ch.redelmann",
)

lazy val eclair = project
  .in(file("."))
  .settings(
    commonSettings,
    name               := "eclair",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),

    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-source-url", "https://raw.githubusercontent.com/redelmann/eclair/master€{FILE_PATH}.scala",
      "-doc-root-content", baseDirectory.value + "/project/root-doc.txt"
    ),

    target in Compile in doc := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    ),
  )

lazy val example = project
  .in(file("example"))
  .settings(
    commonSettings,
    name := "eclair-examples",
    scalaSource in Compile := baseDirectory.value,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
  )
  .dependsOn(eclair)


