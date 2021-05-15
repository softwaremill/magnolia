val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  scalaVersion := "2.12.13",
  organization := "com.softwaremill.magnolia",
  description := "Fast, easy and transparent typeclass derivation for Scala 2",
  version := "1.0.0-SNAPSHOT",
)

lazy val core = (project in file(".core"))
  .settings(
    name := "magnolia-core",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
    Compile / doc / scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))),
    Compile / doc / scalacOptions --= Seq("-Xlint:doc-detached"),
    libraryDependencies += "com.propensive" %% "mercator" % "0.2.1"
  )

lazy val examples = (project in file(".examples"))
  .dependsOn(core)
  .settings(
    scalacOptions ++= Seq("-Xexperimental", "-Xfuture"),
    name := "magnolia-examples",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "examples",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
  )
  .dependsOn(core)

lazy val test = (project in file(".test"))
  .dependsOn(examples)
  .settings(
    name := "magnolia-test",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "test",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    libraryDependencies += "com.propensive" %% "probably-cli" % "0.5.0",
    libraryDependencies += "com.propensive" %% "contextual-examples" % "1.5.0"
  )
