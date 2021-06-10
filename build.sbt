import com.softwaremill.UpdateVersionInDocs

val scala2_12 = "2.12.13"
val scala2_13 = "2.13.6"
val scala2 = List(scala2_12, scala2_13)

excludeLintKeys in Global ++= Set(ideSkipProject)

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.magnolia",
  description := "Fast, easy and transparent typeclass derivation for Scala 2",
  updateDocs := UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("readme.md"))),
  ideSkipProject := (scalaVersion.value == scala2_12), // only import 2.13 projects
)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(name := "magnolia", publishArtifact := false, scalaVersion := scala2_13)
    .aggregate((core.projectRefs ++ examples.projectRefs ++ test.projectRefs): _*)

lazy val core = (projectMatrix in file("core"))
  .settings(commonSettings)
  .settings(
    name := "magnolia-core",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
    Compile / doc / scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))),
    Compile / doc / scalacOptions --= Seq("-Xlint:doc-detached"),
    libraryDependencies += "com.propensive" %% "mercator" % "0.2.1"
  )
  .jvmPlatform(scalaVersions = scala2)

lazy val examples = (projectMatrix in file("examples"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    scalacOptions ++= Seq("-Xexperimental", "-Xfuture"),
    name := "magnolia-examples",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
  )
  .dependsOn(core)
  .jvmPlatform(scalaVersions = scala2)

lazy val test = (projectMatrix in file("test"))
  .dependsOn(examples)
  .settings(commonSettings)
  .settings(
    name := "magnolia-test",
    Test / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Test / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.26",
      "com.propensive" %% "contextual-examples" % "1.5.0"
    ).map(_ % Test)
  )
  .jvmPlatform(scalaVersions = List(scala2_12))
