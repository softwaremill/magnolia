import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{updateDocs, ossPublishSettings}
import com.softwaremill.UpdateVersionInDocs

val scala2_12 = "2.12.21"
val scala2_13 = "2.13.18"
val scala2 = List(scala2_12, scala2_13)

Global / excludeLintKeys ++= Set(ideSkipProject)
ThisBuild / dynverTagPrefix := "scala2-v" // a custom prefix is needed to differentiate tags between scala2 & scala3 versions

commonSmlBuildSettings
ossPublishSettings

organization := "com.softwaremill.magnolia1_2"
description := "Fast, easy and transparent typeclass derivation for Scala 2"
ideSkipProject := (scalaVersion.value == scala2_12) // only import 2.13 projects

lazy val root =
  project
    .in(file("."))
    .settings(
      name := "magnolia-root",
      publishArtifact := false,
      scalaVersion := scala2_13,
      updateDocs := Def.uncached(UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("readme.md"))))
    )
    .aggregate((core.projectRefs ++ examples.projectRefs ++ test.projectRefs)*)

lazy val core = (projectMatrix in file("core"))
  .settings(
    name := "magnolia",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
    Compile / doc / scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))),
    Compile / doc / scalacOptions --= Seq("-Xlint:doc-detached"),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    mimaPreviousArtifacts := {
      val current = version.value
      val isRcOrMilestone = current.contains("M") || current.contains("RC")
      if (!isRcOrMilestone) {
        val previous = previousStableVersion.value
        println(s"[info] Not a M or RC version, using previous version for MiMa check: $previous")
        previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet
      } else {
        println(s"[info] $current is an M or RC version, no previous version to check with MiMa")
        Set.empty
      }
    },
    versionScheme := Some("early-semver")
  )
  .jvmPlatform(scalaVersions = scala2)
  .jsPlatform(scalaVersions = scala2)
  .nativePlatform(scalaVersions = scala2)

lazy val examples = (projectMatrix in file("examples"))
  .dependsOn(core)
  .settings(
    scalacOptions ++= Seq("-Xexperimental", "-Xfuture"),
    name := "magnolia-examples",
    Compile / scalacOptions ++= Seq("-Ywarn-macros:after"),
    Compile / scalacOptions --= Seq("-Ywarn-unused:params"),
    publishArtifact := false,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(core)
  .jvmPlatform(scalaVersions = scala2)
  .jsPlatform(scalaVersions = scala2)
  .nativePlatform(scalaVersions = scala2)

lazy val test = (projectMatrix in file("test"))
  .dependsOn(examples)
  .settings(
    name := "magnolia-test",
    Test / scalacOptions += "-Ywarn-macros:after",
    Test / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    // `%%` is platform-aware in sbt 2; the JVM artifact is pinned to keep the sbt 1 behaviour, where tests are only run on the JVM
    // (JS & Native test linking fails, as the tests use java.io.ObjectInputStream)
    libraryDependencies += ("org.scalameta" %% "munit" % "1.0.0-M12" % Test).platform(Platform.jvm),
    publishArtifact := false
  )
  .jvmPlatform(scalaVersions = scala2)
  .jsPlatform(scalaVersions = scala2)
  .nativePlatform(scalaVersions = scala2)
