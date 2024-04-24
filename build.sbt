import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{updateDocs, ossPublishSettings}
import com.softwaremill.UpdateVersionInDocs

val scala2_12 = "2.12.19"
val scala2_13 = "2.13.13"
val scala2 = List(scala2_12, scala2_13)

excludeLintKeys in Global ++= Set(ideSkipProject)
ThisBuild / dynverTagPrefix := "scala2-v" // a custom prefix is needed to differentiate tags between scala2 & scala3 versions

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.magnolia1_2",
  description := "Fast, easy and transparent typeclass derivation for Scala 2",
  updateDocs := UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("readme.md"))),
  ideSkipProject := (scalaVersion.value == scala2_12) // only import 2.13 projects
)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(name := "magnolia-root", publishArtifact := false, scalaVersion := scala2_13)
    .aggregate((core.projectRefs ++ examples.projectRefs ++ test.projectRefs): _*)

lazy val core = (projectMatrix in file("core"))
  .settings(commonSettings)
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
  .settings(commonSettings)
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
  .settings(commonSettings)
  .settings(
    name := "magnolia-test",
    Test / scalacOptions += "-Ywarn-macros:after",
    Test / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M12" % Test,
    publishArtifact := false
  )
  .jvmPlatform(scalaVersions = scala2)
  .jsPlatform(scalaVersions = scala2)
  .nativePlatform(scalaVersions = scala2)
