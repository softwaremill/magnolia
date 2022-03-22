import com.softwaremill.UpdateVersionInDocs
import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{updateDocs, ossPublishSettings}

val scala3 = "3.1.1"

ThisBuild / dynverTagPrefix := "scala3-v" // a custom prefix is needed to differentiate tags between scala2 & scala3 versions

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  scalaVersion := scala3,
  organization := "com.softwaremill.magnolia1_3",
  description := "Fast, easy and transparent typeclass derivation for Scala 3",
  updateDocs := UpdateVersionInDocs(
    sLog.value,
    organization.value,
    version.value,
    List(file("readme.md"))
  )
)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(name := "magnolia-root", publishArtifact := false)
    .aggregate(
      (core.projectRefs ++ examples.projectRefs ++ test.projectRefs): _*
    )

lazy val core = (projectMatrix in file(".core"))
  .settings(commonSettings)
  .settings(
    name := "magnolia",
    Compile / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "core",
    mimaPreviousArtifacts := {
      val current = version.value
      val isRcOrMilestone = current.contains("M") || current.contains("RC")
      if (!isRcOrMilestone) {
        val previous = previousStableVersion.value
        println(
          s"[info] Not a M or RC version, using previous version for MiMa check: $previous"
        )
        previousStableVersion.value
          .map(organization.value %% moduleName.value % _)
          .toSet
      } else {
        println(
          s"[info] $current is an M or RC version, no previous version to check with MiMa"
        )
        Set.empty
      }
    },
    versionScheme := Some("early-semver")
  )
  .jvmPlatform(scalaVersions = List(scala3))
  .jsPlatform(scalaVersions = List(scala3))
  .nativePlatform(scalaVersions = List(scala3))

lazy val examples = (projectMatrix in file(".examples"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "magnolia-examples",
    Compile / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "examples",
    publishArtifact := false
  )
  .jvmPlatform(scalaVersions = List(scala3))
  .jsPlatform(scalaVersions = List(scala3))
  .nativePlatform(scalaVersions = List(scala3))

lazy val test = (projectMatrix in file(".test"))
  .dependsOn(examples)
  .settings(commonSettings)
  .settings(
    name := "magnolia-test",
    projectDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.26"
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    Test / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "test",
    publishArtifact := false
  )
  .jvmPlatform(scalaVersions = List(scala3))
  .jsPlatform(scalaVersions = List(scala3))
  .nativePlatform(scalaVersions = List(scala3))
