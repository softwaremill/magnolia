import com.softwaremill.UpdateVersionInDocs

val scala3 = "3.0.0-RC3"

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  scalaVersion := scala3,
  organization := "com.softwaremill.magnolia",
  description := "Fast, easy and transparent typeclass derivation for Scala 3",
  updateDocs := UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("readme.md")))
)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(publishArtifact := false)
    .aggregate((core.projectRefs ++ examples.projectRefs ++ test.projectRefs): _*)

lazy val core = (projectMatrix in file(".core"))
  .settings(
    name := "magnolia-core",
    Compile / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "core",
  )
  .jvmPlatform(scalaVersions = List(scala3))

lazy val examples = (projectMatrix in file(".examples"))
  .dependsOn(core)
  .settings(
    name := "magnolia-examples",
    Compile / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "examples",
  )
  .jvmPlatform(scalaVersions = List(scala3))

lazy val test = (projectMatrix in file(".test"))
  .dependsOn(examples)
  .settings(
    name := "magnolia-test",
    projectDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.26"
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    Test / scalaSource := baseDirectory.value / ".." / ".." / ".." / "src" / "test",
    publishArtifact := false,
  )
  .jvmPlatform(scalaVersions = List(scala3))
