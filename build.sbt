import com.softwaremill.UpdateVersionInDocs

val scala2 = "2.12.13"

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  scalaVersion := scala2,
  organization := "com.softwaremill.magnolia",
  description := "Fast, easy and transparent typeclass derivation for Scala 2",
  updateDocs := UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("readme.md")))
)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(name := "magnolia", publishArtifact := false)
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
  .jvmPlatform(scalaVersions = List(scala2))

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
  .jvmPlatform(scalaVersions = List(scala2))

lazy val test = (projectMatrix in file("test"))
  .dependsOn(examples)
  .settings(commonSettings)
  .settings(
    name := "magnolia-test",
    Test / scalacOptions += "-Ywarn-macros:after",
    Test / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test
  )
  .jvmPlatform(scalaVersions = List(scala2))
