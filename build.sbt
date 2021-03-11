ThisBuild / scalaVersion := "3.0.0-RC1"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.17.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/propensive/magnolia"),
    "scm:git@github.com:propensive/magnolia.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "propensive",
    name  = "Jon Pretty",
    email = "jon.pretty@propensive.com",
    url   = url("https://twitter.com/propensive")
  )
)

ThisBuild / description := "Fast, easy and transparent typeclass derivation for Scala 2"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/propensive/magnolia"))

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

lazy val core = (project in file(".core"))
  .settings(
    name := "magnolia-core",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core",
  )

lazy val examples = (project in file(".examples"))
  .dependsOn(core)
  .settings(
    name := "magnolia-examples",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "examples",
  )

lazy val test = (project in file(".test"))
  .dependsOn(examples)
  .settings(
    name := "magnolia-test",
    projectDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.22"
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    Test / scalaSource := baseDirectory.value / ".." / "src" / "test",
  )

lazy val scala2Version = "2.12.13"

lazy val coreScala2 = (project in file(".core-scala2"))
  .settings(
    name := "magnolia-core-scala2",
    scalaVersion := scala2Version,
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core-scala2",
    libraryDependencies += "com.propensive" %% "mercator" % "0.2.1"
  )

lazy val examplesScala2 = (project in file(".examples-scala2"))
  .dependsOn(coreScala2)
  .settings(
    scalacOptions ++= Seq("-Xexperimental", "-Xfuture"),
    name := "magnolia-examples",
    scalaVersion := scala2Version,
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "examples-scala2",
  )

lazy val testScala2 = (project in file(".test-scala2"))
  .dependsOn(examplesScala2)
  .settings(
    name := "magnolia-test",
    scalaVersion := scala2Version,
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "test-scala2",
    libraryDependencies += "com.propensive" %% "probably-cli" % "0.5.0",
    libraryDependencies += "com.propensive" %% "contextual-examples" % "1.5.0"
  )