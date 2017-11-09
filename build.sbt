import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import ReleaseTransformations._

import sbtcrossproject.{crossProject, CrossType}

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scalaMacroDependencies: _*)
  .settings(examplesDependencies)
  .settings(moduleName := "magnolia")
  .nativeSettings(nativeSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("examples"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(moduleName := "magnolia-examples")
  .settings(quasiQuotesDependencies)
  .settings(examplesDependencies)
  .nativeSettings(nativeSettings)
  .dependsOn(core)

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tests"))
  .settings(buildSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(moduleName := "magnolia-tests")
  .settings(quasiQuotesDependencies)
  .nativeSettings(nativeSettings)
  .dependsOn(examples)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js
lazy val testsNative = tests.native

lazy val benchmarks = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("benchmarks"))
  .settings(buildSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(moduleName := "magnolia-benchmarks")
  .settings(quasiQuotesDependencies)
  .nativeSettings(nativeSettings)
  .dependsOn(examples)

lazy val benchmarksJVM = benchmarks.jvm
lazy val benchmarksJS = benchmarks.js
lazy val benchmarksNative = benchmarks.native

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.12.4",
  name := "magnolia",
  version := "0.4.0",
  scalacOptions ++= Seq("-deprecation", "-feature", "-Ywarn-value-discard", "-Ywarn-dead-code", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen", "-Ywarn-inaccessible", "-Ywarn-adapted-args"),
  crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2"),
  scmInfo := Some(ScmInfo(url("https://github.com/propensive/magnolia"),
    "scm:git:git@github.com:propensive/magnolia.git"))
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://magnolia.propensive.com/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if(isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>propensive</id>
        <name>Jon Pretty</name>
        <url>https://github.com/propensive/magnolia/</url>
      </developer>
    </developers>
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  ),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

import java.io.File

def crossVersionSharedSources()  = Seq(
 (unmanagedSourceDirectories in Compile) ++= { (unmanagedSourceDirectories in Compile ).value.map {
     dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)}}
)

lazy val nativeSettings: Seq[Setting[_]] = Seq(
  // Scala Native not yet available for 2.12.x, so override the versions
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.10.6", "2.11.11")
)

lazy val quasiQuotesDependencies: Seq[Setting[_]] =
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      case Some((2, 10)) => Seq(
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
        "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary
      )
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  libraryDependencies += compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val examplesDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.typelevel" %% "cats-core" % "0.9.0",
  libraryDependencies += "com.propensive" %% "estrapade" % "1.0.3",
  libraryDependencies += "com.propensive" %% "contextual-data" % "1.0.3"
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
