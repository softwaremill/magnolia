import com.typesafe.sbt.pgp.PgpKeys.publishSigned

lazy val core = project
  .in(file("core"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scalaMacroDependencies: _*)
  .settings(moduleName := "magnolia")

lazy val examples = project
  .in(file("examples"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(moduleName := "magnolia-examples")
  .dependsOn(core)

lazy val tests = project
  .in(file("tests"))
  .settings(buildSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(unmanagedSettings)
  .settings(moduleName := "magnolia-tests")
  .dependsOn(examples)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(buildSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(moduleName := "magnolia-benchmarks")
  .dependsOn(examples)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.12.4",
  name := "magnolia",
  version := "0.5.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Ywarn-value-discard",
    "-Ywarn-dead-code",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-inaccessible",
    "-Ywarn-adapted-args"
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/propensive/magnolia"),
            "scm:git:git@github.com:propensive/magnolia.git")
  )
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://magnolia.work/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
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
  )
)

lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

import java.io.File

lazy val unmanagedSettings = unmanagedBase := (scalaVersion.value.split("\\.").map(_.toInt).to[List] match {
  case List(2, 12, _) => baseDirectory.value / "lib" / "2.12"
  case List(2, 11, _) => baseDirectory.value / "lib" / "2.11"
})

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  libraryDependencies += compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
  )
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield
  Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
