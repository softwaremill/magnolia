// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.crossProject
import com.softwaremill.PublishTravis.publishTravisSettings

val v2_12 = "2.12.10"
val v2_13 = "2.13.1"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .settings(buildSettings)
  .settings(publishSettings)
  .settings(scalaMacroDependencies)
  .settings(moduleName := "magnolia")
  .settings(libraryDependencies ++= Seq("com.propensive" %% "mercator" % "0.2.1"))

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val examples = crossProject(JVMPlatform, JSPlatform)
  .in(file("examples"))
  .settings(buildSettings)
  .settings(noPublishSettings)
  .settings(moduleName := "magnolia-examples")
  .dependsOn(core)

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js

lazy val tests = project
  .in(file("tests"))
  .settings(buildSettings)
  .settings(noPublishSettings)
  .settings(unmanagedSettings)
  .settings(moduleName := "magnolia-tests")
  .settings(
    initialCommands in console := """import magnolia.tests._; import magnolia.examples._;""",
    libraryDependencies ++= Seq(
      // These two to allow compilation under Java 9...
      // Specifically to import XML stuff that got modularised
      "javax.xml.bind" % "jaxb-api" % "2.3.0" % "compile",
      "com.sun.xml.bind" % "jaxb-impl" % "2.3.0" % "compile"
    )
  )
  // compiling and running the tests only for 2.12
  .settings(skip := scalaVersion.value != v2_12)
  .settings(test := Def.taskDyn { if (scalaVersion.value == v2_12) (run in Compile).toTask("") else Def.task {} }.value)
  .dependsOn(examplesJVM)

lazy val root = (project in file("."))
  .aggregate(coreJVM, coreJS, examplesJVM, examplesJS, tests)
  .settings(buildSettings)
  .settings(publishSettings)
  .settings(publishTravisSettings)
  .settings(noPublishSettings)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(buildSettings: _*)
  .settings(moduleName := "magnolia-benchmarks")
  .dependsOn(examplesJVM)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  name := "magnolia",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-deprecation",
    "-feature",
    "-Ywarn-value-discard",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Xexperimental",
          "-Xfuture",
          "-Ywarn-nullary-unit",
          "-Ywarn-inaccessible",
          "-Ywarn-adapted-args"
        )
      case _ =>
        Nil
    }
  },
  scmInfo := Some(
    ScmInfo(url("https://github.com/propensive/mercator"),
      "scm:git:git@github.com:propensive/mercator.git")
  ),
  crossScalaVersions := v2_12 :: v2_13 :: Nil,
  scalaVersion := crossScalaVersions.value.head
)

lazy val publishSettings = ossPublishSettings ++ Seq(
  homepage := Some(url("http://propensive.com/")),
  organizationHomepage := Some(url("http://propensive.com/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(
      id = "propensive",
      name = "Jon Pretty",
      email = "",
      url = new URL("https://github.com/propensive/mercator/")
    )
  ),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/propensive/" + name.value),
      "scm:git:git@github.com/propensive/" + name.value + ".git"
    )
  ),
  sonatypeProfileName := "com.propensive",
)

lazy val unmanagedSettings = unmanagedBase :=
  baseDirectory.value / "lib" /
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((major, minor)) => s"$major.$minor"
      case _ => ""
    })

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
)
