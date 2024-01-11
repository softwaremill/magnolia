addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.1")

val sbtSoftwareMillVersion = "2.0.18"
addSbtPlugin(
  "com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion
)
addSbtPlugin(
  "com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.16")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")
