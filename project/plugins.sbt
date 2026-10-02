val sbtSoftwareMillVersion = "3.0.1"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.2.1")

addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
