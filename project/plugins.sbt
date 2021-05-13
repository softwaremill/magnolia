addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.8.0")

val sbtSoftwareMillVersion = "2.0.3"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)