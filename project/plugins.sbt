addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.scala-sbt.plugins" % "sbt-onejar" % "0.8")

lazy val root = project.in( file(".") ).dependsOn( assemblyPlugin )

lazy val assemblyPlugin = uri("git://github.com/ensime/ensime-sbt-cmd")
