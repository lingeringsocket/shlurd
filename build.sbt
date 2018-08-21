organization := Common.organization

name := "shlurd"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

lazy val rootProject = (project in file("."))

lazy val cli = project.dependsOn(rootProject)

lazy val root = rootProject.aggregate(cli)

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "com.googlecode.kiama" %% "kiama" % "1.8.0",
  "org.typelevel" %% "spire" % "0.16.0",
  "org.typelevel" %% "spire-extras" % "0.16.0",
  "org.jgrapht" % "jgrapht-core" % "1.2.0",
  "org.jgrapht" % "jgrapht-io" % "1.2.0",
  "org.atteo" % "evo-inflector" % "1.2.2",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models-english"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))

mainClass in Compile := Some("com.lingeringsocket.shlurd.cli.ShlurdCliApp")

fullClasspath in Runtime ++= (fullClasspath in cli in Runtime).value

scalacOptions in (Compile, console) := Common.scalacCommonOptions :+ "-Yrepl-sync"

testOptions in Test += Tests.Setup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestSetup").newInstance)

testOptions in Test += Tests.Cleanup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestCleanup").newInstance)

if (sys.env.get("xonly").getOrElse("true") != "false") {
  Seq(
    testOptions in Test += Tests.Argument("xonly")
  )
} else {
  Seq()
}

initialCommands := """
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
"""
