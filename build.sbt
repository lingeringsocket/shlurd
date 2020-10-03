organization := Common.organization

name := "shlurd"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

lazy val rootProject = (project in file("."))

lazy val cli = project.dependsOn(rootProject % "test->test;compile->compile")

lazy val corenlp = project.dependsOn(rootProject % "test->test;compile->compile")

lazy val mdocProject = (project in file ("mdoc")).dependsOn(cli)

lazy val `wordnet-bundle` = project.dependsOn(rootProject)

lazy val root = rootProject.aggregate(cli)

lazy val docs = project.in(file("generated-docs")).settings(
  mdoc := run.in(Compile).evaluated,
  mainClass in (Compile, run) := Some("com.lingeringsocket.shlurd.doc.MdocMain")
).dependsOn(mdocProject).enablePlugins(MdocPlugin)

resolvers ++= Common.resolvers

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-simple" % "1.7.26",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.1",
  "org.typelevel" %% "spire" % "0.16.0",
  "org.typelevel" %% "spire-extras" % "0.16.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.jgrapht" % "jgrapht-core" % "1.3.1",
  "org.jgrapht" % "jgrapht-io" % "1.3.1",
  "org.atteo" % "evo-inflector" % "1.2.2",
  "com.ibm.icu" % "icu4j" % "64.2",
  "net.sf.extjwnl" % "extjwnl" % "2.0.2",
  "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2",
  "com.lingeringsocket" % "extjwnl-data-mcr30-2016" % "0.0.1",
  "eus.ixa" % "ixa-pipe-ml" % "0.0.8" exclude("net.sourceforge.argparse4j", "argparse4j") exclude("com.google.guava", "guava") exclude("org.jdom", "jdom2") exclude("org.carrot2", "morfologik-stemming") exclude("org.apache.opennlp", "opennlp-tools")
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))

mainClass in Compile := Some("com.lingeringsocket.shlurd.cli.ShlurdCliApp")

fullClasspath in Runtime ++= (fullClasspath in cli in Runtime).value

scalacOptions in (Compile, console) := Common.scalacCommonOptions ++ Common.consoleOptions

scalacOptions in (Test, console) := Common.scalacCommonOptions ++ Common.consoleOptions

testOptions in Test += Tests.Setup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestSetup").getDeclaredConstructor().newInstance())

testOptions in Test += Tests.Cleanup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestCleanup").getDeclaredConstructor().newInstance())

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
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.jgrapht._
"""
