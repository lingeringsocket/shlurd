name := "shlurd"

ThisBuild / organization := Common.organization

ThisBuild / version := Common.version

ThisBuild / scalaVersion := Common.scalaVersion

ThisBuild / githubWorkflowBuildPreamble := Seq(
  WorkflowStep.Sbt(List(
    "scalastyle",
    "test:scalastyle",
    "corenlp/scalastyle"
  ), name = Some("Scalastyle")),
  WorkflowStep.Run(List(
    "sudo apt-get install graphviz",
    "wget https://github.com/lingeringsocket/morphala/archive/main.zip",
    "unzip main.zip",
    "pushd morphala-main && sbt ++${{ matrix.scala }} clean compile publishLocal && popd"
  ), name = Some("Install Prerequisites"))
)

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List(
    "test"
  ), name = Some("Build and Test"), env = Common.jvmEnv),
  WorkflowStep.Sbt(List(
    "corenlp/test"
  ), name = Some("Build and Test CoreNLP"), env = Common.jvmEnv),
  WorkflowStep.Run(List(
    "mdoc/bin/buildDocs"
  ), name = Some("Build Documentation"), env = Common.jvmEnv),
  WorkflowStep.Use(
    UseRef.Public("JamesIves", "github-pages-deploy-action", "3.7.1"),
    name = Some("Publish Documentation"),
    params = Map(
      "ACCESS_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}",
      "BRANCH" -> "gh-pages",
      "FOLDER" -> "generated-docs/target/mdoc",
      "CLEAN" -> "true"
    )
  )
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

scalastyleFailOnError := true

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel.withRank(KeyRanks.Invisible) := Common.traceLevel

lazy val rootProject = (project in file("."))

lazy val cli = project.dependsOn(rootProject % "test->test;compile->compile")

lazy val corenlp = project.dependsOn(rootProject % "test->test;compile->compile")

lazy val mdocProject = (project in file ("mdoc")).dependsOn(cli)

lazy val `wordnet-bundle` = project.dependsOn(rootProject)

lazy val root = rootProject.aggregate(cli)

lazy val docs = project.in(file("generated-docs")).settings(
  mdoc := run.in(Compile).evaluated,
  mainClass in (Compile, run) := Some("com.lingeringsocket.shlurd.doc.MdocMain"),
  scalaVersion := Common.scalaVersion
).dependsOn(mdocProject).enablePlugins(MdocPlugin)

resolvers ++= Common.resolvers

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-simple" % "1.7.26",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
  "org.typelevel" %% "spire" % "0.17.0",
  "org.typelevel" %% "spire-extras" % "0.17.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
  "org.jgrapht" % "jgrapht-core" % "1.3.1",
  "org.jgrapht" % "jgrapht-io" % "1.3.1",
  "com.ibm.icu" % "icu4j" % "64.2",
  "net.sf.extjwnl" % "extjwnl" % "2.0.2",
  "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2",
  "net.sf.extjwnl.mcr" % "extjwnl-data-spa-mcr30" % "1.0.5",
  "net.sf.extjwnl.mcr" % "extjwnl-data-alignment-mcr30" % "1.0.5",
  "com.lingeringsocket" %% "morphala" % "0.1-SNAPSHOT",
  "eus.ixa" % "ixa-pipe-ml" % "0.0.8" exclude("net.sourceforge.argparse4j", "argparse4j") exclude("com.google.guava", "guava") exclude("org.jdom", "jdom2") exclude("org.carrot2", "morfologik-stemming") exclude("org.apache.opennlp", "opennlp-tools")
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))

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
