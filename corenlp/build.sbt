organization := Common.organization

name := "shlurd-corenlp"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

val scalacCommonOptions = Common.scalacCommonOptions

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

resolvers ++= Common.resolvers

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Seq(
  "org.jline" % "jline" % "3.11.0",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.1",
  "com.twitter" %% "chill" % "0.9.3",
  "com.ibm.icu" % "icu4j" % "64.2",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models-english"
)

scalacOptions in (Compile, console) := Common.scalacCommonOptions :+ "-Yrepl-sync"

scalacOptions in (Test, console) := Common.scalacCommonOptions :+ "-Yrepl-sync"

testOptions in Test += Tests.Setup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.corenlp.CorenlpTestSetup").newInstance)

testOptions in Test += Tests.Cleanup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.corenlp.CorenlpTestCleanup").newInstance)

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
import com.lingeringsocket.shlurd.jgrapht._
import com.lingeringsocket.shlurd.corenlp._
"""
