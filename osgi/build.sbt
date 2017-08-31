organization := "com.lingeringsocket.shlurd"

name := "shlurd-osgi"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

defaultSingleProjectSettings

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "com.lingeringsocket.shlurd" %% "shlurd" % "0.1-SNAPSHOT",
  "com.lingeringsocket.shlurd" %% "shlurd-models" % "0.1-SNAPSHOT" classifier "assembly"
)

import com.typesafe.sbt.osgi.OsgiKeys._

osgiRepositoryRules := Seq(ignoreAll("xalan", "lucene-sandbox", "xercesImpl", "lucene-queryparser", "lucene-core", "lucene-analyzers-common", "lucene-queries", "AppleJavaExtensions", "stanford-corenlp"), rewrite("shlurd-models_2.11", imports= "com.sun.net.httpserver.*;resolution:=optional,nu.xom.*;resolution:=optional,*"), rewrite("protobuf-java"), rewrite("javax.json"), rewrite("commons-lang3"), rewrite("slf4j-api"), rewrite("slf4j-simple"), rewrite("scala-reflect"), rewrite("scala-parser-combinators_2.11"), rewrite("guava", imports="com.google.errorprone.annotations.concurrent.*;resolution:=optional,*"), rewrite("jline"), rewrite("joda-time", imports="org.joda.convert.*;resolution:=optional,*"))

osgiDependencies in Compile := packageReqs("com.lingeringsocket.shlurd.world")
