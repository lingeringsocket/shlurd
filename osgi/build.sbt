organization := "com.lingeringsocket.shlurd"

name := "shlurd-osgi"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.12"

defaultSingleProjectSettings

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "com.lingeringsocket.shlurd" %% "shlurd" % "0.1-SNAPSHOT",
  "com.lingeringsocket.shlurd" %% "shlurd-corenlp-models" % "0.1-SNAPSHOT" classifier "assembly"
)

import com.typesafe.sbt.osgi.OsgiKeys._

osgiRepositoryRules := Seq(ignoreAll("xalan", "lucene-sandbox", "xercesImpl", "lucene-queryparser", "lucene-core", "lucene-analyzers-common", "lucene-queries", "AppleJavaExtensions", "stanford-corenlp", "jollyday", "xml-apis", "commons-lang3", "jaxb-api", "jline", "jsr305", "machinist_2.11", "spire-macros_2.11"), rewrite("shlurd-corenlp-models_2.11", imports="com.sun.net.httpserver.*;resolution:=optional,nu.xom.*;resolution:=optional,de.jollyday.*;resolution:=optional,javax.xml.*;resolution:=optional,org.slf4j.*;resolution:=optional,*"), rewrite("protobuf-java"), rewrite("javax.json"), rewrite("scala-reflect"), rewrite("scala-parser-combinators_2.11"), rewrite("guava", imports="com.google.errorprone.annotations.concurrent.*;resolution:=optional,*"), rewrite("joda-time", imports="org.joda.convert.*;resolution:=optional,*"), rewrite("kiama_2.11", imports="jline.*;resolution:=optional,*"), rewrite("jgrapht-core"))

osgiDependencies in Compile := packageReqs("com.lingeringsocket.shlurd.platonic")
