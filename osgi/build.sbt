organization := "com.lingeringsocket.shlurd"

name := "shlurd-osgi"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.12"

defaultSingleProjectSettings

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "com.lingeringsocket.shlurd" %% "shlurd" % "0.1-SNAPSHOT",
  "com.lingeringsocket.shlurd" %% "shlurd-wordnet-bundle" % "0.1-SNAPSHOT" classifier "assembly"
)

import com.typesafe.sbt.osgi.OsgiKeys._

osgiRepositoryRules := Seq(ignoreAll("commons-lang3", "commons-text", "jline", "jsr305", "machinist_2.11", "spire-macros_2.11", "extjwnl", "extjwnl-data-wn31"), rewrite("shlurd-wordnet-bundle_2.11", imports="org.slf4j.*;resolution:=optional,*"), rewrite("scala-reflect"), rewrite("scala-parser-combinators_2.11"), rewrite("guava", imports="com.google.errorprone.annotations.concurrent.*;resolution:=optional,*"), rewrite("kiama_2.11", imports="jline.*;resolution:=optional,*"), rewrite("jgrapht-core"), rewrite("jgrapht-io", imports="org.apache.commons.lang3.*;resolution:=optional,org.apache.commons.text.*;resolution:=optional,org.apache.commons.text.translate.*;resolution:=optional,org.antlr.v4.runtime.*;resolution:=optional,org.antlr.v4.runtime.atn.*;resolution:=optional,org.antlr.v4.runtime.dfa.*;resolution:=optional,org.antlr.v4.runtime.misc.*;resolution:=optional,org.antlr.v4.runtime.tree.*;resolution:=optional,*"), rewrite("concurrentlinkedhashmap-lru"), rewrite("ixa-pipe-ml", imports="opennlp.tools.dictionary.*;resolution:=optional,opennlp.tools.ml.*;resolution:=optional,opennlp.tools.util.model.*;resolution:=optional,net.sourceforge.argparse4j.inf.*;resolution:=optional,morfologik.stemming.*;resolution:=optional,org.jdom2.output.*;resolution:=optional,opennlp.tools.cmdline.*;resolution:=optional,opennlp.tools.ngram.*;resolution:=optional,net.sourceforge.argparse4j.*;resolution:=optional,opennlp.tools.ml.model.*;resolution:=optional,opennlp.tools.tokenize.*;resolution:=optional,opennlp.tools.util.ext.*;resolution:=optional,opennlp.tools.parser.*;resolution:=optional,opennlp.tools.util.featuregen.*;resolution:=optional,opennlp.tools.util.*;resolution:=optional,opennlp.tools.util.eval.*;resolution:=optional,org.jdom2.*;resolution:=optional"))

osgiDependencies in Compile := packageReqs("com.lingeringsocket.shlurd.platonic")
