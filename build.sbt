organization := "com.lingeringsocket.shlurd"

name := "shlurd"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.12"

scalastyleFailOnError := true

scalacOptions := Seq(
  "-unchecked", "-feature", "-Xlint", "-Ywarn-unused-import",
  "-deprecation", "-Xfatal-warnings", "-Yrangepos")

maxErrors := 99

traceLevel := 10

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.0.3" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "com.googlecode.kiama" %% "kiama" % "1.8.0",
  "org.typelevel" %% "spire" % "0.14.1",
  "org.jgrapht" % "jgrapht-core" % "1.1.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models-english"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))
