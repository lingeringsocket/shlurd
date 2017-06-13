name := "shlurd"

organization := "com.lingeringsocket.shlurd"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

scalastyleFailOnError := true

scalacOptions := Seq(
  "-unchecked", "-feature", "-Xlint", "-Ywarn-unused-import",
  "-deprecation", "-Xfatal-warnings", "-Yrangepos")

maxErrors := 99

traceLevel := 10

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.5" % "test",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0" classifier "models"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))
