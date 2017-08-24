organization := "com.lingeringsocket.shlurd"

name := "shlurd-models"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

maxErrors := 99

traceLevel := 10

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0" classifier "models"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))

assemblyExcludedJars in assembly := { 
  val cp = (fullClasspath in assembly).value
  cp filter {!_.data.getName.startsWith("stanford-corenlp")}
}

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

test in assembly := {}
