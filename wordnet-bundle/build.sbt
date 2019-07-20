organization := Common.organization

name := "shlurd-wordnet-bundle"

version := Common.version

scalaVersion := Common.scalaVersion

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

libraryDependencies ++= Seq(
  "net.sf.extjwnl" % "extjwnl" % "2.0.2",
  "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket.shlurd")))

assemblyExcludedJars in assembly := { 
  val cp = (fullClasspath in assembly).value
  cp filter {entry => !entry.data.getName.startsWith("extjwnl")}
}

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.withClassifier(Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

test in assembly := {}
