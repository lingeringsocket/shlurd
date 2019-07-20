organization := Common.organization

name := "shlurd-mdoc"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

val scalacCommonOptions = Common.scalacCommonOptions

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

resolvers ++= Common.resolvers

libraryDependencies ++= Seq(
  "org.scalameta" %% "mdoc" % "1.3.1",
)
