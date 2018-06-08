organization := Common.organization

name := "shlurd-cli"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

val scalacCommonOptions = Common.scalacCommonOptions

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Seq(
  "com.twitter" %% "chill" % "0.9.2"
)
