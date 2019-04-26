organization := Common.organization

name := "shlurd-cli"

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
  "com.twitter" %% "chill" % "0.9.3"
)

testOptions in Test += Tests.Setup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestSetup").newInstance)

testOptions in Test += Tests.Cleanup(
  (loader : java.lang.ClassLoader) => loader.loadClass("com.lingeringsocket.shlurd.ShlurdTestCleanup").newInstance)
