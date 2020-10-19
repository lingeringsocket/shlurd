organization := Common.organization

name := "shlurd-mdoc"

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

val scalacCommonOptions = Common.scalacCommonOptions

scalacOptions := Common.scalacOptions

maxErrors := Common.maxErrors

traceLevel.withRank(KeyRanks.Invisible) := Common.traceLevel

resolvers ++= Common.resolvers

libraryDependencies ++= Seq(
  "org.scalameta" %% "mdoc" % "2.2.10"
)
