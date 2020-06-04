// shlurd:  a limited understanding of small worlds
// Copyright 2017-2018 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
import sbt._
import Keys._

object Common
{
  def organization = "com.lingeringsocket.shlurd"

  def version = "0.1-SNAPSHOT"

  def scalaVersion = "2.12.11"

  def scalacCommonOptions = Seq(
    "-unchecked", "-feature", "-Xlint",
    "-deprecation", "-Xfatal-warnings", "-Yrangepos")

  def scalacOptions = scalacCommonOptions :+ "-Ywarn-unused-import"

  def consoleOptions = Seq("-Yrepl-sync","-Ywarn-unused:-imports")

  def maxErrors = 99

  def traceLevel = 100

  def specs2Deps = Seq(
    "org.specs2" %% "specs2-core" % "4.0.3" % "test")

  def resolvers = Seq(Resolver.sonatypeRepo("snapshots"))
}
