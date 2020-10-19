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
package com.lingeringsocket.shlurd

import scala.io._

import java.net._
import java.io._

object ResourceUtils
{
  class AugmentableLoader(parent : ClassLoader) extends URLClassLoader(
    Array[URL](), parent)
  {
    def addUrl(url : URL) : Unit =
    {
      super.addURL(url)
    }
  }

  private val loader = new AugmentableLoader(getClass.getClassLoader)

  def addUrl(url : URL) : Unit =
  {
    loader.addUrl(url)
  }

  def getResourcePath(resource : String) =
    getClass.getResource(resource).getPath

  def getResourceFile(resource : String) =
    new File(getResourcePath(resource))

  def getResourceStream(resource : String) =
      loader.getResourceAsStream(resource.stripPrefix("/"))

  def getResourceSource(resource : String) =
    Source.fromInputStream(getResourceStream(resource))

  def readResource(resource : String) : String =
    getResourceSource(resource).getLines().mkString("\n")
}
