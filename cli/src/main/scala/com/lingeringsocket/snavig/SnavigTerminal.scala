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
package com.lingeringsocket.snavig

import scala.io._

trait SnavigTerminal
{
  import SnavigShell.logger

  def emitPrompt()
  {
    logger.trace("PROMPT")
  }

  def emitControl(msg : String)
  {
    logger.trace(s"CONTROL $msg")
  }

  def emitNarrative(msg : String)
  {
    if (!msg.isEmpty) {
      logger.debug(s"NARRATIVE $msg")
    }
  }

  def readCommand() : Option[String] =
  {
    val result = readInput
    result.foreach(cmd => logger.debug(s"INPUT $cmd"))
    result
  }

  def readInput() : Option[String] =
  {
    None
  }

  def getDefaultSaveFile() : String =
  {
    "snavig-save.zip"
  }
}

class SnavigConsole extends SnavigTerminal
{
  override def emitPrompt()
  {
    super.emitPrompt
    print("> ")
  }

  override def emitControl(msg : String)
  {
    super.emitControl(msg)
    println(s"[Snavig] $msg")
  }

  override def emitNarrative(msg : String)
  {
    super.emitNarrative(msg)
    println(msg)
  }

  override def readInput() : Option[String] =
  {
    Option(StdIn.readLine)
  }
}