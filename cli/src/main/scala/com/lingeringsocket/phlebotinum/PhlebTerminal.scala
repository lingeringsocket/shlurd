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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd.platonic._

import org.jline.terminal._
import org.jline.reader._

trait PhlebTerminal
{
  import PhlebShell.logger

  private var debugging = false

  def toggleDebug() : Unit =
  {
    debugging = !debugging
  }

  def isDebugging : Boolean = debugging

  def emitPrompt() : Unit =
  {
    logger.trace("PROMPT")
  }

  def emitDebug(msg : String) : Unit =
  {
    if (debugging) {
      emitDirect(msg)
    }
  }

  def emitTrace(msg : String) : Unit =
  {
    logger.trace(msg)
    emitDebug(msg)
  }

  def emitDirect(msg : String) : Unit

  def emitControl(msg : String) : Unit =
  {
    logger.trace(s"CONTROL $msg")
  }

  def emitNarrative(msg : String) : Unit =
  {
    if (!msg.isEmpty) {
      val formatted = s"NARRATIVE $msg"
      logger.debug(formatted)
    }
  }

  def emitVisualization(visualizer : SpcGraphVisualizer) : Unit =
  {
    visualizer.display()
  }

  def readCommand : Option[String] =
  {
    val result = readInput
    result.foreach(cmd => logger.debug(s"INPUT $cmd"))
    result
  }

  def readInput : Option[String] =
  {
    None
  }

  def getDefaultSaveFile : String =
  {
    "phlebotinum-save.zip"
  }

  // override this to return empty string if no init save desired
  def getInitSaveFile : String =
  {
    "init-save.zip"
  }

  def getTranslatorOpt : Option[PhlebTranslator] = None
}

class PhlebConsole extends PhlebTerminal
{
  val jlineTerminal = TerminalBuilder.builder.build
  val reader = LineReaderBuilder.builder.terminal(jlineTerminal).build
  val out = jlineTerminal.writer

  override def emitPrompt() : Unit =
  {
    super.emitPrompt()
  }

  override def emitControl(msg : String) : Unit =
  {
    super.emitControl(msg)
    out.println(s"[phlebotinum] $msg")
    jlineTerminal.flush
  }

  override def emitNarrative(msg : String) : Unit =
  {
    super.emitNarrative(msg)
    emitDirect(msg)
  }

  override def emitDirect(msg : String) : Unit =
  {
    out.println(msg)
  }

  override def readInput : Option[String] =
  {
    try {
      // FIXME what is going on with empty input?
      Option(reader.readLine("> "))
    } catch {
      case ex : EndOfFileException => {
        None
      }
    }
  }
}
