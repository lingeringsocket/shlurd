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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._

import scala.io._

class PhlebSpec extends Specification
{
  "PhlebApp" should
  {
    "interpret script" in
    {
      testScript("phlebotinum-script.txt")
    }

    "interpret conversations" in
    {
      testScript("phlebotinum-convo-script.txt")
    }
  }

  private def testScript(fileName : String) =
  {
    val terminal = new PhlebTestTerminal(fileName)
    PhlebShell.run("/example-phlebotinum/", terminal)
    terminal.nextScriptLine must beEmpty
  }

  class PhlebTestTerminal(fileName : String)
      extends PhlebTerminal
  {
    private val script = Source.fromFile(
      ResourceUtils.getResourceFile(s"/expect/$fileName")).
      getLines.zipWithIndex

    override def emitNarrative(msg : String)
    {
      super.emitNarrative(msg)
      if (!msg.isEmpty) {
        nextScriptLine match {
          case Some((expected, lineNo)) => {
            if (msg != expected) {
              reportErrorLocation(lineNo)
            }
            msg must be equalTo expected
          }
          case _ => {
            msg must be equalTo "EOF"
          }
        }
      }
    }

    override def emitVisualization(visualizer : SpcGraphVisualizer)
    {
      // just skip it
    }

    // FIXME do this the proper specs2 way
    private def reportErrorLocation(lineNo : Int)
    {
      val lineNoOneBased = lineNo + 1
      println(
        s"PhlebSpec FAIL at $fileName:$lineNoOneBased")
    }

    override def readInput() : Option[String] =
    {
      nextScriptLine match {
        case Some((cmd, lineNo)) => {
          if (!cmd.startsWith("> ")) {
            reportErrorLocation(lineNo)
          }
          cmd must startWith("> ")
          Some(cmd.stripPrefix("> "))
        }
        case _ => None
      }
    }

    override def getDefaultSaveFile() =
    {
      "phlebotinum-test-save.zip"
    }

    def nextScriptLine() : Option[(String, Int)] = {
      if (!script.hasNext) {
        None
      } else {
        val s = script.next
        if (SprParser.isIgnorableLine(s._1)) {
          nextScriptLine
        } else {
          Some(s)
        }
      }
    }
  }
}
