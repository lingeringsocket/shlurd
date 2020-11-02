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
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._

import java.io._
import scala.io._
import scala.util._

class PhlebSpec extends Specification
{
  "PhlebApp" should
  {
    "interpret script" in
    {
      val maxFileSize = 1000000
      testScript("phlebotinum-script.txt")

      // make sure we didn't bloat the serialization
      val initLength = new File("run/example-phlebotinum/init-save.zip").length
      initLength must be lessThan maxFileSize
      val saveLength = new File("run/phlebotinum-test-save.zip").length
      saveLength must be lessThan maxFileSize
    }

    "interpret conversations" in
    {
      testScript("phlebotinum-convo-script.txt")
    }

    "interpret Spanish script" in
    {
      val translator = new PhlebTranslator(
        SnlUtils.spanishEnglishAlignment,
        TRANSLATE_FIRST_TO_SECOND
      )
      testScript("phlebotinum-spanish-script.txt", Some(translator))
    }
  }

  private def testScript(
    fileName : String,
    translatorOpt : Option[PhlebTranslator] = None) =
  {
    // preload
    PhlebBaseline.frozenCosmos

    val terminal = new PhlebTestTerminal(fileName, translatorOpt)
    PhlebShell.run("/example-phlebotinum/", terminal)
    terminal.nextScriptLine must beEmpty
  }

  class PhlebTestTerminal(
    fileName : String,
    translatorOpt : Option[PhlebTranslator] = None
  ) extends PhlebTerminal
  {
    private val script = Using.resource(Source.fromFile(
      ResourceUtils.getResourceFile(s"/expect/$fileName")
    )) {
      source => source.getLines().zipWithIndex.toSeq.iterator
    }

    override def emitNarrative(msg : String) : Unit =
    {
      super.emitNarrative(msg)
      emitDirect(msg)
    }

    override def emitDirect(msg : String) : Unit =
    {
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

    override def emitVisualization(visualizer : SpcGraphVisualizer) : Unit =
    {
      // just skip it
    }

    // FIXME do this the proper specs2 way
    private def reportErrorLocation(lineNo : Int) : Unit =
    {
      val lineNoOneBased = lineNo + 1
      println(
        s"PhlebSpec FAIL at $fileName:$lineNoOneBased")
    }

    override def readInput : Option[String] =
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

    override def getDefaultSaveFile =
    {
      "phlebotinum-test-save.zip"
    }

    override def getTranslatorOpt =
    {
      translatorOpt
    }

    def nextScriptLine : Option[(String, Int)] = {
      if (!script.hasNext) {
        None
      } else {
        val s = script.next()
        if (SprParser.isIgnorableLine(s._1)) {
          nextScriptLine
        } else {
          Some(s)
        }
      }
    }
  }
}
