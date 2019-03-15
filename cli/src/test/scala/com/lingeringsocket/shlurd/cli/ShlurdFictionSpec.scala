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
package com.lingeringsocket.shlurd.cli

import com.lingeringsocket.shlurd._

import org.specs2.mutable._

import scala.io._

class ShlurdFictionSpec extends Specification
{
  "ShlurdFictionApp" should
  {
    "interpret script" in
    {
      val fileName = "fiction-script.txt"
      val script = Source.fromFile(
        ResourceUtils.getResourceFile(s"/expect/$fileName")).
        getLines.zipWithIndex
      def nextScriptLine() : Option[(String, Int)] = {
        if (!script.hasNext) {
          None
        } else {
          val s = script.next
          if (s._1.isEmpty) {
            nextScriptLine
          } else {
            Some(s)
          }
        }
      }
      val (phenomenalMind, noumenalMind) =
        ShlurdFictionShell.createNewCosmos
      val terminal = new ShlurdFictionTerminal {
        override def emitNarrative(msg : String)
        {
          super.emitNarrative(msg)
          if (!msg.isEmpty) {
            nextScriptLine match {
              case Some((expected, lineNo)) => {
                // FIXME do this the proper specs2 way
                if (msg != expected) {
                  val lineNoOneBased = lineNo + 1
                  println(
                    s"ShlurdFictionSpec FAIL at $fileName:$lineNoOneBased")
                }
                msg must be equalTo expected
              }
              case _ => {
                msg must be equalTo "EOF"
              }
            }
          }
        }

        override def readInput() : Option[String] =
        {
          nextScriptLine.map(_._1.stripPrefix("> "))
        }
      }
      val shell = new ShlurdFictionShell(
        phenomenalMind, noumenalMind, terminal)
      shell.init
      shell.run
      nextScriptLine must beEmpty
    }
  }
}
