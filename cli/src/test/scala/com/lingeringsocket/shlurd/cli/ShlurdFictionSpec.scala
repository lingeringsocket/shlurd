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

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import scala.io._

class ShlurdFictionSpec extends Specification
{
  "ShlurdFictionApp" should
  {
    "interpret script" in
    {
      val script = Source.fromFile(
        SprParser.getResourceFile("/expect/fiction-script.txt")).getLines
      def nextScriptLine() : Option[String] = {
        if (!script.hasNext) {
          None
        } else {
          val s = script.next
          if (s.isEmpty) {
            nextScriptLine
          } else {
            Some(s)
          }
        }
      }
      val mind = ShlurdFictionShell.newMind
      val terminal = new ShlurdFictionTerminal {
        override def emitPrompt()
        {
        }

        override def emitControl(msg : String)
        {
        }

        override def emitNarrative(msg : String)
        {
          if (!msg.isEmpty) {
            nextScriptLine match {
              case Some(expected) => {
                msg must be equalTo expected
              }
              case _ => {
                msg must be equalTo "EOF"
              }
            }
          }
        }

        override def readCommand() : Option[String] =
        {
          val command = nextScriptLine
          command
        }
      }
      val shell = new ShlurdFictionShell(mind, terminal)
      shell.init
      shell.run
      nextScriptLine must beEmpty
    }
  }
}
