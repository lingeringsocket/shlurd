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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.io._

import java.io._

object ShlurdCliApp extends App
{
  val serializer = new ShlurdCliSerializer
  val file = new File("run/shlurd-mind.zip")
  val terminal = new ShlurdCliTerminal
  val mind = loadOrCreate(file, terminal)
  val shell = new ShlurdCliShell(mind, terminal)
  shell.run
  serializer.saveMind(mind, file)

  private def loadOrCreate(
    file : File,
    terminal : ShlurdCliTerminal) : ShlurdCliMind =
  {
    if (file.exists) {
      terminal.emitControl("Awaking from kryo...")
      val oldMind = serializer.loadMind(file)
      terminal.emitControl("Hello again, human!")
      oldMind
    } else {
      terminal.emitControl("Loading initial beliefs...")
      ShlurdCliShell.newMind(terminal)
    }
  }
}

class ShlurdCliTerminal
{
  def emitPrompt()
  {
    print("> ")
  }

  def emitControl(msg : String)
  {
    if (msg.isEmpty) {
      println
    } else {
      emitResponse(msg)
    }
  }

  def emitResponse(msg : String)
  {
    println(s"[SHLURD] $msg")
  }

  def readCommand() : Option[String] =
  {
    Option(StdIn.readLine)
  }
}

object ShlurdCliShell
{
  def newMind(terminal : ShlurdCliTerminal) =
  {
    val cosmos = ShlurdPrimordialWordnet.loadCosmos
    val beliefs = SprParser.getResourceFile("/ontologies/console.txt")
    val source = Source.fromFile(beliefs)
    cosmos.loadBeliefs(source)

    val entityInterviewer = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        "interviewer", REF_SUBJECT, Set())).get
    val entityShlurd = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        SmcLemmas.LEMMA_PERSON, REF_SUBJECT, Set("shlurd"))).get

    terminal.emitControl("Hello, human!")
    new ShlurdCliMind(cosmos, entityInterviewer, entityShlurd, true)
  }
}

class ShlurdCliShell(
  mind : ShlurdCliMind,
  terminal : ShlurdCliTerminal
)
{
  private val params = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)

  private val interpreter = new SpcInterpreter(
    mind, ACCEPT_MODIFIED_BELIEFS, params)

  def run()
  {
    mind.startConversation
    var exit = false
    terminal.emitControl("")
    while (!exit) {
      terminal.emitPrompt
      terminal.readCommand match {
        case Some(input) => {
          val sentences = mind.newParser(input).parseAll
          sentences.foreach(sentence => {
            val output = interpreter.interpret(sentence)
            terminal.emitControl("")
            terminal.emitResponse(output)
            terminal.emitControl("")
          })
        }
        case _ => {
          exit = true
        }
      }
    }
    terminal.emitControl("")
    terminal.emitControl("Shutting down...")
    // don't serialize conversation since that could be an extra source of
    // deserialization problems later
    mind.stopConversation
  }
}
