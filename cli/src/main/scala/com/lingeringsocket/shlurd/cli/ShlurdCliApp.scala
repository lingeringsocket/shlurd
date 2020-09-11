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
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.collection._
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
    val cosmos = ShlurdPrincetonPrimordial.newMutableCosmos
    val beliefs = ResourceUtils.getResourceFile("/console/beliefs.txt")
    val source = Source.fromFile(beliefs)
    val preferredSynonyms = new mutable.LinkedHashMap[SpcIdeal, String]
    val bootMind = new SpcWordnetOntologyMind(
      SnlUtils.defaultTongue, cosmos, preferredSynonyms)
    bootMind.loadBeliefs(source)

    terminal.emitControl("Hello, human!")
    new ShlurdCliMind(cosmos, preferredSynonyms)
  }
}

class ShlurdCliShell(
  mind : ShlurdCliMind,
  terminal : ShlurdCliTerminal
)
{
  private val cosmos = mind.getCosmos

  private val entityInterviewer = cosmos.uniqueEntity(
    cosmos.resolveQualifiedNoun(
      "wnf-interviewer-1", REF_SUBJECT, Set())).get

  private val entityShlurd = cosmos.uniqueEntity(
    cosmos.resolveQualifiedNoun(
      SmcIdeals.FORM_SOMEONE, REF_SUBJECT, Set("shlurd"))).get

  private val params = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)

  private val responder = new SpcResponder(
    mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS), params,
    communicationContext = SmcCommunicationContext(
      Some(entityInterviewer),
      Some(entityShlurd)
    )
  )

  def run()
  {
    mind.startConversation
    var exit = false
    terminal.emitControl("")
    while (!exit) {
      terminal.emitPrompt
      terminal.readCommand match {
        case Some(input) => {
          val parseResults = mind.newParser(input).parseAll
          parseResults.foreach(parseResult => {
            val output = responder.process(parseResult)
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
