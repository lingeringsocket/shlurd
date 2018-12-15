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
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.io._

import java.io._

object ShlurdFictionApp extends App
{
  val serializer = new ShlurdCliSerializer
  val file = new File("run/shlurd-fiction.zip")
  val (mind, init) = loadOrCreate(file)
  val app = new ShlurdFictionApp(mind, file, serializer)
  if (init) {
    app.init
  }
  app.run

  private def loadOrCreate(file : File) : (ShlurdCliMind, Boolean) =
  {
    if (file.exists) {
      println("[SIF] Reloading...")
      val oldMind = serializer.load(file)
      println("[SIF] Reload complete.")
      tupleN((oldMind, false))
    } else {
      println("[SIF] Initializing...")
      val cosmos = new SpcCosmos
      SpcPrimordial.initCosmos(cosmos)
      val beliefs = SprParser.getResourceFile("/ontologies/fiction-beliefs.txt")
      val source = Source.fromFile(beliefs)
      cosmos.loadBeliefs(source)

      val entityPlayer = cosmos.uniqueEntity(
        cosmos.resolveQualifiedNoun(
          "player", REF_SUBJECT, Set())).get
      val entityInterpreter = cosmos.uniqueEntity(
        cosmos.resolveQualifiedNoun(
          "interpreter", REF_SUBJECT, Set())).get

      val newMind = new ShlurdCliMind(cosmos, entityPlayer, entityInterpreter)
      tupleN((newMind, true))
    }
  }
}

class ShlurdFictionApp(
  mind : ShlurdCliMind,
  file : File,
  serializer : ShlurdCliSerializer)
{
  private val params = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  private val executor = new SmcExecutor[SpcEntity]
  {
    override def executeImperative(predicate : SilPredicate) : Boolean =
    {
      def playerRef = SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)
      val newPredicate = predicate match {
        case ap : SilActionPredicate => ap.copy(subject = playerRef)
        case _ => predicate
      }
      val sentence = SilPredicateSentence(newPredicate)
      interpretReentrant(sentence)
    }

    override def executeInvocation(
      invocation : SmcStateChangeInvocation[SpcEntity])
    {
      val sentence = SilPredicateSentence(
        SilStatePredicate(
          mind.getCosmos.specificReferences(invocation.entities),
          SilPropertyState(invocation.state)
        )
      )
      interpretReentrant(sentence)
    }
  }

  private val interpreter = new SpcInterpreter(
    mind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private def interpretReentrant(sentence : SilSentence) : Boolean =
  {
    val result = interpreter.interpret(sentence)
    result == interpreter.sentencePrinter.sb.respondCompliance
  }

  private def init()
  {
    val source = Source.fromFile(
      SprParser.getResourceFile("/ontologies/fiction-init.txt"))
    val sentences = mind.newParser(source.getLines.mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = interpreter.interpret(sentence)
      assert(output == "OK.", output)
    })
    println("[SIF] Initialization complete.")
  }

  private def run()
  {
    mind.startConversation
    var exit = false
    println
    while (!exit) {
      print("> ")
      val input = StdIn.readLine
      if (input == null) {
        exit = true
      } else {
        val sentences = mind.newParser(input).parseAll
        sentences.foreach(sentence => {
          val output = interpreter.interpret(sentence)
          println
          println(output)
          println
        })
      }
    }
    println
    println("Saving...NOT!")
    // don't serialize conversation since that could be an extra source of
    // deserialization problems later
    mind.stopConversation
    // serializer.save(mind, file)
  }
}
