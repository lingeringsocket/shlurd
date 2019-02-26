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
import scala.collection._

import java.io._

object ShlurdFictionApp extends App
{
  val file = new File("run/shlurd-fiction.zip")
  val (mind, init) = ShlurdFictionShell.loadOrCreate(file)
  val shell = new ShlurdFictionShell(mind)
  if (init) {
    shell.init
  }
  shell.run
}

class ShlurdFictionTerminal
{
  def emitPrompt()
  {
    print("> ")
  }

  def emitControl(msg : String)
  {
    println(s"[SIF] $msg")
  }

  def emitNarrative(msg : String)
  {
    println(msg)
  }

  def readCommand() : Option[String] =
  {
    Option(StdIn.readLine)
  }
}

object ShlurdFictionShell
{
  val PLAYER_WORD = "player"

  val INTERPRETER_WORD = "interpreter"

  val OK = "OK."

  def loadOrCreate(file : File) : (ShlurdCliMind, Boolean) =
  {
    val terminal = new ShlurdFictionTerminal
    if (file.exists) {
      terminal.emitControl("Reloading...")
      val serializer = new ShlurdCliSerializer
      val oldMind = serializer.loadMind(file)
      terminal.emitControl("Reload complete.")
      tupleN((oldMind, false))
    } else {
      terminal.emitControl("Initializing...")
      tupleN((newMind, true))
    }
  }

  def newMind() : ShlurdCliMind =
  {
    val cosmos = new SpcCosmos
    SpcPrimordial.initCosmos(cosmos)
    val beliefs = SprParser.getResourceFile("/ontologies/fiction-beliefs.txt")
    val source = Source.fromFile(beliefs)
    cosmos.loadBeliefs(source)

    val entityPlayer = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        PLAYER_WORD, REF_SUBJECT, Set())).get
    val entityInterpreter = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        INTERPRETER_WORD, REF_SUBJECT, Set())).get

    new ShlurdCliMind(cosmos, entityPlayer, entityInterpreter, false)
  }
}

class ShlurdFictionShell(
  mind : ShlurdCliMind,
  terminal : ShlurdFictionTerminal = new ShlurdFictionTerminal)
{
  import ShlurdFictionShell._

  sealed trait Deferred {
  }

  case class DeferredTrigger(quotation : String) extends Deferred

  case class DeferredReport(quotation : String) extends Deferred

  private val params = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  private val deferredQueue = new mutable.Queue[Deferred]

  private val executor = new SmcExecutor[SpcEntity]
  {
    override def executeAction(ap : SilActionPredicate) : Option[String] =
    {
      // FIXME make sure that verb is ask/say/etc
      ap.directObject match {
        case Some(SilQuotationReference(quotation)) => {
          ap.subject match {
            case SilNounReference(
              SilWordInflected(inflected), DETERMINER_UNIQUE, COUNT_SINGULAR
            ) => {
              val ok = Some(OK)
              inflected match {
                case PLAYER_WORD => {
                  defer(DeferredTrigger(quotation))
                  ok
                }
                case INTERPRETER_WORD => {
                  defer(DeferredReport(quotation))
                  ok
                }
                case _ => None
              }
            }
            case _ => None
          }
        }
        case _ => None
      }
    }

    override def executeImperative(predicate : SilPredicate) : Option[String] =
    {
      def playerRef =
        SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)
      val newPredicate = predicate match {
        case ap : SilActionPredicate => ap.copy(subject = playerRef)
        case _ => predicate
      }
      val sentence = SilPredicateSentence(newPredicate)
      Some(interpretReentrantFiat(sentence))
    }

    override def executeInvocation(
      invocation : SmcStateChangeInvocation[SpcEntity])
    {
      val sentence = SilPredicateSentence(
        SilStatePredicate(
          mind.specificReferences(invocation.entities),
          SilPropertyState(invocation.state)
        )
      )
      interpretReentrantFiat(sentence)
    }
  }

  private val interpreter = new SpcInterpreter(
    mind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private def interpretReentrantFiat(sentence : SilSentence) : String =
  {
    interpreter.interpret(sentence)
  }

  private def interpretReentrantQuery(query : String) : String =
  {
    val sentence = interpreter.newParser(query).parseOne
    interpreter.interpret(sentence, query)
  }

  private def defer(deferred : Deferred)
  {
    deferredQueue += deferred
  }

  def init()
  {
    val source = Source.fromFile(
      SprParser.getResourceFile("/ontologies/fiction-init.txt"))
    val sentences = mind.newParser(
      source.getLines.filterNot(_.isEmpty).mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = interpreter.interpret(sentence)
      assert(output == OK, output)
    })
    terminal.emitControl("Initialization complete.")
  }

  private def processDeferred()
  {
    var first = true
    while (deferredQueue.nonEmpty) {
      deferredQueue.dequeue match {
        case DeferredTrigger(quotation) => {
          val sentences = mind.newParser(quotation).parseAll
          sentences.foreach(sentence => {
            val output = interpreter.interpret(sentence)
            terminal.emitNarrative("")
            terminal.emitNarrative(output)
            if (first) {
              first = false
              if (output != OK) {
                deferredQueue.clear
              }
            }
          })
        }
        case DeferredReport(quotation) => {
          terminal.emitNarrative("")
          terminal.emitNarrative(quotation)
        }
      }
    }
  }

  def run()
  {
    mind.startConversation
    var exit = false
    terminal.emitNarrative("")
    while (!exit) {
      terminal.emitPrompt
      terminal.readCommand match {
        case Some(input) => {
          defer(DeferredTrigger(input))
          processDeferred
          terminal.emitNarrative("")
        }
        case _ => {
          exit = true
        }
      }
    }
    terminal.emitNarrative("")
    terminal.emitControl("Saving...NOT!")
    // don't serialize conversation since that could be an extra source of
    // deserialization problems later
    mind.stopConversation
  }
}
