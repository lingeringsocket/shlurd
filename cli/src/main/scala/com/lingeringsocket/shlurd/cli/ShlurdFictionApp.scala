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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.io._
import scala.collection._

import java.io._

import org.slf4j._

object ShlurdFictionAliases
{
  val map = Map(
    "d" -> "go down",
    "down" -> "go down",
    "e" -> "go east",
    "east" -> "go east",
    "g" -> "again",
    "i" -> "inventory",
    "inventory" -> "what am I holding",
    "l" -> "look",
    "look" -> "what do I see",
    // FIXME this can instead be "no" in context
    "n" -> "go north",
    "north" -> "go north",
    "ne" -> "go northeast",
    "northeast" -> "go northeast",
    "nw" -> "go northwest",
    "northwest" -> "go northwest",
    "o" -> "oops",
    "q" -> "quit",
    "s" -> "go south",
    "south" -> "go south",
    "se" -> "go southeast",
    "southeast" -> "go southeast",
    "sw" -> "go southwest",
    "southwest" -> "go southwest",
    "u" -> "go up",
    "up" -> "go up",
    "w" -> "go west",
    "west" -> "go west",
    "x" -> "examine",
    "y" -> "yes",
    "z" -> "wait"
  )
}

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

object ShlurdFictionShell
{
  val logger =
    LoggerFactory.getLogger(
      classOf[ShlurdFictionShell])

  val PLAYER_WORD = "player-character"

  val INTERPRETER_WORD = "game-interpreter"

  val OK = "OK."

  def loadOrCreate(file : File) : (ShlurdCliMind, Boolean) =
  {
    val terminal = new ShlurdFictionConsole
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
    val cosmos = ShlurdPrimordialWordnet.loadCosmos
    val beliefs = ResourceUtils.getResourceFile(
      "/example-fiction/game-beliefs.txt")
    val source = Source.fromFile(beliefs)
    val bootMind = new SpcWordnetMind(cosmos)
    bootMind.loadBeliefs(source)

    val entityPlayer = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        PLAYER_WORD, REF_SUBJECT, Set())).get
    val entityInterpreter = cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        INTERPRETER_WORD, REF_SUBJECT, Set())).get

    new ShlurdCliMind(cosmos, entityPlayer, entityInterpreter)
  }
}

class ShlurdFictionInterpreter(
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance,
  params : SmcResponseParams,
  executor : SmcExecutor[SpcEntity])
    extends SpcInterpreter(mind, beliefAcceptance, params, executor)
{
  import ShlurdFictionShell.logger

  override protected def spawn(subMind : SpcMind) =
  {
    new ShlurdFictionInterpreter(
      subMind, ACCEPT_MODIFIED_BELIEFS, params, executor)
  }

  override protected def checkCycle(
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    isPrecondition : Boolean) : Boolean =
  {
    if (logger.isTraceEnabled) {
      val printed = sentencePrinter.printPredicateStatement(
        predicate, SilTam.indicative)
      if (isPrecondition) {
        logger.trace(s"VERIFY $printed")
      } else {
        logger.trace(s"TRIGGER $printed")
      }
    }
    super.checkCycle(predicate, seen)
  }
}

class ShlurdFictionShell(
  mind : ShlurdCliMind,
  terminal : ShlurdFictionTerminal = new ShlurdFictionConsole)
{
  import ShlurdFictionShell._

  sealed trait Deferred {
  }

  case class DeferredTrigger(quotation : String) extends Deferred

  case class DeferredReport(quotation : String) extends Deferred

  case class DeferredComplaint(quotation : String) extends Deferred

  private val params = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  private val deferredQueue = new mutable.Queue[Deferred]

  private val executor = new SmcExecutor[SpcEntity]
  {
    override def executeAction(ap : SilActionPredicate) : Option[String] =
    {
      val lemma = ap.action.lemma
      ap.directObject match {
        case Some(SilQuotationReference(quotation)) => {
          ap.subject match {
            case SilNounReference(
              SilWordInflected(inflected), DETERMINER_UNIQUE, COUNT_SINGULAR
            ) => {
              val ok = Some(OK)
              inflected match {
                case PLAYER_WORD => {
                  if (lemma == "ask") {
                    defer(DeferredTrigger(quotation))
                    ok
                  } else {
                    None
                  }
                }
                case INTERPRETER_WORD => {
                  lemma match {
                    case "say" => {
                      defer(DeferredReport(quotation))
                      ok
                    }
                    case "complain" => {
                      defer(DeferredComplaint(quotation))
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

  private val sentencePrinter = new SilSentencePrinter

  private val beliefInterpreter = new ShlurdFictionInterpreter(
    mind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private val interpreter = new ShlurdFictionInterpreter(
    mind, ACCEPT_NO_BELIEFS, params, executor)

  private def interpretReentrantFiat(sentence : SilSentence) : String =
  {
    if (logger.isTraceEnabled) {
      val printed = sentencePrinter.print(sentence)
      logger.trace(s"FIAT $printed")
    }
    beliefInterpreter.interpret(sentence)
  }

  private def defer(deferred : Deferred)
  {
    deferredQueue += deferred
  }

  def init()
  {
    val source = Source.fromFile(
      ResourceUtils.getResourceFile("/example-fiction/game-init.txt"))
    val sentences = mind.newParser(
      source.getLines.filterNot(_.isEmpty).mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = beliefInterpreter.interpret(mind.analyzeSense(sentence))
      assert(output == OK, output)
    })
    terminal.emitControl("Initialization complete.")
  }

  private def processDeferred()
  {
    var first = true
    while (deferredQueue.nonEmpty) {
      deferredQueue.dequeue match {
        case DeferredTrigger(input) => {
          logger.trace(s"INTERPRET $input")
          val sentences = mind.newParser(preprocess(input)).parseAll
          sentences.foreach(sentence => {
            var output = interpreter.interpret(mind.analyzeSense(sentence))
            logger.trace(s"RESULT $output")
            terminal.emitNarrative("")
            if (first) {
              first = false
              if (output != OK) {
                val complaints = deferredQueue.flatMap(_ match {
                  case c : DeferredComplaint => Some(c)
                  case _ => None
                })
                deferredQueue.clear
                complaints.foreach(complaint => {
                  output = ""
                  terminal.emitNarrative(complaint.quotation)
                })
              }
            }
            if (output.nonEmpty) {
              terminal.emitNarrative(output)
            }
          })
        }
        case DeferredReport(report) => {
          terminal.emitNarrative("")
          terminal.emitNarrative(report)
        }
        case DeferredComplaint(complaint) => {
          terminal.emitNarrative("")
          terminal.emitNarrative("OOPS")
          terminal.emitNarrative("")
          terminal.emitNarrative(complaint)
        }
      }
    }
  }

  private def preprocess(input : String) : String =
  {
    ShlurdFictionAliases.map.get(input.trim.toLowerCase) match {
      case Some(replacement) => {
        preprocess(replacement)
      }
      case _ => {
        // special case for examine, which can take an object
        if (input.startsWith("x ")) {
          "examine "  + input.stripPrefix("x ")
        } else {
          input
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

trait ShlurdFictionTerminal
{
  import ShlurdFictionShell.logger

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
}

class ShlurdFictionConsole extends ShlurdFictionTerminal
{
  override def emitPrompt()
  {
    super.emitPrompt
    print("> ")
  }

  override def emitControl(msg : String)
  {
    super.emitControl(msg)
    println(s"[SIF] $msg")
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
