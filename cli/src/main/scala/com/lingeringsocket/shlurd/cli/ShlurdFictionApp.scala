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
  val file = new File("run/fiction-save.zip")
  val (snapshot, init) =
    ShlurdFictionShell.loadOrCreate(file)
  val shell = new ShlurdFictionShell(snapshot)
  if (init) {
    shell.init
  }
  ShlurdFictionShell.run(shell)
}

class ShlurdFictionMind(
  cosmos : SpcCosmos,
  entityFirst : SpcEntity,
  entitySecond : SpcEntity
) extends ShlurdCliMind(cosmos, entityFirst, entitySecond)
{
  private var timestamp = SpcTimestamp.ZERO

  def getTimestamp() = timestamp

  def startNewTurn()
  {
    timestamp = timestamp.successor
  }

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new ShlurdFictionMind(
      newCosmos, entityFirst, entitySecond)
    mind.initFrom(this)
    mind
  }

  override def equivalentReferences(
    entity : SpcEntity,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    val references = super.equivalentReferences(entity, determiner)
    if (entity.form.name == ShlurdFictionShell.INVENTORY_WORD) {
      val (nouns, others) =
        references.partition(_.isInstanceOf[SilNounReference])
      // prefer "the player's stuff" over "the player-inventory"
      others ++ nouns
    } else {
      references
    }
  }

  override protected def getFormName(form : SpcForm) : String =
  {
    synonymize(form, super.getFormName(form))
  }

  override protected def getPossesseeName(role : SpcRole) : String =
  {
    synonymize(role, super.getPossesseeName(role))
  }

  private def synonymize(ideal : SpcIdeal, name : String) : String =
  {
    def isHyphenized(s : String) = s.contains('-')
    if (isHyphenized(name)) {
      val synonyms = cosmos.getSynonymsForIdeal(ideal)
      synonyms.map(_.name).filterNot(isHyphenized).headOption.getOrElse(name)
    } else {
      name
    }
  }
}

object ShlurdFictionShell
{
  val logger =
    LoggerFactory.getLogger(
      classOf[ShlurdFictionShell])

  val PLAYER_WORD = "player-character"

  val INTERPRETER_WORD = "game-interpreter"

  val INVENTORY_WORD = "player-inventory"

  val OK = "OK."

  def run(
    firstShell : ShlurdFictionShell)
  {
    var shellOpt : Option[ShlurdFictionShell] = Some(firstShell)
    while (shellOpt.nonEmpty) {
      val shell = shellOpt.get
      shellOpt = shell.run
    }
  }

  def loadOrCreate(file : File)
      : (ShlurdFictionSnapshot, Boolean) =
  {
    val terminal = new ShlurdFictionConsole
    if (file.exists) {
      tupleN((restore(file, terminal), false))
    } else {
      terminal.emitControl("Initializing...")
      val (newPhenomenalMind, newNoumenalMind) = createNewCosmos
      tupleN((
        ShlurdFictionSnapshot(
          newPhenomenalMind,
          newNoumenalMind
        ),
        true))
    }
  }

  def restore(file : File, terminal : ShlurdFictionTerminal)
      : ShlurdFictionSnapshot =
  {
    terminal.emitControl(s"Restoring from $file...")
    val serializer = new ShlurdCliSerializer
    val snapshot = serializer.loadSnapshot(file)
    terminal.emitControl("Restore complete.")
    snapshot
  }

  def createNewCosmos() : (ShlurdFictionMind, ShlurdFictionMind) =
  {
    val noumenalCosmos = ShlurdPrimordialWordnet.loadCosmos
    val beliefs = ResourceUtils.getResourceFile(
      "/example-fiction/game-beliefs.txt")
    val source = Source.fromFile(beliefs)
    val bootMind = new SpcWordnetMind(noumenalCosmos)
    bootMind.loadBeliefs(source)

    val entityPlayer = noumenalCosmos.uniqueEntity(
      noumenalCosmos.resolveQualifiedNoun(
        PLAYER_WORD, REF_SUBJECT, Set())).get
    val entityInterpreter = noumenalCosmos.uniqueEntity(
      noumenalCosmos.resolveQualifiedNoun(
        INTERPRETER_WORD, REF_SUBJECT, Set())).get

    val noumenalMind = new ShlurdFictionMind(
      noumenalCosmos, entityPlayer, entityInterpreter)

    val phenomenalCosmos = new SpcCosmos
    phenomenalCosmos.copyFrom(noumenalCosmos)
    val phenomenalMind = new ShlurdFictionMind(
      phenomenalCosmos, entityPlayer, entityInterpreter)

    tupleN((phenomenalMind, noumenalMind))
  }
}

class ShlurdFictionResponder(
  shell : ShlurdFictionShell,
  propagateBeliefs : Boolean,
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance,
  params : SmcResponseParams,
  executor : SmcExecutor[SpcEntity])
    extends SpcResponder(mind, beliefAcceptance, params, executor)
{
  import ShlurdFictionShell.logger

  override protected def publishBelief(belief : SpcBelief)
  {
    val printed = sentencePrinter.print(belief.sentence)
    logger.trace(s"BELIEF $printed")
    if (propagateBeliefs) {
      shell.deferPhenomenon(printed)
    }
  }

  override protected def spawn(subMind : SpcMind) =
  {
    new ShlurdFictionResponder(
      shell, propagateBeliefs, subMind,
      ACCEPT_MODIFIED_BELIEFS, params, executor)
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
    if (!isPrecondition) {
      predicate match {
        case ap : SilActionPredicate => {
          val lemma = ap.action.toLemma
          ap.subject match {
            case SilNounReference(
              SilWordInflected(inflected), DETERMINER_UNIQUE, COUNT_SINGULAR
            ) => {
              inflected match {
                case ShlurdFictionShell.PLAYER_WORD => {
                  lemma match {
                    case "perceive" => {
                      ap.directObject.foreach(ref => {
                        val resultCollector = SmcResultCollector[SpcEntity]()
                        val result = resolveReferences(
                          SilStatePredicate(ref, SilExistenceState()),
                          resultCollector,
                          true).get
                        assert(result.isTrue)
                        shell.deferPerception(
                          resultCollector.referenceMap(ref))
                      })
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
    super.checkCycle(predicate, seen, isPrecondition)
  }
}

object ShlurdFictionSnapshot
{
  def apply(
    phenomenalMind : ShlurdFictionMind,
    noumenalMind : ShlurdFictionMind) : ShlurdFictionSnapshot =
  {
    ShlurdFictionSnapshot(
      phenomenalMind,
      noumenalMind,
      new SpcPerception(
        noumenalMind.getCosmos,
        phenomenalMind.getCosmos
      )
    )
  }
}

case class ShlurdFictionSnapshot(
  phenomenalMind : ShlurdFictionMind,
  noumenalMind : ShlurdFictionMind,
  perception : SpcPerception
)

class ShlurdFictionShell(
  snapshot : ShlurdFictionSnapshot,
  terminal : ShlurdFictionTerminal = new ShlurdFictionConsole)
{
  import ShlurdFictionShell._

  sealed trait Deferred {
  }

  case class DeferredCommand(quotation : String) extends Deferred

  case class DeferredDirective(quotation : String) extends Deferred

  case class DeferredReport(quotation : String) extends Deferred

  case class DeferredComplaint(quotation : String) extends Deferred

  case class DeferredPerception(entities : Set[SpcEntity]) extends Deferred

  case class DeferredPhenomenon(belief : String) extends Deferred

  private val phenomenalMind = snapshot.phenomenalMind

  private val noumenalMind = snapshot.noumenalMind

  private val perception = snapshot.perception

  private val params = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  private val deferredQueue = new mutable.Queue[Deferred]

  private var gameTurnTimestamp = SpcTimestamp.ZERO

  private var restoreFile : Option[File] = None

  private val executor = new SmcExecutor[SpcEntity]
  {
    override def executeAction(ap : SilActionPredicate) : Option[String] =
    {
      val lemma = ap.action.toLemma
      ap.directObject match {
        case Some(SilQuotationReference(quotation)) => {
          ap.subject match {
            case SilNounReference(
              SilWordInflected(inflected), DETERMINER_UNIQUE, COUNT_SINGULAR
            ) => {
              val ok = Some(OK)
              inflected match {
                case PLAYER_WORD => {
                  lemma match {
                    case "ask" => {
                      defer(DeferredCommand(quotation))
                      ok
                    }
                    case _ => None
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
                    case "save" => {
                      val file = getSaveFile(quotation)
                      terminal.emitControl(s"Saving $file...")
                      val serializer = new ShlurdCliSerializer
                      phenomenalMind.stopConversation
                      serializer.saveSnapshot(
                        ShlurdFictionSnapshot(
                          phenomenalMind,
                          noumenalMind,
                          perception),
                        file)
                      phenomenalMind.startConversation
                      ok
                    }
                    case "restore" => {
                      restoreFile = Some(getSaveFile(quotation))
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

    private def getSaveFile(quotation : String) : File =
    {
      val fileName = {
        if (quotation == ".zip") {
          terminal.getDefaultSaveFile
        } else {
          quotation
        }
      }
      val sanitized = fileName.replaceAll("[:\\\\/*\"?|<>']", "_")
      new File(s"run/$sanitized")
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
      Some(processFiat(sentence))
    }

    override def executeInvocation(
      invocation : SmcStateChangeInvocation[SpcEntity])
    {
      val sentence = SilPredicateSentence(
        SilStatePredicate(
          noumenalMind.specificReferences(invocation.entities),
          SilPropertyState(invocation.state)
        )
      )
      processFiat(sentence)
    }
  }

  private val sentencePrinter = new SilSentencePrinter

  private val noumenalInitializer = new ShlurdFictionResponder(
    this, false, noumenalMind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private val noumenalUpdater = new ShlurdFictionResponder(
    this, true, noumenalMind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private val phenomenalResponder = new ShlurdFictionResponder(
    this, false, phenomenalMind, ACCEPT_NO_BELIEFS,
    params.copy(existenceAssumption = EXISTENCE_ASSUME_UNKNOWN), executor)

  private val phenomenalUpdater = new ShlurdFictionResponder(
    this, false, phenomenalMind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private def processFiat(sentence : SilSentence) : String =
  {
    if (logger.isTraceEnabled) {
      val printed = sentencePrinter.print(sentence)
      logger.trace(s"FIAT $printed")
    }
    noumenalUpdater.process(sentence)
  }

  def defer(deferred : Deferred)
  {
    deferredQueue += deferred
  }

  def deferPhenomenon(belief : String)
  {
    defer(DeferredPhenomenon(belief))
  }

  def deferPerception(entities : Set[SpcEntity])
  {
    defer(DeferredPerception(entities))
  }

  def init()
  {
    initMind(
      noumenalMind,
      noumenalInitializer,
      "/example-fiction/game-init.txt")
    initMind(
      phenomenalMind,
      phenomenalUpdater,
      "/example-fiction/player-init.txt")
    terminal.emitControl("Initialization complete.")
  }

  private def initMind(
    mind : ShlurdCliMind,
    responder : ShlurdFictionResponder,
    resourceName : String)
  {
    val source = Source.fromFile(
      ResourceUtils.getResourceFile(resourceName))
    val sentences = mind.newParser(
      source.getLines.filterNot(_.isEmpty).mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = responder.process(mind.analyzeSense(sentence))
      assert(output == OK, output)
      processDeferred
    })
  }

  private def processDeferred()
  {
    var first = true
    while (deferredQueue.nonEmpty) {
      deferredQueue.dequeue match {
        case DeferredDirective(input) => {
          logger.trace(s"DIRECTIVE $input")
          val sentences = noumenalMind.newParser(input).parseAll
          sentences.foreach(sentence => {
            val output = noumenalUpdater.process(
              noumenalMind.analyzeSense(sentence))
            assert(output == OK)
          })
        }
        case DeferredCommand(input) => {
          logger.trace(s"INTERPRET $input")
          val expanded = preprocess(input)
          if (expanded != input) {
            logger.trace(s"EXPANDED $expanded")
          }
          val sentences = phenomenalMind.newParser(expanded).parseAll
          sentences.foreach(sentence => {
            var output = phenomenalResponder.process(
              phenomenalMind.analyzeSense(sentence))
            logger.trace(s"RESULT $output")
            terminal.emitNarrative("")
            var assumption = ""
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
              if (sentence.tam.isInterrogative) {
                val entities =
                  phenomenalMind.getConversation.getUtterances.
                    takeRight(2).flatMap(
                      _.referenceMap.values.flatten)
                if (entities.exists(entity => {
                  val timestamp = perception.getEntityTimestamp(entity)
                  timestamp.map(_.isBefore(gameTurnTimestamp)).getOrElse(false)
                })) {
                  assumption = "(At least I assume that's still the case.)"
                }
              }
            }
            if (output.nonEmpty) {
              terminal.emitNarrative(output)
            }
            if (assumption.nonEmpty) {
              terminal.emitNarrative("")
              terminal.emitNarrative(assumption)
            }
          })
        }
        case DeferredPhenomenon(belief) => {
          logger.trace(s"PHENOMENON $belief")
          val sentences = phenomenalMind.newParser(preprocess(belief)).parseAll
          sentences.foreach(sentence => {
            var output = phenomenalUpdater.process(
              phenomenalMind.analyzeSense(sentence))
            assert(output == OK, output)
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
        case DeferredPerception(entities) => {
          logger.trace(s"PERCEIVE $entities")
          val timestamp = noumenalMind.getTimestamp
          entities.toSeq.sortBy(_.name).foreach(entity => {
            perception.perceiveEntityAssociations(
              entity, timestamp)
            perception.perceiveEntityProperties(
              entity, timestamp)
          })
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

  def run() : Option[ShlurdFictionShell] =
  {
    phenomenalMind.startConversation
    var exit = false
    terminal.emitNarrative("")
    while (!exit) {
      noumenalMind.startNewTurn
      gameTurnTimestamp = noumenalMind.getTimestamp
      terminal.emitPrompt
      terminal.readCommand match {
        case Some(input) => {
          defer(DeferredDirective("the game-turn advances"))
          processDeferred
          defer(DeferredCommand(input))
          processDeferred
          terminal.emitNarrative("")
        }
        case _ => {
          exit = true
        }
      }
      if (restoreFile.nonEmpty) {
        exit = true
      }
    }
    restoreFile match {
      case Some(file) => {
        val snapshot = ShlurdFictionShell.restore(file, terminal)
        Some(new ShlurdFictionShell(snapshot, terminal))
      }
      case _ => {
        terminal.emitNarrative("")
        terminal.emitControl("Shutting down...")
        None
      }
    }
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

  def getDefaultSaveFile() : String =
  {
    "fiction-save.zip"
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
