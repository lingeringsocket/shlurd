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
package com.lingeringsocket.snavig

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.cli._

import scala.io._
import scala.collection._
import scala.util._

import java.io._

import org.slf4j._

object SnavigShell
{
  val logger =
    LoggerFactory.getLogger(
      classOf[SnavigShell])

  val PLAYER_WORD = "player-character"

  val INTERPRETER_WORD = "game-interpreter"

  val INVENTORY_WORD = "player-inventory"

  val OK = "OK."

  private val actionRespond = SilWord("responds", "respond")

  def run(terminal : SnavigTerminal = new SnavigConsole)
  {
    val newShell = this.synchronized {
      val file = new File("run/snavig-init-save.zip")
      val (snapshot, init) =
        SnavigShell.loadOrCreate(file, terminal)
      val shell = new SnavigShell(snapshot, terminal)
      if (init) {
        shell.init
        val serializer = new SnavigSerializer
        serializer.saveSnapshot(snapshot, file)
      }
      shell
    }
    SnavigShell.run(newShell)
  }

  def run(
    firstShell : SnavigShell)
  {
    var shellOpt : Option[SnavigShell] = Some(firstShell)
    while (shellOpt.nonEmpty) {
      val shell = shellOpt.get
      shellOpt = shell.run
    }
  }

  def loadOrCreate(file : File, terminal : SnavigTerminal)
      : (SnavigSnapshot, Boolean) =
  {
    if (file.exists) {
      tupleN((restore(file, terminal), false))
    } else {
      terminal.emitControl("Initializing...")
      val snapshot = createNewCosmos
      tupleN((
        snapshot,
        true))
    }
  }

  def restore(file : File, terminal : SnavigTerminal)
      : SnavigSnapshot =
  {
    terminal.emitControl(s"Restoring from $file...")
    val serializer = new SnavigSerializer
    val snapshot = serializer.loadSnapshot(file)
    terminal.emitControl("Restore complete.")
    snapshot
  }

  def createNewCosmos() : SnavigSnapshot =
  {
    val noumenalCosmos = ShlurdPrimordialWordnet.loadCosmos
    val beliefs = ResourceUtils.getResourceFile(
      "/example-snavig/game-beliefs.txt")
    val source = Source.fromFile(beliefs)
    val bootMind = new SpcWordnetMind(noumenalCosmos)
    bootMind.loadBeliefs(source)

    val entityPlayer = noumenalCosmos.uniqueEntity(
      noumenalCosmos.resolveQualifiedNoun(
        PLAYER_WORD, REF_SUBJECT, Set())).get
    val entityInterpreter = noumenalCosmos.uniqueEntity(
      noumenalCosmos.resolveQualifiedNoun(
        INTERPRETER_WORD, REF_SUBJECT, Set())).get

    val noumenalMind = new SnavigMind(
      noumenalCosmos, entityPlayer, entityInterpreter, None)

    val phenomenalCosmos = new SpcCosmos
    phenomenalCosmos.copyFrom(noumenalCosmos)
    val perception = new SpcPerception(noumenalCosmos, phenomenalCosmos)
    val phenomenalMind = new SnavigMind(
      phenomenalCosmos, entityPlayer, entityInterpreter, Some(perception))

    val mindMap = new mutable.LinkedHashMap[String, SnavigMind]
    mindMap.put(SnavigSnapshot.PLAYER_PHENOMENAL, phenomenalMind)
    mindMap.put(entityPlayer.name, phenomenalMind)
    mindMap.put(SnavigSnapshot.NOUMENAL, noumenalMind)

    SnavigSnapshot(mindMap)
  }

  def singletonLookup(
    referenceMap : Map[SilReference, Set[SpcEntity]],
    ref : SilReference) : Option[SpcEntity] =
  {
    referenceMap.get(ref) match {
      case Some(set) => {
        if (set.size == 1) {
          Some(set.head)
        } else {
          None
        }
      }
      case _ => None
    }
  }
}


class SnavigShell(
  snapshot : SnavigSnapshot,
  terminal : SnavigTerminal = new SnavigConsole)
{
  import SnavigShell._

  sealed trait Deferred {
  }

  case class DeferredCommand(quotation : String) extends Deferred

  case class DeferredDirective(quotation : String) extends Deferred

  case class DeferredReport(quotation : String) extends Deferred

  case class DeferredComplaint(quotation : String) extends Deferred

  case class DeferredCommunication(
    speaker : SpcEntity,
    listener : SpcEntity,
    quotation : String) extends Deferred

  case class DeferredUtterance(
    listenerMind : SnavigMind,
    quotation : String) extends Deferred

  case class DeferredPerception(
    perceiver : SpcEntity,
    perceived : Set[SpcEntity]) extends Deferred

  case class DeferredPhenomenon(belief : String) extends Deferred

  private val phenomenalMind = snapshot.getPhenomenalMind

  private val noumenalMind = snapshot.getNoumenalMind

  private val playerEntity = phenomenalMind.entityFirst

  private val interpreterEntity = phenomenalMind.entitySecond

  private val playerPerception = phenomenalMind.perception.get

  private val params = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  private val deferredQueue = new mutable.Queue[Deferred]

  private var gameTurnTimestamp = SpcTimestamp.ZERO

  private var restoreFile : Option[File] = None

  private var listenerMind : Option[SnavigMind] = None

  private val executor = new SmcExecutor[SpcEntity]
  {
    override def executeAction(
      ap : SilActionPredicate,
      referenceMap : Map[SilReference, Set[SpcEntity]]) : Option[String] =
    {
      val lemma = ap.action.toLemma
      val subjectEntity = singletonLookup(referenceMap, ap.subject)
      val targetRefOpt = ap.modifiers.flatMap(_ match {
        case SilAdpositionalVerbModifier(
          SilAdposition.TO,
          ref) => Some(ref)
        case _ => None
      }).headOption
      val targetEntityOpt = targetRefOpt.flatMap(
        ref => singletonLookup(referenceMap, ref))
      val ok = Some(OK)
      ap.directObject match {
        case Some(SilQuotationReference(quotation)) => {
          if (subjectEntity == Some(playerEntity)) {
            lemma match {
              case "ask" => {
                targetEntityOpt match {
                  case Some(targetEntity) => {
                    if (targetEntity == interpreterEntity) {
                      defer(DeferredCommand(quotation))
                    } else {
                      defer(DeferredCommunication(
                        playerEntity,
                        targetEntity,
                        quotation))
                    }
                    ok
                  }
                  case _ => {
                    defer(DeferredCommand(quotation))
                    ok
                  }
                }
              }
              case "say" | "tell" => {
                targetEntityOpt match {
                  case Some(targetEntity) => {
                    defer(DeferredCommunication(
                      playerEntity,
                      targetEntity,
                      quotation))
                    ok
                  }
                  case _ => {
                    None
                  }
                }
              }
              case _ => None
            }
          } else if (subjectEntity == Some(interpreterEntity)) {
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
                val serializer = new SnavigSerializer
                phenomenalMind.stopConversation
                serializer.saveSnapshot(
                  snapshot,
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
          } else if (subjectEntity.nonEmpty) {
            lemma match {
              case "believe" => {
                subjectEntity match {
                  case Some(entity) => {
                    // FIXME if quotation does not start with slash,
                    // then interpret inline
                    importEntityBeliefs(
                      entity,
                      quotation)
                    ok
                  }
                  case _ => None
                }
              }
              case _ => None
            }
          } else {
            None
          }
        }
        case _ => {
          lemma match {
            case "talk" => {
              targetEntityOpt match {
                case Some(targetEntity) => {
                  if (targetEntity == playerEntity) {
                    talkToSelf
                  } else {
                    accessEntityMind(targetEntity) match {
                      case Some(entityMind) => {
                        listenerMind = Some(entityMind)
                        defer(DeferredReport(
                          "(You are now in conversation.  Say TTYL to stop.)"))
                      }
                      case _ => {
                        defer(DeferredReport(
                          SprUtils.capitalize(
                            noResponse(targetRefOpt.get))))
                      }
                    }
                  }
                  ok
                }
                case _ => {
                  None
                }
              }
            }
            case _ => None
          }
        }
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

  private val noumenalInitializer = new SnavigResponder(
    this, false, noumenalMind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private val noumenalUpdater = new SnavigResponder(
    this, true, noumenalMind, ACCEPT_MODIFIED_BELIEFS, params, executor)

  private val phenomenalResponder = new SnavigResponder(
    this, false, phenomenalMind, ACCEPT_NO_BELIEFS,
    params.copy(existenceAssumption = EXISTENCE_ASSUME_UNKNOWN), executor)

  private val phenomenalUpdater = new SnavigResponder(
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

  def deferPerception(
    perceiver : SpcEntity, perceived : Set[SpcEntity])
  {
    defer(DeferredPerception(perceiver, perceived))
  }

  def init()
  {
    initMind(
      noumenalMind,
      noumenalInitializer,
      "/example-snavig/game-init.txt")
    initMind(
      phenomenalMind,
      phenomenalUpdater,
      "/example-snavig/player-init.txt")
    terminal.emitControl("Initialization complete.")
  }

  private def initMind(
    mind : ShlurdCliMind,
    responder : SnavigResponder,
    resourceName : String)
  {
    val source = Source.fromFile(
      ResourceUtils.getResourceFile(resourceName))
    val sentences = mind.newParser(
      source.getLines.filterNot(_.isEmpty).mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = responder.process(mind.analyzeSense(sentence))
      assert(output == OK, tupleN((sentence, output)))
      processDeferred
    })
  }

  private def accessEntityMind(entity : SpcEntity) : Option[SnavigMind] =
  {
    if (snapshot.mindMap.contains(entity.name)) {
      snapshot.mindMap.get(entity.name)
    } else {
      val noumenalCosmos = noumenalMind.getCosmos
      lazy val newCosmos = {
        val cosmos = new SpcCosmos
        cosmos.copyFrom(noumenalCosmos)
        cosmos
      }
      val (cosmos, perception) = noumenalCosmos.evaluateEntityProperty(
        entity, "awareness"
      ) match {
        case Failure(ex) => {
          throw ex
        }
        case Success((_, Some(awareness))) => {
          awareness match {
            case "mindless" => {
              return None
            }
            case "unperceptive" => {
              tupleN((newCosmos, None))
            }
            case "perceptive" => {
              val cosmos = newCosmos
              tupleN((
                cosmos, Some(new SpcPerception(noumenalCosmos, cosmos))))
            }
            case "omniscient" => {
              tupleN((noumenalCosmos, None))
            }
            case _ => {
              return None
            }
          }
        }
        case unexpected => {
          return None
        }
      }
      val newMind = new SnavigMind(
        cosmos, playerEntity, entity, perception)
      snapshot.mindMap.put(entity.name, newMind)
      Some(newMind)
    }
  }

  private def importEntityBeliefs(
    entity : SpcEntity,
    resourceName : String)
  {
    accessEntityMind(entity) match {
      case Some(mind) => {
        val responder = new SnavigResponder(
          this, false, mind, ACCEPT_MODIFIED_BELIEFS,
          SmcResponseParams(), executor)
        initMind(mind, responder, resourceName)
      }
      case _ => {
        terminal.emitControl(
          s"Beliefs ignored for mindless entity $entity.")
      }
    }
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
        case DeferredUtterance(_, "TTYL" | "ttyl") => {
          listenerMind = None
          defer(DeferredReport(
            "(You are no longer in conversation.)"))
        }
        case DeferredUtterance(targetMind, input) => {
          logger.trace(s"UTTERANCE $input")
          val responder = new SnavigResponder(
            this, false, targetMind, ACCEPT_NO_BELIEFS,
            SmcResponseParams(), executor)
          val sentences = targetMind.newParser(input).parseAll
          sentences.foreach(sentence => {
            val output = processUtterance(
              responder,
              targetMind.analyzeSense(sentence))
            logger.trace(s"RESULT $output")
            terminal.emitNarrative("")
            // FIXME allow side effects of conversation
            assert(deferredQueue.isEmpty)
            terminal.emitNarrative(output)
          })
        }
        case DeferredCommand(input) => {
          logger.trace(s"COMMAND $input")
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
                  val timestamp = playerPerception.getEntityTimestamp(entity)
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
        case DeferredCommunication(speaker, listener, quotation) => {
          // FIXME handle arbitrary speaker/listener combinations,
          // parameters, etc
          if (speaker == listener) {
            talkToSelf
          } else {
            val listenerReference = phenomenalMind.specificReference(
              listener, DETERMINER_UNIQUE)
            val reply = accessEntityMind(listener) match {
              case Some(entityMind) => {
                val entityResponder = new SnavigResponder(
                  this, false, entityMind, ACCEPT_NO_BELIEFS,
                  SmcResponseParams(), executor)
                // FIXME use parseAll instead
                val sentence = entityMind.newParser(quotation).parseOne
                val response = processUtterance(
                  entityResponder,
                  entityMind.analyzeSense(sentence))
                val responseSentence = SilPredicateSentence(
                  SilActionPredicate(
                    listenerReference,
                    actionRespond,
                    Some(SilQuotationReference(response))
                  )
                )
                sentencePrinter.printUnterminated(responseSentence)
              }
              case _ => {
                noResponse(listenerReference)
              }
            }
            terminal.emitNarrative("")
            terminal.emitNarrative(SprUtils.capitalize(reply))
          }
        }
        case DeferredPerception(perceiver, perceived) => {
          logger.trace(s"PERCEIVE $perceiver $perceived")
          val entityMind = snapshot.mindMap.get(perceiver.name)
          entityMind.flatMap(_.perception).foreach(perception => {
            val timestamp = noumenalMind.getTimestamp
            perceived.toSeq.sortBy(_.name).foreach(entity => {
              perception.perceiveEntityAssociations(
                entity, timestamp)
              perception.perceiveEntityProperties(
                entity, timestamp)
            })
          })
        }
      }
    }
  }

  private def talkToSelf()
  {
    defer(DeferredReport(
      "Only crazy people talk to themselves."))
  }

  private def noResponse(listenerReference : SilReference) : String =
  {
    val responseSentence = SilPredicateSentence(
      SilActionPredicate(
        listenerReference,
        actionRespond
      ),
      SilTam.indicative.negative
    )
    sentencePrinter.print(responseSentence)
  }

  private def processUtterance(
    responder : SnavigResponder, sentence : SilSentence) : String =
  {
    if (sentence.tam.isImperative) {
      // FIXME support imperatives
      sentencePrinter.sb.contradictAssumption("", false)
    } else {
      responder.process(sentence)
    }
  }

  private def preprocess(input : String) : String =
  {
    SnavigAliases.map.get(input.trim.toLowerCase) match {
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

  def run() : Option[SnavigShell] =
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
          listenerMind match {
            case Some(targetMind) => {
              defer(DeferredUtterance(targetMind, input))
            }
            case _ => {
              defer(DeferredCommand(input))
            }
          }
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
        val snapshot = SnavigShell.restore(file, terminal)
        Some(new SnavigShell(snapshot, terminal))
      }
      case _ => {
        terminal.emitNarrative("")
        terminal.emitControl("Shutting down...")
        None
      }
    }
  }
}
