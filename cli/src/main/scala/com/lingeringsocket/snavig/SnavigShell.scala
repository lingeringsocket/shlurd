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

  private val serializer = new SnavigSerializer

  private val responderParams = SmcResponseParams(verbosity = RESPONSE_COMPLETE)

  def ok = Some(OK)

  def run(terminal : SnavigTerminal = new SnavigConsole)
  {
    val newShell = this.synchronized {
      val file = new File("run/snavig-init-save.zip")
      val (snapshot, init) =
        loadOrCreate(file, terminal)
      val shell = new SnavigShell(snapshot, terminal)
      if (init) {
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
      val snapshot = createNewCosmos(terminal)
      terminal.emitControl("Initialization complete.")
      tupleN((
        snapshot,
        true))
    }
  }

  def restore(file : File, terminal : SnavigTerminal)
      : SnavigSnapshot =
  {
    terminal.emitControl(s"Restoring from $file...")
    val snapshot = serializer.loadSnapshot(file)
    terminal.emitControl("Restore complete.")
    snapshot
  }

  def createNewCosmos(terminal : SnavigTerminal) : SnavigSnapshot =
  {
    val bootCosmos = ShlurdPrimordialWordnet.newMutableCosmos
    val preferredSynonyms = new mutable.LinkedHashMap[SpcIdeal, String]
    val bootMind = new SnavigMind(bootCosmos, None, preferredSynonyms)
    bootMind.importBeliefs("/example-snavig/game-axioms.txt")

    val noumenalCosmos = bootCosmos.newClone
    val noumenalMind = new SnavigMind(
      noumenalCosmos, None, preferredSynonyms)

    val playerEntity =
      bootstrapLookup(noumenalCosmos, PLAYER_WORD)

    val mindMap = new mutable.LinkedHashMap[String, SnavigMind]
    mindMap.put(SnavigSnapshot.BOOTSTRAP, bootMind)
    mindMap.put(SnavigSnapshot.NOUMENAL, noumenalMind)
    val snapshot = SnavigSnapshot(mindMap)

    lazy val executor = new SnavigExecutor(noumenalMind)
    {
      override protected def processFiat(
        sentence : SilSentence,
        entities : Iterable[SpcEntity])
          : Option[String] =
      {
        Some(noumenalInitializer.process(sentence))
      }

      override protected def executeActionImpl(
        ap : SilActionPredicate,
        referenceMap : Map[SilReference, Set[SpcEntity]],
        subjectEntityOpt : Option[SpcEntity],
        quotationOpt : Option[String]
      ) : Option[String] =
      {
        val lemma = ap.action.toLemma
        quotationOpt match {
          case Some(quotation) => {
            subjectEntityOpt match {
              case Some(subjectEntity) => {
                lemma match {
                  case "believe" => {
                    // FIXME if quotation does not start with slash,
                    // then interpret inline
                    importEntityBeliefs(
                      terminal,
                      this,
                      snapshot,
                      subjectEntity,
                      quotation)
                    ok
                  }
                  case _ => None
                }
              }
              case _ => None
            }
          }
          case _ => {
            lemma match {
              case "perceive" => {
                processPerception(snapshot, ap, referenceMap, subjectEntityOpt)
                ok
              }
              case _ => None
            }
          }
        }
      }
    }

    lazy val noumenalInitializer : SnavigResponder = new SnavigResponder(
      None, noumenalMind, ACCEPT_MODIFIED_BELIEFS, responderParams,
      executor, SmcCommunicationContext(Some(playerEntity), Some(playerEntity)))
    initMind(
      noumenalMind,
      noumenalInitializer,
      "/example-snavig/game-init.txt")

    val playerMindOpt = accessEntityMind(
      snapshot,
      playerEntity)
    playerMindOpt.foreach(playerMind => {
      snapshot.mindMap.put(SnavigSnapshot.PLAYER_PHENOMENAL, playerMind)
    })

    snapshot
  }

  private def initMind(
    mind : ShlurdCliMind,
    responder : SnavigResponder,
    resourceName : String)
  {
    val dup = mind.getCosmos.isDuplicateBeliefResource(resourceName)
    assert(!dup)

    val source = Source.fromFile(
      ResourceUtils.getResourceFile(resourceName))
    val sentences = mind.newParser(
      source.getLines.filterNot(_.isEmpty).mkString("\n")).parseAll
    sentences.foreach(sentence => {
      val output = responder.process(mind.analyzeSense(sentence))
      assert(output == OK, tupleN((sentence, output)))
    })
  }

  private def importEntityBeliefs(
    terminal : SnavigTerminal,
    executor : SnavigExecutor,
    snapshot : SnavigSnapshot,
    entity : SpcEntity,
    resourceName : String)
  {
    accessEntityMind(snapshot, entity) match {
      case Some(mind) => {
        val responder = new SnavigResponder(
          None, mind, ACCEPT_MODIFIED_BELIEFS,
          SmcResponseParams(), executor,
          SmcCommunicationContext(Some(entity), Some(entity)))
        initMind(mind, responder, resourceName)
      }
      case _ => {
        terminal.emitControl(
          s"Beliefs ignored for mindless entity $entity.")
      }
    }
  }

  private[snavig] def executePerception(
    snapshot : SnavigSnapshot,
    perceiver : SpcEntity,
    perceived : Set[SpcEntity])
  {
    logger.trace(s"PERCEIVE $perceiver $perceived")
    val entityMind = accessEntityMind(snapshot, perceiver)
    val noumenalMind = snapshot.getNoumenalMind
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

  private def accessEntityMind(
    snapshot : SnavigSnapshot,
    entity : SpcEntity) : Option[SnavigMind] =
  {
    val bootCosmos = snapshot.getBootstrapMind.getCosmos
    val noumenalMind = snapshot.getNoumenalMind
    val noumenalCosmos = noumenalMind.getCosmos
    if (snapshot.mindMap.contains(entity.name)) {
      snapshot.mindMap.get(entity.name)
    } else {
      lazy val newCosmos = bootCosmos.newClone
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
        case _ => {
          return None
        }
      }
      val newMind = new SnavigMind(
        cosmos,
        perception, noumenalMind.preferredSynonyms)
      snapshot.mindMap.put(entity.name, newMind)
      Some(newMind)
    }
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

  private def bootstrapLookup(
    cosmos : SpcCosmos, word : String) : SpcEntity =
  {
    cosmos.uniqueEntity(
      cosmos.resolveQualifiedNoun(
        word, REF_SUBJECT, Set())).get
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
    listener : SpcEntity,
    listenerMind : SnavigMind,
    quotation : String) extends Deferred

  case class DeferredPhenomenon(belief : String) extends Deferred

  private val bootMind = snapshot.getBootstrapMind

  private val bootCosmos = bootMind.getCosmos

  private val phenomenalMind = snapshot.getPhenomenalMind

  private val noumenalMind = snapshot.getNoumenalMind

  private val noumenalCosmos = noumenalMind.getCosmos

  private val playerEntity =
    bootstrapLookup(noumenalCosmos, PLAYER_WORD)

  private val interpreterEntity =
    bootstrapLookup(noumenalCosmos, INTERPRETER_WORD)

  private val playerPerception = phenomenalMind.perception.get

  private val deferredQueue = new mutable.Queue[Deferred]

  private var gameTurnTimestamp = SpcTimestamp.ZERO

  private var restoreFile : Option[File] = None

  private var listenerMind : Option[(SpcEntity, SnavigMind)] = None

  private val executor = new SnavigExecutor(noumenalMind)
  {
    override protected def processFiat(
      sentence : SilSentence,
      entities : Iterable[SpcEntity])
        : Option[String] =
    {
      if (logger.isTraceEnabled) {
        val printed = sentencePrinter.print(sentence)
        logger.trace(s"FIAT $printed")
      }
      val staleEntities = findStale(entities)
      if (staleEntities.nonEmpty) {
        // FIXME move this to the scripting level, and discriminate
        // seeing, hearing, touching, reaching, etc
        val complaint = "You can only interact with what's nearby."
        defer(DeferredComplaint(complaint))
        Some(complaint)
      } else {
        Some(noumenalUpdater.process(sentence))
      }
    }

    override protected def executeActionImpl(
      ap : SilActionPredicate,
      referenceMap : Map[SilReference, Set[SpcEntity]],
      subjectEntityOpt : Option[SpcEntity],
      quotationOpt : Option[String]
    ) : Option[String] =
    {
      val lemma = ap.action.toLemma
      val targetRefOpt = ap.modifiers.flatMap(_ match {
        case SilAdpositionalVerbModifier(
          SilAdposition.TO,
          ref) => Some(ref)
        case _ => None
      }).headOption
      val targetEntityOpt = targetRefOpt.flatMap(
        ref => singletonLookup(referenceMap, ref))
      quotationOpt match {
        case Some(quotation) => {
          if (subjectEntityOpt == Some(playerEntity)) {
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
          } else if (subjectEntityOpt == Some(interpreterEntity)) {
            lemma match {
              case "say" | "recite" => {
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
          } else {
            None
          }
        }
        case _ => {
          lemma match {
            case "perceive" => {
              processPerception(snapshot, ap, referenceMap, subjectEntityOpt)
              ok
            }
            case "talk" => {
              targetEntityOpt match {
                case Some(targetEntity) => {
                  if (targetEntity == playerEntity) {
                    talkToSelf
                  } else {
                    accessEntityMind(targetEntity) match {
                      case Some(entityMind) => {
                        listenerMind = Some(tupleN((targetEntity, entityMind)))
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

    override def executeImperative(
      predicate : SilPredicate,
      referenceMap : Map[SilReference, Set[SpcEntity]])
        : Option[String] =
    {
      def playerRef =
        SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)
      val newPredicate = predicate match {
        case ap : SilActionPredicate => ap.copy(subject = playerRef)
        case _ => predicate
      }
      val sentence = SilPredicateSentence(newPredicate)
      val entities = SilUtils.collectReferences(sentence).flatMap(
        referenceMap.get(_).getOrElse(Set.empty))
      processFiat(sentence, entities)
    }
  }

  private val sentencePrinter = new SilSentencePrinter

  private val playerToInterpreter = SmcCommunicationContext(
    Some(playerEntity),
    Some(interpreterEntity)
  )

  private val noumenalUpdater : SnavigResponder = new SnavigResponder(
    Some(this), noumenalMind, ACCEPT_MODIFIED_BELIEFS, responderParams,
    executor, playerToInterpreter)

  private val phenomenalResponder = new SnavigResponder(
    None, phenomenalMind, ACCEPT_NO_BELIEFS,
    responderParams.copy(existenceAssumption = EXISTENCE_ASSUME_UNKNOWN),
    executor, playerToInterpreter)

  private val phenomenalUpdater = new SnavigResponder(
    None, phenomenalMind, ACCEPT_MODIFIED_BELIEFS, responderParams,
    executor, playerToInterpreter)

  def defer(deferred : Deferred)
  {
    deferredQueue += deferred
  }

  def deferPhenomenon(belief : String)
  {
    defer(DeferredPhenomenon(belief))
  }

  private def accessEntityMind(
    entity : SpcEntity) : Option[SnavigMind] =
  {
    SnavigShell.accessEntityMind(snapshot, entity)
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
        case DeferredUtterance(_, _, "TTYL" | "ttyl") => {
          listenerMind = None
          defer(DeferredReport(
            "(You are no longer in conversation.)"))
        }
        case DeferredUtterance(targetEntity, targetMind, input) => {
          logger.trace(s"UTTERANCE $input")
          val communicationContext = SmcCommunicationContext(
            Some(playerEntity),
            Some(targetEntity)
          )
          val responder = new SnavigResponder(
            None, targetMind, ACCEPT_NO_BELIEFS,
            SmcResponseParams(), executor, communicationContext)
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
                if (complaints.nonEmpty || sentence.tam.isImperative) {
                  deferredQueue.clear
                }
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
                if (findStale(entities).nonEmpty) {
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
          if (speaker == listener) {
            talkToSelf
          } else {
            val listenerReference = phenomenalMind.specificReference(
              listener, DETERMINER_UNIQUE)
            val reply = accessEntityMind(listener) match {
              case Some(entityMind) => {
                val communicationContext = SmcCommunicationContext(
                  Some(speaker),
                  Some(listener)
                )
                val entityResponder = new SnavigResponder(
                  None, entityMind, ACCEPT_NO_BELIEFS,
                  SmcResponseParams(), executor, communicationContext)
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
      }
    }
  }

  private def findStale(entities : Iterable[SpcEntity]) : Iterable[SpcEntity] =
  {
    entities.filter(entity => {
      val timestamp = playerPerception.getEntityTimestamp(entity)
      timestamp.map(_.isBefore(gameTurnTimestamp)).getOrElse(false)
    })
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

    gameTurnTimestamp = noumenalMind.getTimestamp
    defer(DeferredDirective("the game-turn debuts"))
    processDeferred

    while (!exit) {
      noumenalMind.startNewTurn
      gameTurnTimestamp = noumenalMind.getTimestamp
      terminal.emitPrompt
      terminal.readCommand match {
        case Some(input) => {
          defer(DeferredDirective("the game-turn advances"))
          processDeferred
          listenerMind match {
            case Some((targetEntity, targetMind)) => {
              defer(DeferredUtterance(targetEntity, targetMind, input))
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

abstract class SnavigExecutor(noumenalMind : SnavigMind)
    extends SmcExecutor[SpcEntity]
{
  import SnavigShell._

  override def executeAction(
    ap : SilActionPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Option[String] =
  {
    val lemma = ap.action.toLemma
    val subjectEntityOpt = singletonLookup(referenceMap, ap.subject)
    val quotationOpt = ap.directObject match {
      case Some(SilQuotationReference(quotation)) => Some(quotation)
      case Some(ref) => {
        singletonLookup(referenceMap, ref) match {
          case Some(entity) => {
            entity match {
              // FIXME type check
              case SpcTransientEntity(_, value) => Some(value)
              case _ => None
            }
          }
          case _ => None
        }
      }
      case _ => None
    }
    executeActionImpl(
      ap,
      referenceMap,
      subjectEntityOpt,
      quotationOpt)
  }

  protected def executeActionImpl(
    ap : SilActionPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]],
    subjectEntityOpt : Option[SpcEntity],
    quotationOpt : Option[String]
  ) : Option[String]

  override def executeInvocation(
    invocation : SmcStateChangeInvocation[SpcEntity],
    referenceMap : Map[SilReference, Set[SpcEntity]])
      : Option[String] =
  {
    val sentence = SilPredicateSentence(
      SilStatePredicate(
        noumenalMind.specificReferences(invocation.entities),
        SilPropertyState(invocation.state)
      )
    )
    processFiat(sentence, invocation.entities)
  }

  protected def processFiat(
    sentence : SilSentence,
    entities : Iterable[SpcEntity])
      : Option[String]

  protected def processPerception(
    snapshot : SnavigSnapshot,
    ap : SilActionPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]],
    subjectEntityOpt : Option[SpcEntity])
  {
    subjectEntityOpt match {
      case Some(subjectEntity) => {
        ap.directObject.foreach(directObjectRef => {
          executePerception(
            snapshot,
            subjectEntity,
            referenceMap(directObjectRef))
        })
        ok
      }
      case _ => None
    }
  }
}
