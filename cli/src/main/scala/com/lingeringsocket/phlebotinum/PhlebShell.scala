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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.collection._
import scala.util._

import java.io._

import org.slf4j._

object PhlebShell
{
  val logger =
    LoggerFactory.getLogger(
      classOf[PhlebShell])

  val PLAYER_WORD = "player-character"

  val INTERPRETER_WORD = "game-interpreter"

  val MAP_PLACE_WORD = "map-place"

  val OK = "OK."

  private val actionRespond =
    SilWord.uninflected("respond").withSense("v:817348")

  private val serializer = new PhlebSerializer

  private val responderParams = SmcResponseParams(
    verbosity = RESPONSE_COMPLETE,
    rememberConversation = false)

  val beliefParams = SpcBeliefParams(
    createImplicitIdeals = false,
    createTentativeIdeals = false,
    createTentativeEntities = true,
    createImplicitProperties = false)

  private implicit val tongue = SnlUtils.defaultTongue

  def ok = Some(OK)

  def run(
    resourcePrefix : String,
    terminal : PhlebTerminal = new PhlebConsole) : Unit =
  {
    val newShell = this.synchronized {
      val suffix = terminal.getInitSaveFile
      val file = new File(s"run${resourcePrefix}${suffix}")
      val (snapshot, init) =
        loadOrCreate(resourcePrefix, file, terminal)
      val shell = new PhlebShell(snapshot, terminal)
      if (init && suffix.nonEmpty) {
        serializer.saveSnapshot(snapshot, file)
      }
      shell
    }
    PhlebShell.run(newShell)
  }

  def run(
    firstShell : PhlebShell) : Unit =
  {
    firstShell.terminalInit()
    var shellOpt : Option[PhlebShell] = Some(firstShell)
    while (shellOpt.nonEmpty) {
      val shell = shellOpt.get
      shellOpt = shell.run
    }
  }

  private def loadOrCreate(
    resourcePrefix : String, file : File, terminal : PhlebTerminal)
      : (PhlebSnapshot, Boolean) =
  {
    if (terminal.getInitSaveFile.nonEmpty && file.exists) {
      tupleN(restore(file, terminal), false)
    } else {
      terminal.emitControl("Initializing...")
      val snapshot = createNewCosmos(resourcePrefix, terminal)
      terminal.emitControl("Initialization complete.")
      tupleN(
        snapshot,
        true)
    }
  }

  def restore(file : File, terminal : PhlebTerminal)
      : PhlebSnapshot =
  {
    terminal.emitControl(s"Restoring from $file...")
    val snapshot = serializer.loadSnapshot(file)
    terminal.emitControl("Restore complete.")
    snapshot
  }

  def createNewCosmos(
    resourcePrefix : String, terminal : PhlebTerminal) : PhlebSnapshot =
  {
    val bootCosmos = PhlebBaseline.newMutableCosmos
    val preferredSynonyms = new mutable.LinkedHashMap[SpcIdeal, String]
    val bootMind = new PhlebMind(
      bootCosmos, None, preferredSynonyms, new PhlebClock)
    bootMind.importBeliefs(
      s"${resourcePrefix}base-axioms.txt",
      SpcResponder(
        bootMind,
        beliefParams,
        SmcResponseParams(reportExceptionCodes = true)))

    val noumenalCosmos = bootCosmos.newClone()
    val noumenalMind = new PhlebMind(
      noumenalCosmos, None, preferredSynonyms, bootMind.clock)

    val playerEntity =
      bootstrapLookup(noumenalCosmos, PLAYER_WORD)

    val mindMap = new mutable.LinkedHashMap[String, PhlebMind]
    mindMap.put(PhlebSnapshot.BOOTSTRAP, bootMind)
    mindMap.put(PhlebSnapshot.NOUMENAL, noumenalMind)
    val snapshot = PhlebSnapshot(mindMap)

    lazy val executor = new PhlebExecutor(terminal, noumenalMind)
    {
      override protected def processFiat(
        annotator : SilAnnotator,
        sentence : SilSentence,
        entities : Iterable[SpcEntity])
          : Option[String] =
      {
        Some(noumenalInitializer.process(
          SprParseResult(sentence, annotator)).text)
      }

      override protected def executeActionImpl(
        ap : SilActionPredicate,
        refMap : SpcRefMap,
        subjectEntityOpt : Option[SpcEntity],
        quotationOpt : Option[String]
      ) : Option[String] =
      {
        val lemma = ap.verb.toLemma
        quotationOpt match {
          case Some(quotation) => {
            subjectEntityOpt match {
              case Some(subjectEntity) => {
                SilWord(lemma) match {
                  case SprPredefWord(PD_BELIEVE) => {
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
                processPerception(None, snapshot, ap, refMap, subjectEntityOpt)
                ok
              }
              case _ => None
            }
          }
        }
      }
    }

    lazy val noumenalInitializer : PhlebResponder = new PhlebResponder(
      Some(terminal),
      noumenalMind,
      beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
      responderParams.copy(reportExceptionCodes = true),
      executor,
      SmcCommunicationContext(tongue, Some(playerEntity), Some(playerEntity)))
    noumenalMind.importBeliefs(
      s"${resourcePrefix}game-init.txt",
      noumenalInitializer)

    val playerMindOpt = accessEntityMind(
      snapshot,
      playerEntity)
    playerMindOpt.foreach(playerMind => {
      snapshot.mindMap.put(PhlebSnapshot.PLAYER_PHENOMENAL, playerMind)
    })

    snapshot
  }

  private def importEntityBeliefs(
    terminal : PhlebTerminal,
    executor : PhlebExecutor,
    snapshot : PhlebSnapshot,
    entity : SpcEntity,
    resourceName : String) : Unit =
  {
    accessEntityMind(snapshot, entity) match {
      case Some(mind) => {
        val responder = new PhlebResponder(
          Some(terminal),
          mind, beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
          SmcResponseParams(reportExceptionCodes = true), executor,
          SmcCommunicationContext(tongue, Some(entity), Some(entity)))
        mind.importBeliefs(resourceName, responder)
      }
      case _ => {
        terminal.emitControl(
          s"Beliefs ignored for mindless entity $entity.")
      }
    }
  }

  private[phlebotinum] def executePerception(
    snapshot : PhlebSnapshot,
    perceiver : SpcEntity,
    perceived : Set[SpcEntity]) : Unit =
  {
    val entityMind = accessEntityMind(snapshot, perceiver)
    val noumenalMind = snapshot.getNoumenalMind
    val timestamp = noumenalMind.clock.getTimestamp
    logger.trace(s"PERCEIVE $perceiver $timestamp $perceived")
    entityMind.flatMap(_.perception).foreach(perception => {
      perceived.toSeq.sortBy(_.name).foreach(entity => {
        perception.perceiveEntityAssociations(
          entity, timestamp)
        perception.perceiveEntityProperties(
          entity, timestamp)
      })
    })
  }

  private def accessEntityMind(
    snapshot : PhlebSnapshot,
    entity : SpcEntity) : Option[PhlebMind] =
  {
    val bootCosmos = snapshot.getBootstrapMind.getCosmos
    val noumenalMind = snapshot.getNoumenalMind
    val noumenalCosmos = noumenalMind.getCosmos
    if (snapshot.mindMap.contains(entity.name)) {
      snapshot.mindMap.get(entity.name)
    } else {
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
              val cloned = bootCosmos.newClone()
              // the existence of the entity in the noumenal world has to
              // bleed into the phenomenal world, otherwise the entity
              // wouldn't have a point of reference for itself
              cloned.createOrReplaceEntity(entity)
              tupleN(cloned, None)
            }
            case "perceptive" => {
              val cosmos = bootCosmos.newClone()
              tupleN(
                cosmos, Some(new SpcPerception(noumenalCosmos, cosmos)))
            }
            case "omniscient" => {
              tupleN(noumenalCosmos, None)
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
      val clock = noumenalMind.clock
      val newMind = new PhlebMind(
        cosmos,
        perception, noumenalMind.preferredSynonyms, clock)
      // cogito ergo sum
      perception.foreach(_.perceiveEntity(entity, clock.getTimestamp))
      snapshot.mindMap.put(entity.name, newMind)
      Some(newMind)
    }
  }

  def singletonLookup(
    refMap : SpcRefMap,
    ref : SilReference) : Option[SpcEntity] =
  {
    refMap.get(ref) match {
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


class PhlebShell(
  snapshot : PhlebSnapshot,
  terminal : PhlebTerminal = new PhlebConsole,
  restored : Boolean = false)
{
  import PhlebShell._

  sealed trait Deferred {
  }

  case class DeferredCommand(
    quotation : String,
    isPlayerInput : Boolean = false) extends Deferred

  case class DeferredDirective(quotation : String) extends Deferred

  case class DeferredReport(quotation : String) extends Deferred

  case class DeferredComplaint(quotation : String) extends Deferred

  case class DeferredCommunication(
    speaker : SpcEntity,
    listener : SpcEntity,
    quotation : String) extends Deferred

  case class DeferredUtterance(
    listener : SpcEntity,
    listenerMind : PhlebMind,
    quotation : String) extends Deferred

  case class DeferredTermination(
  ) extends Deferred

  private val phenomenalMind = snapshot.getPhenomenalMind

  private val phenomenalCosmos = phenomenalMind.getCosmos

  private val noumenalMind = snapshot.getNoumenalMind

  private val noumenalCosmos = noumenalMind.getCosmos

  private val playerEntity =
    bootstrapLookup(noumenalCosmos, PLAYER_WORD)

  private val interpreterEntity =
    bootstrapLookup(noumenalCosmos, INTERPRETER_WORD)

  private val playerPerception = phenomenalMind.perception.get

  private val deferredQueue = new mutable.Queue[Deferred]

  private val deferredPerception = new mutable.ArrayBuffer[
    (PhlebSnapshot, SpcEntity, Set[SpcEntity])
  ]

  private var gameTurnTimestamp = SpcTimestamp.ZERO

  private var restoreFile : Option[File] = None

  private var terminated = false

  private var listenerMind : Option[(SpcEntity, PhlebMind)] = None

  private val aliases = terminal.getTranslatorOpt.map(
    _.getAliases).getOrElse(PhlebAliases.english)

  private implicit val tongue = noumenalMind.getTongue

  private val executor = new PhlebExecutor(terminal, noumenalMind)
  {
    override protected def processFiat(
      annotator : SilAnnotator,
      sentence : SilSentence,
      entities : Iterable[SpcEntity])
        : Option[String] =
    {
      if (logger.isTraceEnabled || terminal.isDebugging) {
        val printed = sentencePrinter.print(sentence)
        terminal.emitTrace(s"RESTATED $printed")
      }
      val staleEntities = findStale(entities)
      // FIXME move this to the scripting level, and discriminate
      // seeing, hearing, touching, reaching, etc
      val motion = sentence match {
        case SilPredicateSentence(
          SilActionPredicate(_, verb, _, _),
          _, _
        ) => {
          Seq("enter", "go", "scry", "visualize").contains(verb.toLemma)
        }
        case _ => false
      }
      if (staleEntities.nonEmpty && !motion) {
        val complaint = "You can only interact with what's nearby."
        defer(DeferredComplaint(complaint))
        Some(complaint)
      } else {
        val fiatUpdater = newFiatUpdater
        Some(fiatUpdater.process(
          SprParseResult(sentence, annotator)).text)
      }
    }

    override protected def executeActionImpl(
      ap : SilActionPredicate,
      refMap : SpcRefMap,
      subjectEntityOpt : Option[SpcEntity],
      quotationOpt : Option[String]
    ) : Option[String] =
    {
      val lemma = ap.verb.toLemma
      val targetRefOpt = ap.modifiers.flatMap(_ match {
        case SilAdpositionalVerbModifier(
          SprPredefAdposition(PD_TO | PD_DATIVE_TO),
          ref) => Some(ref)
        case _ => None
      }).headOption.orElse {
        ap.directObject
      }
      val targetEntityOpt = targetRefOpt.flatMap(
        ref => singletonLookup(refMap, ref))
      val targetEntitySet = targetRefOpt.flatMap(ref => refMap.get(ref))
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
              case verb @ ("translate" | "complain-translate") => {
                val translatorOpt = terminal.getTranslatorOpt
                val translation = translatorOpt.flatMap(translator => {
                  val parseResults =
                    noumenalUpdater.newParser(quotation).parseAll
                  val translations = parseResults.map(parseResult => {
                    val playerTongue = translator.getPlayerTongue
                    val responseTranslator = translator.newResponseTranslator
                    val analyzer =
                      new SprWordnetSenseAnalyzer(
                        translator.getInterpreterTongue,
                        parseResult.annotator)
                    val analyzed = analyzer.analyze(parseResult.sentence)
                    responseTranslator.translate(analyzed) match {
                      case s if (s.hasUnknown) => None
                      case translated => {
                        val playerSentencePrinter =
                          playerTongue.newSentencePrinter(playerTongue)
                        Some(SprUtils.capitalize(
                          playerSentencePrinter.print(translated)))
                      }
                    }
                  })
                  if (translations.exists(_.isEmpty)) {
                    None
                  } else {
                    val concatenated = translations.flatten.mkString(" ")
                    terminal.emitTrace(s"TRANSLATED $concatenated")
                    Some(concatenated)
                  }
                }).getOrElse(quotation)
                if (verb == "complain-translate") {
                  defer(DeferredComplaint(translation))
                } else {
                  defer(DeferredReport(translation))
                }
                ok
              }
              case "complain" => {
                defer(DeferredComplaint(quotation))
                ok
              }
              case "save" => {
                val file = getSaveFile(quotation)
                terminal.emitControl(s"Saving $file...")
                phenomenalMind.stopConversation()
                serializer.saveSnapshot(
                  snapshot,
                  file)
                phenomenalMind.startConversation()
                ok
              }
              case "restore" => {
                restoreFile = Some(getSaveFile(quotation))
                ok
              }
              case _ => None
            }
          } else {
            lemma match {
              case "say" | "reply" | "ask" | "tell" => {
                defer(DeferredReport(DQUOTE + quotation + DQUOTE))
                ok
              }
              case _ => None
            }
          }
        }
        case _ => {
          lemma match {
            case "debug" => {
              terminal.toggleDebug()
              ok
            }
            case "visualize" => {
              visualize(phenomenalCosmos.getGraph, targetEntitySet)
              ok
            }
            case "scry" => {
              visualize(noumenalCosmos.getGraph, targetEntitySet)
              ok
            }
            case "terminate" if (
              subjectEntityOpt == Some(interpreterEntity)
            ) => {
              defer(DeferredTermination())
              ok
            }
            case "perceive" => {
              processPerception(
                Some(PhlebShell.this), snapshot, ap, refMap, subjectEntityOpt)
              ok
            }
            case "talk" => {
              targetEntityOpt match {
                case Some(targetEntity) => {
                  if (targetEntity == playerEntity) {
                    talkToSelf()
                  } else {
                    accessEntityMind(targetEntity) match {
                      case Some(entityMind) => {
                        listenerMind = Some(tupleN(targetEntity, entityMind))
                        defer(DeferredReport(
                          "(You are now in conversation.  Say TTYL to stop.)"))
                      }
                      case _ => {
                        val annotator = SpcAnnotator()
                        defer(DeferredReport(
                          SprUtils.capitalize(
                            translateResponseSentence(
                              noResponse(conversationalReference(
                                annotator, targetEntity)),
                              true))))
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

    private def visualize(
      graph : SpcGraph,
      targetEntitySetOpt : Option[Set[SpcEntity]]) : Unit =
    {
      val visualizer = new SpcGraphVisualizer(
        graph,
        SpcGraphVisualizer.entityFullOptions.copy(
          entityFilter = (entity => {
            targetEntitySetOpt match {
              case Some(set) => {
                set.contains(entity)
              }
              case _ => true
            }
          }),
          includeProperties = targetEntitySetOpt.isEmpty
        )
      )
      terminal.emitVisualization(visualizer)
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
      resultCollector : SmcResultCollector[SpcEntity])
        : Option[String] =
    {
      val annotator = SpcAnnotator()
      def playerRef =
        annotator.pronounRef(PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
          phenomenalMind)
      val reannotated = annotator.copy(
        predicate, SilPhraseCopyOptions(preserveNotes = true))
      val newPredicate = reannotated match {
        case ap : SilActionPredicate => ap.copy(subject = playerRef)
        case _ => reannotated
      }
      val sentence = SilPredicateSentence(newPredicate)
      val refSet = SilUtils.collectReferences(sentence).toSet
      val refMap = resultCollector.refMap
      val entities = refSet.flatMap(ref =>
        refMap.get(ref).getOrElse(Set.empty))
      validateFiat(annotator, newPredicate, refMap).orElse(
        processFiat(annotator, sentence, entities)
      )
    }
  }

  private val sentencePrinter = new SilSentencePrinter(
    SnlUtils.defaultTongue, noumenalMind)

  private val playerToInterpreter = SmcCommunicationContext(
    tongue,
    Some(playerEntity),
    Some(interpreterEntity),
    terminal.getTranslatorOpt.map(_.getPlayerTongue)
  )

  private val noumenalUpdater : PhlebResponder = new PhlebResponder(
    Some(terminal),
    noumenalMind,
    beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
    responderParams,
    executor, playerToInterpreter)

  private val phenomenalResponder = new PhlebResponder(
    Some(terminal),
    phenomenalMind,
    beliefParams.copy(acceptance = IGNORE_BELIEFS),
    responderParams.copy(
      existenceAssumption = EXISTENCE_ASSUME_UNKNOWN,
      rememberConversation = true),
    executor, playerToInterpreter)

  private def newFiatUpdater : PhlebResponder =
  {
    // preserve conversation scope
    val fiatMind = phenomenalMind.spawn(noumenalCosmos)
    new PhlebResponder(
      Some(terminal),
      fiatMind,
      beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
      responderParams,
      executor, playerToInterpreter)
  }

  def terminalInit() : Unit =
  {
    terminal.getInitCommandOpt.foreach(command => {
      defer(DeferredDirective(command))
    })
  }

  def defer(deferred : Deferred) : Unit =
  {
    deferredQueue += deferred
  }

  private def validateFiat(
    annotator : SpcAnnotator,
    predicate : SilPredicate,
    refMap : SpcRefMap)
      : Option[String] =
  {
    val result = phenomenalResponder.processTriggerablePredicate(
      annotator, phenomenalCosmos, predicate, refMap,
      APPLY_CONSTRAINTS_ONLY, 0, true).message
    if (result == ok) {
      None
    } else {
      result
    }
  }

  private def accessEntityMind(
    entity : SpcEntity) : Option[PhlebMind] =
  {
    PhlebShell.accessEntityMind(snapshot, entity)
  }

  private def parserForTranslation(
    expanded : String,
    translatorOpt : Option[PhlebTranslator],
    responder : PhlebResponder) : SprParser =
  {
    translatorOpt.map(translator => {
      // FIXME need to hybridize in responder for
      // genderAnalyzer and maybe word labeler
      val playerTongue = translator.getPlayerTongue
      val playerScorer = new SpcContextualScorer(
        playerTongue, responder)
      val context = SprContext(
        new SprWordnetLabeler(playerTongue),
        scorer = new SnlTranslatingScorer(
          playerScorer,
          playerTongue,
          translator.getInterpreterTongue,
          translator.alignment,
          translator.playerToInterpreter
        ),
        annotator = responder.newAnnotator,
        genderAnalyzer = playerTongue)
      SprParser(expanded, context)
    }).getOrElse {
      responder.newParser(expanded)
    }
  }

  private def translateInput(
    parseResult : SprParseResult,
    translatorOpt : Option[PhlebTranslator],
    responder : PhlebResponder,
    dumpParse : Boolean) : SprParseResult =
  {
    translatorOpt.map(translator => {
      val interpreterScorer = new SpcContextualScorer(
        translator.getInterpreterTongue, responder)
      val inputTranslator = translator.newInputTranslator(
        Some(interpreterScorer))
      val analyzer =
        new SprWordnetSenseAnalyzer(
          translator.getPlayerTongue,
          parseResult.annotator)
      val analyzed = analyzer.analyze(parseResult.sentence)
      val translatedInput =
        inputTranslator.translate(analyzed)
      if (dumpParse) {
        terminal.emitTrace(s"TRANSLATED $translatedInput")
      }
      parseResult.copy(
        sentence = translatedInput,
        annotator = inputTranslator.annotator
      )
    }).getOrElse {
      parseResult
    }
  }

  private def translateResponseSentence(
    sentence : SilSentence,
    terminated : Boolean) : String =
  {
    val text = {
      if (terminated) {
        sentencePrinter.print(sentence)
      } else {
        sentencePrinter.printUnterminated(sentence)
      }
    }
    translateResponse(
      text,
      SmcResponse(
        tongue,
        text,
        phenomenalResponder.newAnnotator,
        sentence
      ),
      terminal.getTranslatorOpt,
      terminated)
  }

  private def translateResponse(
    output : String,
    response : SmcResponse,
    translatorOpt : Option[PhlebTranslator],
    terminated : Boolean = true) : String =
  {
    terminal.getTranslatorOpt.map(translator => {
      val translated = translator.newResponseTranslator.translate(
        response.sentence)
      // FIXME genderAnalyzer should hybridize in appropriate mind
      val playerTongue = translator.getPlayerTongue
      val playerSentencePrinter = playerTongue.newSentencePrinter(
        playerTongue)
      val printed = translated match {
        case _ : SilUnparsedSentence => {
          playerSentencePrinter.printUnterminated(translated)
        }
        case _ => {
          if (terminated) {
            playerSentencePrinter.print(translated)
          } else {
            playerSentencePrinter.printUnterminated(translated)
          }
        }
      }
      val capitalized = SprUtils.capitalize(printed)
      terminal.emitTrace(s"TRANSLATED $capitalized")
      capitalized
    }).getOrElse(output)
  }

  private def processDeferred() : Unit =
  {
    var first = true
    while (deferredQueue.nonEmpty) {
      deferredQueue.dequeue() match {
        case DeferredTermination() => {
          logger.trace("TERMINATE")
          terminated = true
        }
        case DeferredDirective(input) => {
          terminal.emitTrace(s"DIRECTIVE $input")
          val parseResults = noumenalUpdater.newParser(input).parseAll
          parseResults.foreach(parseResult => {
            val output = noumenalUpdater.process(parseResult).text
            assert(output == OK, tupleN(parseResult, output))
          })
        }
        case DeferredUtterance(_, _, "TTYL" | "ttyl") => {
          listenerMind = None
          defer(DeferredReport(
            "(You are no longer in conversation.)"))
        }
        case DeferredUtterance(targetEntity, targetMind, input) => {
          terminal.emitTrace(s"UTTERANCE $input")
          val communicationContext = SmcCommunicationContext(
            tongue,
            Some(playerEntity),
            Some(targetEntity)
          )
          val responder = new PhlebResponder(
            Some(terminal),
            targetMind,
            beliefParams.copy(acceptance = IGNORE_BELIEFS),
            SmcResponseParams(), executor, communicationContext)
          val translatorOpt = terminal.getTranslatorOpt
          val parser = parserForTranslation(
            input,
            translatorOpt,
            responder)
          parser.parseAll.foreach(parseResult => {
            val dumpParse = (terminal.isDebugging || logger.isTraceEnabled)
            val translatedInput = translateInput(
              parseResult, translatorOpt, responder, dumpParse)
            val output = processUtterance(responder, translatedInput)
            logger.trace(s"RESULT ${output.text}")
            terminal.emitNarrative("")
            // FIXME allow side effects of conversation
            assert(deferredQueue.isEmpty)
            val translatedOutput = translateResponse(
              output.text, output, translatorOpt, true)
            terminal.emitNarrative(DQUOTE + translatedOutput + DQUOTE)
          })
        }
        case DeferredCommand(input, isPlayerInput) => {
          terminal.emitTrace(s"COMMAND $input")
          updatePerception()
          val expanded = preprocess(input)
          if (expanded != input) {
            terminal.emitTrace(s"EXPANDED $expanded")
          }
          val dumpParse = first &&
            (terminal.isDebugging || logger.isTraceEnabled)
          val translatorOpt = {
            if (isPlayerInput) {
              terminal.getTranslatorOpt
            } else {
              None
            }
          }
          val parser = parserForTranslation(
            expanded, translatorOpt, phenomenalResponder)
          val parseResults = parser.parseAll
          parseResults.foreach(parseResult => {
            if (dumpParse) {
              terminal.emitTrace(s"PARSED ${parseResult.sentence}")
            }
            val translated = translateInput(
              parseResult, translatorOpt, phenomenalResponder, dumpParse)
            val response = phenomenalResponder.process(translated)
            var output = response.text
            logger.trace(s"RESULT $output")
            terminal.emitNarrative("")
            var assumption = ""
            val sentence = translated.sentence
            if (first) {
              first = false
              if (output != OK) {
                val complaints = deferredQueue.flatMap(_ match {
                  case c : DeferredComplaint => Some(c)
                  case _ => None
                })
                if (complaints.nonEmpty ||
                  sentence.tam.isImperative)
                {
                  deferredQueue.clear()
                }
                complaints.foreach(complaint => {
                  output = ""
                  terminal.emitNarrative(complaint.quotation)
                })
              }
              if (sentence.tam.isInterrogative &&
                !output.equals(sentencePrinter.responseBundle.respondDontKnow))
              {
                val entities =
                  phenomenalMind.getConversation.getUtterances.
                    takeRight(2).flatMap(
                      _.refMap.values.flatten)
                val staleEntities = findStale(entities)
                if (staleEntities.nonEmpty) {
                  assumption =
                    "the game-interpreter invokes all continuity-assumptions"
                }
              }
            }
            if (output.nonEmpty) {
              output = translateResponse(
                output, response, terminal.getTranslatorOpt)
              terminal.emitNarrative(output)
            }
            if (assumption.nonEmpty) {
              terminal.emitNarrative("")
              defer(DeferredDirective(assumption))
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
        case DeferredCommunication(speaker, listener, quotation) => {
          if (speaker == listener) {
            talkToSelf()
          } else {
            val annotator = SpcAnnotator()
            val listenerReference = conversationalReference(
              annotator, listener)
            val reply = accessEntityMind(listener) match {
              case Some(entityMind) => {
                val communicationContext = SmcCommunicationContext(
                  tongue,
                  Some(speaker),
                  Some(listener)
                )
                val entityResponder = new PhlebResponder(
                  Some(terminal),
                  entityMind,
                  beliefParams.copy(acceptance = IGNORE_BELIEFS),
                  SmcResponseParams(), executor, communicationContext)
                // FIXME use parseAll instead, and deal with fact
                // that entity language and interpreter language
                // may be different
                val translatorOpt = terminal.getTranslatorOpt
                val parser = parserForTranslation(
                  quotation,
                  translatorOpt,
                  entityResponder)
                val parseResult = parser.parseOne
                val dumpParse = (terminal.isDebugging || logger.isTraceEnabled)
                val translatedInput = translateInput(
                  parseResult, translatorOpt, entityResponder, dumpParse)
                val response = processUtterance(
                  entityResponder, translatedInput)
                val translatedResponse = translateResponse(
                  response.text, response, translatorOpt)
                val responseSentence = SilPredicateSentence(
                  SilActionPredicate(
                    listenerReference,
                    actionRespond,
                    Some(annotator.quotationRef(translatedResponse))
                  )
                )
                translateResponseSentence(responseSentence, false)
              }
              case _ => {
                translateResponseSentence(noResponse(listenerReference), true)
              }
            }
            terminal.emitNarrative("")
            terminal.emitNarrative(SprUtils.capitalize(reply))
          }
        }
      }
    }
    updatePerception()
  }

  private[phlebotinum] def deferPerception(
    snapshot : PhlebSnapshot,
    perceiver : SpcEntity,
    perceived : Set[SpcEntity]) : Unit =
  {
    deferredPerception += tupleN(
      snapshot,
      perceiver,
      perceived
    )
  }

  private def updatePerception() : Unit =
  {
    deferredPerception.foreach {
      case (snapshot, perceiver, perceived) => {
        executePerception(
          snapshot, perceiver, perceived)
      }
    }
    deferredPerception.clear()
  }

  private def conversationalReference(
    annotator : SpcAnnotator,
    entity : SpcEntity) : SilReference =
  {
    phenomenalMind.thirdPersonDeictic(
      annotator, Set(entity)
    ).getOrElse {
      phenomenalMind.specificReference(
        annotator, entity, DETERMINER_DEFINITE)
    }
  }

  private def findStale(entities : Iterable[SpcEntity]) : Iterable[SpcEntity] =
  {
    entities.filter(entity => {
      val timestamp = playerPerception.getEntityTimestamp(entity)
      timestamp.map(_.isBefore(gameTurnTimestamp)).getOrElse(false)
    })
  }

  private def talkToSelf() : Unit =
  {
    defer(DeferredReport(
      "Only crazy people talk to themselves."))
  }

  private def noResponse(listenerReference : SilReference) : SilSentence =
  {
    SilPredicateSentence(
      SilActionPredicate(
        listenerReference,
        actionRespond
      ),
      SilTam.indicative.negative
    )
  }

  private def processUtterance(
    responder : PhlebResponder, parseResult : SprParseResult) : SmcResponse =
  {
    if (parseResult.sentence.tam.isImperative) {
      // FIXME support imperatives
      val text = sentencePrinter.responseBundle.contradictAssumption("", false)
      SmcResponse(
        tongue,
        text,
        responder.newAnnotator,
        SilUnparsedSentence(text))
    } else {
      responder.process(parseResult)
    }
  }

  private def preprocess(input : String) : String =
  {
    aliases.get(input.trim.toLowerCase) match {
      case Some(replacement) => {
        preprocess(replacement)
      }
      case _ => {
        // special case for examine, which can take an object
        if (input.startsWith("x ")) {
          aliases("x") + " "  + input.stripPrefix("x ")
        } else {
          input
        }
      }
    }
  }

  def run : Option[PhlebShell] =
  {
    phenomenalMind.startConversation()
    var exit = false
    terminal.emitNarrative("")

    gameTurnTimestamp = noumenalMind.clock.getTimestamp
    if (!restored) {
      defer(DeferredDirective("the game-turn initializes"))
    }
    defer(DeferredDirective("the game-turn reloads"))
    processDeferred()

    terminal.emitNarrative("")

    while (!exit) {
      noumenalMind.clock.startNewTurn()
      gameTurnTimestamp = noumenalMind.clock.getTimestamp

      terminal.emitPrompt()
      terminal.readCommand match {
        case Some(input) => {
          defer(DeferredDirective("the game-turn advances"))
          processDeferred()
          listenerMind match {
            case Some((targetEntity, targetMind)) => {
              defer(DeferredUtterance(targetEntity, targetMind, input))
            }
            case _ => {
              defer(DeferredCommand(input, true))
            }
          }
          processDeferred()
          terminal.emitNarrative("")
        }
        case _ => {
          exit = true
        }
      }
      if (terminated || restoreFile.nonEmpty) {
        exit = true
      }
    }
    restoreFile match {
      case Some(file) => {
        val snapshot = PhlebShell.restore(file, terminal)
        Some(new PhlebShell(snapshot, terminal, true))
      }
      case _ => {
        terminal.emitNarrative("")
        terminal.emitControl("Shutting down...")
        None
      }
    }
  }
}

abstract class PhlebExecutor(terminal : PhlebTerminal, noumenalMind : PhlebMind)
    extends SmcExecutor[SpcEntity]
{
  import PhlebShell._

  private implicit val tongue = noumenalMind.getTongue

  override def executeAction(
    ap : SilActionPredicate,
    refMap : SpcRefMap) : Option[String] =
  {
    val subjectEntityOpt = singletonLookup(refMap, ap.subject)
    val quotationOpt = ap.directObject match {
      case Some(SilQuotationReference(quotation, _)) => Some(quotation)
      case Some(ref) => {
        singletonLookup(refMap, ref) match {
          case Some(entity) => {
            entity match {
              // FIXME type check
              case SpcTransientEntity(_, value, _) => Some(value)
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
      refMap,
      subjectEntityOpt,
      quotationOpt)
  }

  protected def executeActionImpl(
    ap : SilActionPredicate,
    refMap : SpcRefMap,
    subjectEntityOpt : Option[SpcEntity],
    quotationOpt : Option[String]
  ) : Option[String]

  override def executeInvocation(
    invocation : SmcStateChangeInvocation[SpcEntity],
    refMap : SpcRefMap)
      : Option[String] =
  {
    val annotator = SpcAnnotator()
    val sentence = SilPredicateSentence(
      SilStatePredicate(
        noumenalMind.specificReferences(annotator, invocation.entities),
        STATE_PREDEF_BE.toVerb,
        SilPropertyState(invocation.state)
      )
    )
    processFiat(annotator, sentence, invocation.entities)
  }

  protected def processFiat(
    annotator : SilAnnotator,
    sentence : SilSentence,
    entities : Iterable[SpcEntity])
      : Option[String]

  protected def processPerception(
    shellOpt : Option[PhlebShell],
    snapshot : PhlebSnapshot,
    ap : SilActionPredicate,
    refMap : SpcRefMap,
    subjectEntityOpt : Option[SpcEntity]) : Unit =
  {
    subjectEntityOpt match {
      case Some(subjectEntity) => {
        ap.directObject.foreach(directObjectRef => {
          shellOpt match {
            case Some(shell) => {
              shell.deferPerception(
                snapshot,
                subjectEntity,
                refMap(directObjectRef))
            }
            case _ => {
              executePerception(
                snapshot,
                subjectEntity,
                refMap(directObjectRef)
              )
            }
          }
        })
        ok
      }
      case _ => None
    }
  }
}
