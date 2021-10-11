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
import scala.util._

import java.io._

abstract class ShlurdCliBaseApp extends App
{
  val serializer = new ShlurdCliSerializer
  val file = new File("run/shlurd-mind.zip")
  val terminal = newTerminal
  val mind = loadOrCreate(file, terminal)
  val shell = new ShlurdCliShell(mind, terminal)
  shell.run()
  // should have some option to control this
  if (false) {
    serializer.saveMind(mind, file)
  }

  def newTerminal : ShlurdCliTerminal

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

object ShlurdCliApp extends ShlurdCliBaseApp
{
  override def newTerminal = new ShlurdCliTerminal
}

class ShlurdCliTerminal
{
  protected def loadBeliefs(bootMind : SpcMind) : Unit =
  {
    Using.resource(ResourceUtils.getResourceSource("/console/beliefs.txt")) {
      source => bootMind.loadBeliefs(source)
    }
  }

  def initialize(bootMind : SpcMind) : Unit =
  {
    loadBeliefs(bootMind)
    emitControl("Hello, human!")
  }

  def beforeCommand(mind : ShlurdCliMind) : Unit =
  {
  }

  def afterCommand(mind : ShlurdCliMind) : Unit =
  {
  }

  def emitPrompt() : Unit =
  {
    print("> ")
  }

  def emitControl(msg : String) : Unit =
  {
    if (msg.isEmpty) {
      println()
    } else {
      emitResponse(msg)
    }
  }

  def emitResponse(msg : String) : Unit =
  {
    println(s"[SHLURD] $msg")
  }

  def readCommand : Option[String] =
  {
    Option(StdIn.readLine())
  }
}

object ShlurdCliShell
{
  def newMind(terminal : ShlurdCliTerminal) =
  {
    val cosmos = ShlurdPrincetonPrimordial.newMutableCosmos
    val preferredSynonyms = new mutable.LinkedHashMap[SpcIdeal, String]
    val bootMind = new SpcWordnetOntologyMind(
      SnlUtils.defaultTongue, cosmos, preferredSynonyms)
    terminal.initialize(bootMind)
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

  private def formVerbUrl(verb : String, form : SpcForm) : String =
  {
    val opt = mind.getOntology.getFormSynset(form) match {
      case Some(synset) => {
        val urlOpt = verb match {
          case "define" => {
            Some(ShlurdCliCloud.getBabelnetDefinitionUrl(synset))
          }
          case "sketch" => {
            ShlurdCliCloud.getBabelnetThumbUrl(synset)
          }
          case _ => {
            ShlurdCliCloud.getBabelnetImageUrl(synset)
          }
        }
        urlOpt match {
          case Some(url) => {
            Some(url.toString)
          }
          case _ => None
        }
      }
      case _ => None
    }
    opt match {
      case Some(urlString) => urlString
      case _ => {
        val noun = SpcWordnetOntology.getNoun(form.name)
        s"(I can't ${verb} form '${noun}'.)"
      }
    }
  }

  private val executor = new SmcExecutor[SpcEntity] {
    override def executeImperative(
      predicate : SilPredicate,
      resultCollector : SmcResultCollector[SpcEntity]) : Option[String] =
    {
      predicate match {
        case SilActionPredicate(
          _,
          SilSimpleWord(_,
            verb @ ("show" | "display" | "define" | "sketch"),
            _
          ),
          Some(objRef : SilAnnotatedReference), _
        ) => {
          resultCollector.refMap.get(objRef) match {
            case Some(targetEntitySet) if (targetEntitySet.nonEmpty) => {
              val string = targetEntitySet.map(entity => {
                formVerbUrl(verb, entity.form)
              }).mkString("\n")
              Some(string)
            }
            case _ => {
              val annotator = resultCollector.
                asInstanceOf[SpcResultCollector].spcAnnotator
              annotator.getNote(objRef).maybeForm match {
                case Some(form) => {
                  Some(formVerbUrl(verb, form))
                }
                case _ => {
                  Some("Nothing to show.")
                }
              }
            }
          }
        }
        case _ => {
          Some("I don't know how.")
        }
      }
    }
  }

  private val responder = new SpcResponder(
    mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS), params,
    executor,
    SmcCommunicationContext(
      mind.getTongue,
      Some(entityInterviewer),
      Some(entityShlurd)
    )
  )

  def run() : Unit =
  {
    mind.startConversation()
    var exit = false
    terminal.emitControl("")
    while (!exit) {
      terminal.beforeCommand(mind)
      terminal.emitPrompt()
      terminal.readCommand match {
        case Some(input) => {
          val parseResults = mind.newParser(input).parseAll
          parseResults.foreach(parseResult => {
            val output = responder.process(parseResult).text
            terminal.emitControl("")
            terminal.emitResponse(output)
            terminal.emitControl("")
          })
        }
        case _ => {
          exit = true
        }
      }
      terminal.afterCommand(mind)
    }
    terminal.emitControl("")
    terminal.emitControl("Shutting down...")
    // don't serialize conversation since that could be an extra source of
    // deserialization problems later
    mind.stopConversation()
  }
}
