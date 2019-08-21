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
package com.lingeringsocket.shlurd.doc

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import mdoc._
import mdoc.internal.cli._
import metaconfig.Configured

import scala.collection._
import scala.meta._
import scala.meta.internal.io._
import scala.io.Source

import java.nio.file._

object ExampleModifier
{
  type NumberedLine = (String, Int)

  case class MalformedInputException(line : String, Index : Int)
      extends RuntimeException
  {
  }

  val prompt = "> "

  val outputs = new mutable.HashSet[Path]

  var dir : Option[Path] = None

  var lastCosmos : Option[SpcCosmos] = None

  var beliefParams : Option[SpcBeliefParams] = None
}



class ConversationProcessor extends StringModifier
{
  import ExampleModifier._

  override val name = "processConversation"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    val lines = Source.fromString(code.text).getLines.toSeq.zipWithIndex

    val cosmos = lastCosmos.get.fork(true)
    val mind = new SpcMind(cosmos)
    val allowImplicits = info.contains("allowImplicits")
    val preventImplicits = info.contains("preventImplicits")
    def chooseImplicit(defaultSetting : Boolean) =
    {
      if (allowImplicits) {
        true
      } else if (preventImplicits) {
        false
      } else {
        defaultSetting
      }
    }
    val responder =
      new SpcResponder(
        mind,
        SpcBeliefParams(
          ACCEPT_NEW_BELIEFS,
          createImplicitIdeals = chooseImplicit(false),
          createTentativeIdeals = chooseImplicit(false),
          createTentativeEntities = chooseImplicit(true),
          createImplicitProperties = chooseImplicit(false)),
        SmcResponseParams(verbosity = RESPONSE_TERSE))
    val exchanges = {
      try {
        extractExchanges(lines)
      } catch {
        case MalformedInputException(line, index) => {
          val position = Position.Range(code, index, 0, index, 0)
          reporter.error(
            position,
            s"\nUNEXPECTED:\n")
          Seq.empty
        }
      }
    }
    val result = exchanges.map {
      case ((input, inputIndex), expectedLines) => {
        val sentence = responder.newParser(input).parseOne
        val response = responder.process(sentence, input)
        val responseLines = Source.fromString(response).getLines.toSeq.filterNot(
          SprParser.isIgnorableLine)
        val iMismatch = range(0 until expectedLines.size).find(index => {
          if (index < responseLines.size) {
            expectedLines(index)._1 != responseLines(index)
          } else {
            true
          }
        })
        iMismatch.foreach(index => {
          val (expected, startLine) = expectedLines(index)
          val actual = {
            if (index < responseLines.size) {
              responseLines(index)
            } else {
              "(response ended prematurely)"
            }
          }
          val position = Position.Range(code, startLine, 0, startLine, 0)
          reporter.error(
            position,
            s"\nMISMATCH:\n\n$actual\n\nEXPECTED:\n")
        })
        if (responseLines.size > expectedLines.size) {
          val actual = responseLines(expectedLines.size)
          val startLine = {
            if (expectedLines.isEmpty) {
              inputIndex
            } else {
              expectedLines.last._2
            }
          }
          val position = Position.Range(code, startLine, 0, startLine, 0)
          reporter.error(
            position,
            s"\nUNEXPECTED:\n\n$actual\n")
        }
        s"$prompt$input\n\n$response"
      }
    }.mkString("\n\n")
    s"```\n$result\n```"
  }

  private def extractExchanges(
    lines : Seq[NumberedLine]
  ) : Seq[(NumberedLine, Seq[NumberedLine])] =
  {
    if (lines.isEmpty) {
      Seq.empty
    } else {
      val (first, rest) = chomp(lines)
      first +: extractExchanges(rest)
    }
  }

  private def chomp(
    lines : Seq[NumberedLine]
  ) : ((NumberedLine, Seq[NumberedLine]), Seq[NumberedLine]) =
  {
    assert(!lines.isEmpty)
    val (line, index) = lines.head
    if (SprParser.isIgnorableLine(line)) {
      chomp(lines.tail)
    } else if (line.startsWith(prompt)) {
      var iNext = lines.indexWhere(_._1.startsWith(prompt), 1)
      if (iNext == -1) {
        iNext = lines.size
      }
      tupleN((
        tupleN((
          tupleN((line.stripPrefix(prompt), index)),
          lines.slice(1, iNext).filterNot(
            line => SprParser.isIgnorableLine(line._1)))),
        lines.drop(iNext)
      ))
    } else {
      throw MalformedInputException(line, index)
    }
  }
}

class BeliefRenderer extends StringModifier
{
  import ExampleModifier._

  override val name = "renderBelief"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    val input = code.text

    val cosmos = new SpcCosmos
    lastCosmos = Some(cosmos)
    SpcPrimordial.initCosmos(cosmos)
    val mind = new SpcMind(cosmos)
    val responder = new SpcResponder(
      mind,
      SpcBeliefParams(
        ACCEPT_NEW_BELIEFS,
        createImplicitIdeals = true,
        createTentativeIdeals = false,
        createTentativeEntities = false,
        createImplicitProperties = false)
    )

    mind.loadBeliefs(
      ResourceUtils.getResourceSource(
        "/ontologies/examples.txt"),
        responder)

    val source = Source.fromString(input)
    val beliefs = source.getLines.
      filterNot(SprParser.isIgnorableLine).mkString("\n")
    val sentences = responder.newParser(beliefs).parseAllPositional
    val ok = responder.sentencePrinter.sb.respondCompliance
    sentences.foreach {
      case(sentence, start, end) => {
        val output = try {
          responder.process(sentence)
        } catch {
          case ex : Throwable => {
            ex.printStackTrace
            ex.toString
          }
        }
        if (output != ok) {
          // FIXME this isn't quite right since whenever
          // we stripped ignorable lines above
          val position = Position.Range(code, end, end)
          reporter.error(
            position,
            s"\nERROR:\n\n$output\n\nAT:\n")
        }
      }
    }
    cosmos.validateBeliefs

    val visualizer = new SpcGraphVisualizer(
      cosmos.getGraph,
      SpcGraphVisualizer.fullOptions)
    val relpath = Paths.get(info)
    val out = dir.get.resolve(relpath)
    assert(!outputs.contains(out), out)
    outputs += out
    Files.createDirectories(out.getParent)
    visualizer.renderToImageFile(out.toFile)
    s"```\n$input\n```\n\n![diagram]($info)"
  }
}

object MdocMain extends App
{
  // build arguments for mdoc
  ExampleModifier.dir = Settings.fromCliArgs(
    args.toList, Settings.default(AbsolutePath(PathIO.workingDirectory.toNIO))
  ) match {
    case Configured.Ok(setting) => Some(setting.out.toNIO)
    case _ => None
  }
  val settings = mdoc.MainSettings()
    .withSiteVariables(immutable.Map("VERSION" -> "1.0.0"))
    .withArgs(args.toList)
  val exitCode = mdoc.Main.process(settings)
  if (exitCode != 0) {
    sys.exit(exitCode)
  }
}
