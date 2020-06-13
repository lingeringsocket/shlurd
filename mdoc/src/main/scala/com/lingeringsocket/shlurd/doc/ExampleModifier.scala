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
import com.lingeringsocket.shlurd.cli._
import com.lingeringsocket.phlebotinum._

import mdoc._
import mdoc.internal.cli._
import metaconfig.Configured

import scala.collection._
import scala.meta._
import scala.meta.internal.io._
import scala.io.Source

import java.net._
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

  def reportErrorLocation(
    reporter : Reporter,
    code : Input,
    startLine : Int,
    actual : String,
    expected : String)
  {
    val position = Position.Range(code, startLine, 0, startLine, 0)
    reporter.error(
      position,
      s"\nMISMATCH:\n\n$actual\n\nEXPECTED:$expected\n")
  }

  def getOutputLocation(info : String) =
  {
    val relpath = Paths.get(info)
    val out = dir.get.resolve(relpath)
    assert(!outputs.contains(out), out)
    outputs += out
    Files.createDirectories(out.getParent)
    out
  }
}

object CosmosModifier
{
  var lastCosmos : Option[SpcCosmos] = None
}

object PhlebTrc
{
  def run(terminal : PhlebTerminal)
  {
    // preload
    PhlebBaseline.frozenCosmos

    ResourceUtils.addUrl(new URL(
      "https://raw.githubusercontent.com/lingeringsocket/" +
        "hello-phlebotinum/master/"))
    PhlebShell.run("/", terminal)
  }
}

abstract class PhlebAbstractProcessor extends StringModifier
{
  import ExampleModifier._

  class DocTerminal(
    info : String, code : Input, reporter : Reporter,
    expectOutput : Boolean
  ) extends PhlebTerminal
  {
    private val script = Source.fromString(code.text).getLines.zipWithIndex

    private var lastLineNo = 0

    private var skipIntro = (info == "skipIntro")

    override def emitNarrative(msg : String)
    {
      super.emitNarrative(msg)
      if (!skipIntro && expectOutput && !msg.isEmpty) {
        nextScriptLine match {
          case Some((expected, lineNo)) => {
            if (msg != expected) {
              reportErrorLocation(reporter, code, lineNo, msg, expected)
            }
          }
          case _ => {
            reportErrorLocation(reporter, code, lastLineNo, msg, "EOF")
          }
        }
      }
    }

    override def emitVisualization(visualizer : SpcGraphVisualizer)
    {
      if (info.isEmpty) {
        reportErrorLocation(
          reporter, code, lastLineNo, "visualize", "output location required")
      }
      val out = getOutputLocation(info)
      visualizer.renderToImageFile(out.toFile)
    }

    override def readInput() : Option[String] =
    {
      skipIntro = false
      nextScriptLine match {
        case Some((cmd, lineNo)) => {
          val expected = prompt
          if (!cmd.startsWith(expected)) {
            reportErrorLocation(reporter, code, lineNo, cmd, expected)
          }
          Some(cmd.stripPrefix(expected))
        }
        case _ => None
      }
    }

    override def getInitSaveFile() : String =
    {
      "trc/init-save.zip"
    }

    def nextScriptLine() : Option[(String, Int)] = {
      if (!script.hasNext) {
        None
      } else {
        val s = script.next
        lastLineNo = s._2
        if (SprParser.isIgnorableLine(s._1)) {
          nextScriptLine
        } else {
          Some(s)
        }
      }
    }
  }

  protected def processImpl(
    info : String,
    code : Input,
    reporter : Reporter,
    expectOutput : Boolean)
  {
    val terminal = new DocTerminal(info, code, reporter, expectOutput)
    PhlebTrc.run(terminal)
    terminal.nextScriptLine matchPartial {
      case Some((cmd, lineNo)) => {
        reportErrorLocation(reporter, code, lineNo, cmd, "EOF")
      }
    }
  }
}

class PhlebProcessor extends PhlebAbstractProcessor
{
  override val name = "processPhleb"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    processImpl(info, code, reporter, true)
    s"```\n${code.text}\n```"
  }
}

class PhlebRenderer extends PhlebAbstractProcessor
{
  override val name = "renderPhleb"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    processImpl(info, code, reporter, false)
    s"[![diagram]($info)]($info)"
  }
}

class ConversationProcessor extends StringModifier
{
  import ExampleModifier._
  import CosmosModifier._

  override val name = "processConversation"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    val lines = Source.fromString(code.text).getLines.toSeq.zipWithIndex

    val wordnet = info.contains("wordnet")
    val cosmos = {
      if (wordnet) {
        ShlurdPrimordialWordnet.newMutableCosmos
      } else {
        lastCosmos.get.fork(true)
      }
    }
    val mind = {
      if (info.contains("wordnet")) {
        new SpcWordnetMind(cosmos)
      } else {
        new SpcMind(cosmos)
      }
    }
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
    val verbosity = {
      if (info.contains("verbose")) {
        RESPONSE_COMPLETE
      } else {
        RESPONSE_TERSE
      }
    }
    val beliefAcceptance = {
      if (info.contains("preventNewBeliefs")) {
        IGNORE_BELIEFS
      } else if (info.contains("acceptModifiedBeliefs")) {
        ACCEPT_MODIFIED_BELIEFS
      } else {
        ACCEPT_NEW_BELIEFS
      }
    }
    mind.startConversation
    val responder =
      new SpcResponder(
        mind,
        SpcBeliefParams(
          beliefAcceptance,
          createImplicitIdeals = chooseImplicit(false),
          createTentativeIdeals = chooseImplicit(false),
          createTentativeEntities = chooseImplicit(true),
          createImplicitProperties = chooseImplicit(false)),
        SmcResponseParams(verbosity = verbosity))
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
        val parseResult = responder.newParser(input).parseOne
        val response = responder.process(parseResult, input)
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
          reportErrorLocation(reporter, code, startLine, actual, "")
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
    mind.stopConversation
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
  import CosmosModifier._

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
    val results = responder.newParser(beliefs).parseAll
    val ok = responder.sentencePrinter.sb.respondCompliance
    results.foreach {
      case pr @ SprParseResult(sentence, _, start, end) => {
        val output = try {
          responder.process(pr)
        } catch {
          case ex : Throwable => {
            ex.printStackTrace
            ex.toString
          }
        }
        if (output != ok) {
          // FIXME this isn't quite right whenever
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
      SpcGraphVisualizationOptions(
        includeEntities = true,
        includeTaxonomy = true, includeRealizations = true,
        includeFormAssocs = true, includeEntityAssocs = true,
        includeInverses = true, includeSynonyms = true,
        includeProperties = true, includeEntityProperties = true
      )
    )
    val out = getOutputLocation(info)
    visualizer.renderToImageFile(out.toFile)
    s"```\n$input\n```\n\n[![diagram]($info)]($info)"
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
