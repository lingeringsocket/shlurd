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
package com.lingeringsocket.shlurd.mdoc

import com.lingeringsocket.shlurd.platonic._

import mdoc._
import mdoc.internal.cli._
import metaconfig.Configured

import scala.meta._
import scala.meta.internal.io._
import scala.io.Source

import java.nio.file._

object BeliefRenderer
{
  private[mdoc] var dir : Option[Path] = None
}

class BeliefRenderer extends StringModifier
{
  import BeliefRenderer._

  override val name = "renderBelief"

  override def process(
    info : String,
    code : Input,
    reporter : Reporter) : String =
  {
    val input = code.text

    val cosmos = new SpcCosmos
    SpcPrimordial.initCosmos(cosmos)

    val mind = new SpcMind(cosmos)

    /*
    val responder =
      new SpcResponder(
        mind,
        SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_TERSE))
    val sentence = responder.newParser(input).parseOne
    val response = responder.process(sentence, input)
    s"```\n> $input\n\n$response\n```"
     */

    mind.loadBeliefs(Source.fromString(input))

    val visualizer = new SpcGraphVisualizer(
      cosmos.getGraph,
      SpcGraphVisualizer.fullOptions)
    val relpath = Paths.get(info)
    val out = dir.get.resolve(relpath)
    Files.createDirectories(out.getParent)
    visualizer.renderToImageFile(out.toFile)
    s"```\n$input\n```\n\n![diagram]($info)"
  }
}

object MdocMain extends App
{
  // build arguments for mdoc
  BeliefRenderer.dir = Settings.fromCliArgs(
    args.toList, Settings.default(AbsolutePath(PathIO.workingDirectory.toNIO))
  ) match {
    case Configured.Ok(setting) => Some(setting.out.toNIO)
    case _ => None
  }
  val settings = mdoc.MainSettings()
    .withSiteVariables(Map("VERSION" -> "1.0.0"))
    .withArgs(args.toList)
  val exitCode = mdoc.Main.process(settings)
  if (exitCode != 0) {
    sys.exit(exitCode)
  }
}
