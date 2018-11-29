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
package com.lingeringsocket.shlurd.parser

import scala.collection.JavaConverters._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.io._

import java.io._

object SprPhraseGraph
{
  type SprPhraseVertex = SprSyntaxTree

  type SprPhraseEdge = DefaultEdge

  type SprPhraseGraph = Graph[SprPhraseVertex, SprPhraseEdge]

  def apply() : SprPhraseGraph =
  {
    new SimpleDirectedGraph[SprPhraseVertex, SprPhraseEdge](
      classOf[SprPhraseEdge]
    )
  }

  def render(graph : SprPhraseGraph, accepted : Set[SprPhraseVertex]) =
  {
    val bold : Attribute =
      new DefaultAttribute[String]("bold", AttributeType.STRING)
    val exporter = new DOTExporter[SprPhraseVertex, SprPhraseEdge](
      new IntegerComponentNameProvider,
      new ComponentNameProvider[SprPhraseVertex]{
        override def getName(vertex : SprPhraseVertex) = {
          vertex.label
        }
      },
      null,
      new ComponentAttributeProvider[SprPhraseVertex]{
        override def getComponentAttributes(vertex : SprPhraseVertex) =
        {
          if (accepted.contains(vertex)) {
            Map("style" -> bold)
          } else {
            Map[String, Attribute]()
          }
        }.asJava
      },
      null)
    val sw = new StringWriter
    exporter.exportGraph(graph, sw)
    sw.toString
  }
}
