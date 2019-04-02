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

import scala.collection._
import scala.collection.JavaConverters._

import org.jgrapht.graph._
import org.jgrapht.io._

import java.io._

object SprPhraseGraph
{
  class SprPhraseVertex(val tree : SprSyntaxTree)
  {
  }

  type SprPhraseEdge = DefaultEdge

  def apply() : SprPhraseGraph =
  {
    new SprPhraseGraph()
  }
}

class SprPhraseGraph
    extends
    SimpleDirectedGraph[SprPhraseGraph.SprPhraseVertex,
      SprPhraseGraph.SprPhraseEdge](
  classOf[SprPhraseGraph.SprPhraseEdge])
{
  import SprPhraseGraph._

  private val map = new mutable.HashMap[SprSyntaxTree, SprPhraseVertex] {
    override protected def elemEquals(
      key1 : SprSyntaxTree, key2 : SprSyntaxTree) : Boolean =
    {
      (key1 eq key2) ||
        ((key1 == key2) && !key1.isLeaf && !key1.isPreTerminal)
    }

    override protected def elemHashCode(
      key : SprSyntaxTree) =
    {
      if (key.isLeaf || key.isPreTerminal) {
        System.identityHashCode(key)
      } else {
        key.hashCode
      }
    }
  }

  def vertexFor(tree : SprSyntaxTree) : SprPhraseVertex =
  {
    map.getOrElseUpdate(tree, {
      val vertex = new SprPhraseVertex(tree)
      addVertex(vertex)
      vertex
    })
  }

  def addPhrase(tree : SprSyntaxTree)
  {
    val newVertex = vertexFor(tree)
    tree.children.foreach(term => {
      val termVertex = vertexFor(term)
      addEdge(newVertex, termVertex)
    })
  }


  def render(accepted : Set[SprSyntaxTree]) =
  {
    val bold : Attribute =
      new DefaultAttribute[String]("bold", AttributeType.STRING)
    val exporter = new DOTExporter[SprPhraseVertex, SprPhraseEdge](
      new IntegerComponentNameProvider,
      new ComponentNameProvider[SprPhraseVertex]{
        override def getName(vertex : SprPhraseVertex) = {
          vertex.tree.label
        }
      },
      null,
      new ComponentAttributeProvider[SprPhraseVertex]{
        override def getComponentAttributes(vertex : SprPhraseVertex) =
        {
          if (accepted.contains(vertex.tree)) {
            Map("style" -> bold)
          } else {
            Map[String, Attribute]()
          }
        }.asJava
      },
      null)
    val sw = new StringWriter
    exporter.putGraphAttribute("ordering", "out")
    exporter.exportGraph(this, sw)
    sw.toString
  }
}
