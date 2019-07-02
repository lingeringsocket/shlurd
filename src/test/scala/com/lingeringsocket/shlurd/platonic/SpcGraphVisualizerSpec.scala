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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd.mind._

object SpcGraphVisualizerSpec
{
  val taxonomyAttributes =
    """[ label="isKindOf" arrowhead="empty" ]"""

  val realizationAttributes =
    """[ label="isA" arrowhead="empty" style="dashed" ]"""
}

class SpcGraphVisualizerSpec extends SpcProcessingSpecification
{
  import SpcGraphVisualizerSpec._

  abstract class VisualizationContext(
    options : SpcGraphVisualizationOptions
  ) extends ProcessingContext
  {
    protected def addBelief(input : String) =
    {
      val result = process(
        input,
        ACCEPT_NEW_BELIEFS,
        SmcResponseParams(throwRejectedBeliefs = true))
      result must be equalTo "OK."
    }

    protected def definePowerpuffs()
    {
      addBelief("a powerpuff is a kind of girl")
      addBelief("Bubblossom is a powerpuff")
      addBelief("Buttercup is a powerpuff")
    }

    protected def defineProfessor()
    {
      addBelief("a professor is a kind of boy")
      addBelief("Utonium is a professor")
    }

    protected def defineTeacher()
    {
      addBelief("a teacher must be a professor")
      addBelief("a powerpuff must have a teacher")
    }

    protected def defineStudents()
    {
      addBelief("Utonium is Bubblossom's teacher")
      addBelief("Utonium is Buttercup's teacher")
    }

    protected def renderToString() : String =
    {
      val visualizer = new SpcGraphVisualizer(cosmos.getGraph, options)
      visualizer.renderToString()
    }

    protected def displayGraph()
    {
      val visualizer = new SpcGraphVisualizer(cosmos.getGraph, options)
      visualizer.display
    }
  }

  "SpcGraphVisualizer" should
  {
    "visualize ideals" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeIdeals = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo("""
        strict digraph G {
          rankdir=LR;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
        }
      """).ignoreSpace
    }

    "visualize taxonomy" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeTaxonomy = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          2->1 $taxonomyAttributes;
        }
      """).ignoreSpace
    }

    "visualize entities" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeEntities = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo("""
        strict digraph G {
          rankdir=LR;
          1 [ label="SpcEntity(Bubblossom)" ];
          2 [ label="SpcEntity(Buttercup)" ];
        }
      """).ignoreSpace
    }

    "visualize entities with realizations" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeRealizations = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          3 [ label="SpcEntity(Bubblossom)" ];
          4 [ label="SpcEntity(Buttercup)" ];
          3->2 $realizationAttributes;
          4->2 $realizationAttributes;
        }
      """).ignoreSpace
    }

    "visualize entities with taxonomy and realizations" in new
      VisualizationContext(SpcGraphVisualizationOptions(
        includeRealizations = true, includeTaxonomy = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          3 [ label="SpcEntity(Bubblossom)" ];
          4 [ label="SpcEntity(Buttercup)" ];
          2->1 $taxonomyAttributes;
          3->2 $realizationAttributes;
          4->2 $realizationAttributes;
        }
      """).ignoreSpace
    }

    "visualize form associations" in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeFormAssocs = true)
    ) {
      definePowerpuffs
      defineProfessor
      defineTeacher
      renderToString must beEqualTo("""
        strict digraph G {
          rankdir=LR;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          3 [ label="SpcForm(boy)" ];
          4 [ label="SpcForm(professor)" ];
          5 [ label="SpcRole(teacher)" ];
          2->5 [ label="teacher" ];
        }
      """).ignoreSpace
    }

    "visualize form associations with taxonomy" in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeTaxonomy = true, includeFormAssocs = true)
    ) {
      definePowerpuffs
      defineProfessor
      defineTeacher
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          3 [ label="SpcForm(boy)" ];
          4 [ label="SpcForm(professor)" ];
          5 [ label="SpcRole(teacher)" ];
          2->1 $taxonomyAttributes;
          4->3 $taxonomyAttributes;
          5->4 $taxonomyAttributes;
          2->5 [ label="teacher" ];
        }
      """).ignoreSpace
    }

    "visualize the whole shebang" in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeIdeals = true,
        includeEntities = true, includeRealizations = true,
        includeTaxonomy = true, includeFormAssocs = true,
        includeEntityAssocs = true)
    ) {
      // verify that includeMeta=false hides all of this
      SpcPrimordial.initCosmos(cosmos)
      definePowerpuffs
      defineProfessor
      defineTeacher
      defineStudents
      val renderedString = renderToString
      renderedString.size must be equalTo 729
      renderedString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 [ label="SpcForm(girl)" ];
          2 [ label="SpcForm(powerpuff)" ];
          3 [ label="SpcForm(boy)" ];
          4 [ label="SpcForm(professor)" ];
          5 [ label="SpcRole(teacher)" ];
          6 [ label="SpcEntity(Bubblossom)" ];
          7 [ label="SpcEntity(Buttercup)" ];
          8 [ label="SpcEntity(Utonium)" ];
          2->1 $taxonomyAttributes;
          4->3 $taxonomyAttributes;
          5->4 $taxonomyAttributes;
          6->2 $realizationAttributes;
          7->2 $realizationAttributes;
          8->4 $realizationAttributes;
          2->5 [ label="teacher" ];
          6->8 [ label="teacher" ];
          7->8 [ label="teacher" ];
        }
      """).ignoreSpace
    }

    "include meta"  in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeIdeals = true,
        includeEntities = true, includeRealizations = true,
        includeTaxonomy = true, includeFormAssocs = true,
        includeEntityAssocs = true, includeMeta = true)
    ) {
      SpcPrimordial.initCosmos(cosmos)
      definePowerpuffs
      defineProfessor
      defineTeacher
      defineStudents
      val renderedString = renderToString
      renderedString.size must be equalTo 10692
    }
  }
}
