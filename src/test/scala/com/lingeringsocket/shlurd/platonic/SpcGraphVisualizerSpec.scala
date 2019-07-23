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
  def formAttributes(name : String) =
    s"""[ label="$name" shape="box" ]"""

  def roleAttributes(name : String) =
    s"""[ label="$name" shape="hexagon" ]"""

  def entityAttributes(name : String) =
    s"""[ label="$name" shape="ellipse" ]"""

  val taxonomyAttributes =
    """[ label="isKindOf" arrowhead="empty" ]"""

  val roleTaxonomyAttributes =
    """[ label="mustBeA" arrowhead="empty" ]"""

  val realizationAttributes =
    """[ label="isA" arrowhead="empty" style="dashed" ]"""

  val formAssocAttributes =
    s"""[ label="has (1)" arrowhead="open" ]"""

  def entityAssocAttributes(roleName : String) =
    s"""[ label="$roleName" arrowhead="open" ]"""
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
      addBelief("a powerpuff's teacher must be a professor")
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
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=LR;
          1 ${formAttributes("girl")};
          2 ${formAttributes("powerpuff")};
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
          1 ${formAttributes("powerpuff")};
          2 ${formAttributes("girl")};
          1->2 $taxonomyAttributes;
        }
      """).ignoreSpace
    }

    "visualize entities" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeEntities = true)
    ) {
      definePowerpuffs
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=LR;
          1 ${entityAttributes("Bubblossom")};
          2 ${entityAttributes("Buttercup")};
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
          1 ${entityAttributes("Bubblossom")};
          2 ${formAttributes("powerpuff")};
          3 ${entityAttributes("Buttercup")};
          1->2 $realizationAttributes;
          3->2 $realizationAttributes;
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
          1 ${formAttributes("powerpuff")};
          2 ${formAttributes("girl")};
          3 ${entityAttributes("Bubblossom")};
          4 ${entityAttributes("Buttercup")};
          1->2 $taxonomyAttributes;
          3->1 $realizationAttributes;
          4->1 $realizationAttributes;
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
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=LR;
          1 ${formAttributes("powerpuff")};
          2 ${roleAttributes("teacher")};
          1->2 $formAssocAttributes;
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
          1 ${formAttributes("powerpuff")};
          2 ${formAttributes("girl")};
          3 ${formAttributes("professor")};
          4 ${formAttributes("boy")};
          5 ${roleAttributes("teacher")};
          1->2 $taxonomyAttributes;
          3->4 $taxonomyAttributes;
          5->3 $roleTaxonomyAttributes;
          1->5 $formAssocAttributes;
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
      renderedString.size must be equalTo 813
      renderedString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 ${formAttributes("girl")};
          2 ${formAttributes("powerpuff")};
          3 ${formAttributes("boy")};
          4 ${formAttributes("professor")};
          5 ${roleAttributes("teacher")};
          6 ${entityAttributes("Bubblossom")};
          7 ${entityAttributes("Buttercup")};
          8 ${entityAttributes("Utonium")};
          2->1 $taxonomyAttributes;
          4->3 $taxonomyAttributes;
          5->4 $roleTaxonomyAttributes;
          6->2 $realizationAttributes;
          7->2 $realizationAttributes;
          8->4 $realizationAttributes;
          2->5 $formAssocAttributes;
          6->8 ${entityAssocAttributes("teacher")};
          7->8 ${entityAssocAttributes("teacher")};
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
      renderedString.size must be equalTo 13232
    }
  }
}
