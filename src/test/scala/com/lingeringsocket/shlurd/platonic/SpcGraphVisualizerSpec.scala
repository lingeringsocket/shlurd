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
    s"""[ label="$name" shape="box" style="bold" ]"""

  def formPropertyAttributes(name : String) =
    s"""[ label="$name" shape="record" style="bold" ]"""

  def roleAttributes(name : String) =
    s"""[ label="$name" shape="hexagon" style="bold" ]"""

  def entityAttributes(name : String) =
    s"""[ label="$name" shape="Mrecord" ]"""

  def synonymAttributes(name : String) =
    s"""[ label="$name" shape="pentagon" style="bold" ]"""

  val taxonomyAttributes =
    """[ label="isKindOf" arrowhead="empty" style="bold" ]"""

  val roleTaxonomyAttributes =
    """[ label="mustBeA" arrowhead="empty" style="bold" ]"""

  val realizationAttributes =
    """[ label="isA" arrowhead="empty" style="dashed" ]"""

  val synonymEdgeAttributes =
    """[ label="isSynonymFor" arrowhead="open" style="bold" ]"""

  val formPropertyEdgeAttributes =
    """[ label="hasProperty" arrowhead="box" style="bold" ]"""

  def formAssocAttributes(constraint : String) =
    s"""[ label="has ($constraint)" arrowhead="open" style="bold" ]"""

  val inverseAssocAttributes =
    s"""[ label="isInverseOf" dir="both" style="bold" ]"""

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

    protected def defineProperties()
    {
      addBelief("a girl's demeanor may be feisty or demure")
      addBelief("a powerpuff's demeanor must be feisty")
      addBelief("a powerpuff's awakeness may be asleep or awake")
      addBelief("Bubblossom is awake")
      addBelief("Buttercup is asleep")
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

    protected def defineInverse()
    {
      addBelief("a professor's student must be a powerpuff")
      addBelief("if a professor is a powerpuff's teacher, then equivalently " +
        "the powerpuff is the professor's student")
    }

    protected def defineStudents()
    {
      addBelief("Utonium is Bubblossom's teacher")
      addBelief("Utonium is Buttercup's teacher")
    }

    protected def defineSynonyms()
    {
      addBelief("a heroine is the same as a powerpuff")
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

    "visualize synonyms" in new VisualizationContext(
      SpcGraphVisualizationOptions(includeIdeals = true, includeSynonyms = true)
    ) {
      definePowerpuffs
      defineSynonyms
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=LR;
          1 ${formAttributes("girl")};
          2 ${formAttributes("powerpuff")};
          3 ${synonymAttributes("heroine")};
          3->2 $synonymEdgeAttributes;
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
          1->2 ${formAssocAttributes("1")};
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
          1->5 ${formAssocAttributes("1")};
        }
      """).ignoreSpace
    }

    "visualize inverse associations" in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeFormAssocs = true,
        includeInverses = true)
    ) {
      defineTeacher
      defineInverse
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=LR;
          1 ${roleAttributes("teacher")};
          2 ${roleAttributes("student")};
          3 ${formAttributes("powerpuff")};
          4 ${formAttributes("professor")};
          1->2 $inverseAssocAttributes;
          3->1 ${formAssocAttributes("1")};
          4->2 ${formAssocAttributes("0..*")};
        }
      """).ignoreSpace
    }

    "visualize properties" in new VisualizationContext(
      SpcGraphVisualizationOptions(
        includeIdeals = true, includeTaxonomy = true,
        includeRealizations = true, includeProperties = true)
    ) {
      definePowerpuffs
      defineProperties
      renderToString must beEqualTo(s"""
        strict digraph G {
          rankdir=BT;
          1 ${formAttributes("girl")};
          2 ${formPropertyAttributes("{demeanor|{feisty|demure|...}}")};
          3 ${formAttributes("powerpuff")};
          4 ${formPropertyAttributes("{demeanor|{feisty}}")};
          5 ${formPropertyAttributes("{awakeness|{asleep|awake|...}}")};
          6 ${entityAttributes("{Bubblossom|awakeness=awake}")};
          7 ${entityAttributes("{Buttercup|awakeness=asleep}")};
          1->2 $formPropertyEdgeAttributes;
          3->4 $formPropertyEdgeAttributes;
          3->5 $formPropertyEdgeAttributes;
          3->1 $taxonomyAttributes;
          6->3 $realizationAttributes;
          7->3 $realizationAttributes;
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
          2->5 ${formAssocAttributes("1")};
          6->8 ${entityAssocAttributes("teacher")};
          7->8 ${entityAssocAttributes("teacher")};
        }
      """).ignoreSpace
      renderedString.size must be equalTo 930
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
      renderedString.size must be equalTo 13853
    }
  }
}
