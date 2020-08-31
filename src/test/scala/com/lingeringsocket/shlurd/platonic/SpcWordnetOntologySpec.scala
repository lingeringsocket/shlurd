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

import com.lingeringsocket.shlurd._

import org.specs2.mutable._
import org.specs2.specification._

import net.sf.extjwnl.data._

class SpcWordnetOntologySpec extends Specification
{
  trait CosmosContext extends Scope
  {
    protected val cosmos = new SpcCosmos

    protected val ontology = new SpcWordnetOntology(
      ShlurdPrincetonWordnet, cosmos)

    protected val graph = cosmos.getGraph
  }

  private def expectUniqueSense(lemma : String) : Synset =
  {
    val senses = ShlurdPrincetonWordnet.getNounSenses(lemma)
    senses.size must be equalTo 1
    senses.head
  }

  private def expectForm(formOpt : Option[SpcForm]) : SpcForm =
  {
    formOpt must beSome
    formOpt.get
  }

  "SpcWordnetOntology" should
  {
    "load form" in new CosmosContext
    {
      val sense = expectUniqueSense("aunt")
      val form = expectForm(ontology.loadForm(sense))
      form.name must be equalTo "wnf-aunt-1"
      cosmos.resolveForm(form.name) must beSome(form)
    }

    "load direct hypernyms" in new CosmosContext
    {
      val auntSense = expectUniqueSense("aunt")
      val kinswomanSense = expectUniqueSense("kinswoman")
      val hypernyms = ontology.loadDirectHypernyms(auntSense, false)
      val auntForm = expectForm(ontology.getSynsetForm(auntSense))
      val kinswomanForm = expectForm(ontology.getSynsetForm(kinswomanSense))
      hypernyms must be equalTo Seq(kinswomanForm)
      cosmos.isHyponym(auntForm, kinswomanForm) must beTrue
      cosmos.isHyponym(kinswomanForm, auntForm) must beFalse
    }

    "load meronym associations" in new CosmosContext
    {
      val sense = ShlurdPrincetonWordnet.getNounSenses("fork").head
      val meronyms = ontology.loadMeronyms(sense)
      val forkForm = expectForm(ontology.getSynsetForm(sense))
      val prongForm = expectForm(ontology.getSynsetForm(
        expectUniqueSense("prong")))
      val tineForm = expectForm(ontology.getSynsetForm(
        expectUniqueSense("tine")))
      meronyms.map(_.name) must be equalTo(
        Seq(
          "wnr-prong-1",
          "wnr-tine-1"))
      meronyms.map(graph.getFormsForRole) must be equalTo(
        Seq(Seq(prongForm), Seq(tineForm)))
      val prongRole = meronyms.head
      val tineRole = meronyms.last
      graph.getFormAssocEdge(forkForm, prongRole) must beSome
      graph.getFormAssocEdge(forkForm, tineRole) must beSome
    }

    "load bidirectional meronym associations" in new CosmosContext
    {
      val countrySense =
        ShlurdPrincetonWordnet.getNounSenses("country").tail.head
      val provinceSense =
        ShlurdPrincetonWordnet.getNounSenses("province").head
      val meronyms = ontology.loadMeronyms(countrySense)
      val countryForm = expectForm(ontology.getSynsetForm(countrySense))
      val provinceForm = expectForm(ontology.getSynsetForm(provinceSense))
      val roleOpt = meronyms.map(
        role => tupleN(((role, graph.getFormsForRole(role))))).find(
          _._2.exists(_ == provinceForm)).map(_._1).headOption
      roleOpt must beSome
      val role = roleOpt.get
      val edgeOpt = graph.getFormAssocEdge(countryForm, role)
      edgeOpt must beSome
      val edge = edgeOpt.get
      edge.constraint must be equalTo
        SpcCardinalityConstraint(0, Int.MaxValue)
      val inverseOpt = cosmos.getInverseAssocEdge(edge)
      inverseOpt must beSome
      val inverseEdge = inverseOpt.get
      inverseEdge.constraint must be equalTo
        SpcCardinalityConstraint(0, 1)
    }
  }
}
