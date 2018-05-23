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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import org.specs2.mutable._
class ShlurdPlatonicCreedSpec extends Specification
{
  trait WorldContext extends NameSpace
  {
    protected val world = new ShlurdPlatonicWorld

    protected val creed = new ShlurdPlatonicCreed(world)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      world.addBelief(sentence)
    }

    protected def expectPreserved(
      input : Iterable[String]) =
    {
      expectNormalized(input, input)
    }

    protected def expectNormalized(
      input : Iterable[String], expected : Iterable[String]) =
    {
      input.foreach(addBelief)
      val printer = new ShlurdSentencePrinter
      val beliefs = creed.allBeliefs.map(printer.print)
      beliefs.map(ShlurdParseUtils.capitalize) must be equalTo expected
    }
  }

  private val stateMust = "A door must be open or closed."
  private val stateMay = "A window may be open or closed."
  private val stateAlias = "A lit light is on."
  private val stateNormalization = "A person at home is present."
  private val formSynonym = "A mentor is a person."
  private val assocHas = "A dog has an owner."
  private val assocMust = "A dog must have an owner."
  private val assocMay = "A person may have a mentor."
  private val assocMayPlural = "A person may have pets."
  private val assocMayProperty = "A person may have a presence as a property."
  private val entityExists = "There is a parakeet."
  private val namedEntityExists = "Fido is a dog."
  private val entityQualifiedExists = "There is an angry cat."
  private val personExists = "Yoda is a person."
  private val personExists2 = "Luke is a person."
  private val personAssoc = "Yoda is Luke's mentor."

  "ShlurdPlatonicCreed" should
  {
    "preserve states" in new WorldContext
    {
      expectPreserved(Seq(stateMust, stateMay))
    }

    "preserve state alias" in new WorldContext
    {
      expectPreserved(Seq(stateAlias))
    }

    "preserve state normalizations" in new WorldContext
    {
      expectPreserved(Seq(stateNormalization))
    }

    "preserve form synonyms" in new WorldContext
    {
      expectPreserved(Seq(formSynonym))
    }

    "preserve form associations" in new WorldContext
    {
      expectPreserved(Seq(
        assocMust, assocMay, assocMayPlural, assocMayProperty))
    }

    "normalize form associations" in new WorldContext
    {
      expectNormalized(Seq(assocHas), Seq(assocMust))
    }

    "preserve entity existence" in new WorldContext
    {
      expectPreserved(Seq(
        entityExists, entityQualifiedExists, personExists))
    }

    "preserve named entity existence" in new WorldContext
    {
      skipped("need to handle named non-persons")
      expectPreserved(Seq(namedEntityExists))
    }

    "preserve entity associations" in new WorldContext
    {
      expectPreserved(Seq(
        formSynonym, assocMay,
        personExists, personExists2, personAssoc))
    }
  }
}
