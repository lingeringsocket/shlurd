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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import org.specs2.mutable._
class SpcCreedSpec extends Specification
{
  trait CosmosContext extends NameSpace
  {
    protected val cosmos = new SpcCosmos

    private val refriedCosmos = new SpcCosmos

    protected val interpreter = new SpcBeliefInterpreter(cosmos)

    private val refriedInterpreter =
      new SpcBeliefInterpreter(refriedCosmos)

    protected val creed = new SpcCreed(cosmos)

    private val refriedCreed = new SpcCreed(refriedCosmos)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpretBelief(sentence)
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
      val printer = new SilSentencePrinter
      val beliefStrings = creed.allBeliefs.map(printer.print)
      beliefStrings.map(ShlurdParseUtils.capitalize) must be equalTo expected
      val refriedBeliefs = beliefStrings.map(beliefString => {
        val sentence = ShlurdParser(beliefString).parseOne
        refriedInterpreter.recognizeBelief(sentence)
      })
      refriedBeliefs.foreach(belief => {
        belief must beSome
          refriedInterpreter.applyBelief(belief.get)
      })
      val refriedStrings = refriedCreed.allBeliefs.map(printer.print)
      refriedStrings.map(ShlurdParseUtils.capitalize) must be equalTo expected
    }
  }

  private val stateMust = "A door must be open or closed."
  private val stateMay = "A window may be open or closed."
  private val stateAlias = "A lit light is on."
  private val stateNormalization = "A person at home is present."
  private val stateProperty = "A dog's mood may be happy or sad."
  private val formTaxonomy = "A duck is a kind of a bird."
  private val formSynonym = "An automobile is a car."
  private val formRole = "A mentor must be a person."
  private val formRole2 = "An owner must be a person."
  private val formRole3 = "A pet must be an animal."
  private val assocHas = "A dog has an owner."
  private val assocMust = "A dog must have one owner."
  private val assocMay = "A person may have one mentor."
  private val assocMayPlural = "A person may have pets."
  private val assocMayProperty = "A person may have one presence as a property."
  private val entityExists = "There is a parakeet."
  private val namedEntityExists = "Fido is a dog."
  private val entityQualifiedExists = "There is an angry cat."
  private val personExists = "Yoda is a person."
  private val personExists2 = "Luke is a person."
  private val personAssoc = "Yoda is Luke's mentor."
  private val mentorRole = "A mentor must be a jedi."
  private val padawanRole = "A padawan must be a jedi."
  private val jediPadawan = "A jedi may have padawans."
  private val jediMentor = "A jedi may have one mentor."
  private val assocInverse1 = "A jedi with a padawan is a mentor."
  private val assocInverse2 = "A jedi with a mentor is a padawan."
  private val padawanMentor = "A padawan may have mentors."

  "SpcCreed" should
  {
    "preserve states" in new CosmosContext
    {
      expectPreserved(Seq(stateMust, stateMay))
    }

    "preserve state alias" in new CosmosContext
    {
      expectPreserved(Seq(stateAlias))
    }

    "preserve state normalizations" in new CosmosContext
    {
      expectPreserved(Seq(stateNormalization))
    }

    "preserve state property" in new CosmosContext
    {
      expectPreserved(Seq(stateProperty))
    }

    "preserve form synonyms" in new CosmosContext
    {
      expectPreserved(Seq(formSynonym))
    }

    "preserve form taxonomy" in new CosmosContext
    {
      expectPreserved(Seq(formTaxonomy))
    }

    "preserve form associations" in new CosmosContext
    {
      expectPreserved(Seq(
        formRole, formRole2, formRole3, assocMay, assocMayPlural,
        assocMayProperty, assocMust))
    }

    "normalize form associations" in new CosmosContext
    {
      expectNormalized(Seq(formRole2, assocHas), Seq(formRole2, assocMust))
    }

    "preserve entity existence" in new CosmosContext
    {
      expectPreserved(Seq(
        entityExists, entityQualifiedExists, personExists))
    }

    "preserve named entity existence" in new CosmosContext
    {
      expectPreserved(Seq(namedEntityExists))
    }

    "preserve entity associations" in new CosmosContext
    {
      expectPreserved(Seq(
        formRole, assocMay,
        personExists, personExists2, personAssoc))
    }

    "preserve inverse associations" in new CosmosContext
    {
      expectPreserved(Seq(
        mentorRole, padawanRole,
        jediPadawan, jediMentor,
        assocInverse1, assocInverse2))
    }

    "normalize inverse associations" in new CosmosContext
    {
      expectNormalized(
        Seq(
          mentorRole, padawanRole,
          jediMentor, assocInverse1),
        Seq(
          mentorRole, padawanRole,
          jediMentor, jediPadawan,
          assocInverse1, assocInverse2))
    }

    "normalize inverse associations with implied roles" in new CosmosContext
    {
      expectNormalized(
        Seq(assocInverse1),
        Seq(mentorRole, jediPadawan, padawanMentor, assocInverse1))
    }
  }
}
