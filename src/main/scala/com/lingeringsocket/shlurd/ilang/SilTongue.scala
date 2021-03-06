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
package com.lingeringsocket.shlurd.ilang

import com.lingeringsocket.shlurd._

import scala.collection._

trait SilTongue extends SilGenderAnalyzer
{
  def getIdentifier : String

  def newSentenceBundle : SilSentenceBundle

  def getPronounUsage(
    inflection : SilInflection,
    proximity : SilProximity) : String

  def isBeingLemma(verb : SilWord) : Boolean =
    isBeingLemma(verb.toLemma)

  def isBeingLemma(lemma : String) : Boolean

  def isGenitiveVariableLemma(lemma : String) : Boolean

  def combineGenders(genders : Seq[SilGender]) : SilGender

  def getAdjectivePosition : SilModifierPosition

  def allowElidedSubject : Boolean = false

  def combinePolarities(
    predicate : SilPredicate,
    truthBoolean : Boolean,
    negateCollection : Boolean,
    subjectVariable : Boolean) : Boolean =
  {
    truthBoolean || negateCollection
  }

  def combinePoliteness(
    p1 : SilPoliteness,
    p2 : SilPoliteness) : SilPoliteness =
  {
    tupleN(p1, p2) match {
      case (POLITENESS_FAMILIAR, POLITENESS_FAMILIAR) => POLITENESS_FAMILIAR
      case _ => POLITENESS_RESPECTFUL
    }
  }

  def getNoneCount : SilCount = COUNT_PLURAL

  def getEffectivePerson(
    pr : SilPronounReference,
    formality : SilFormality = SilFormality.DEFAULT) : SilPerson =
  {
    pr.person
  }
}
