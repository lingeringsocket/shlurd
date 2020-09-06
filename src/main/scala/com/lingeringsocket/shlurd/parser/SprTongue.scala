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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

trait SprSynthesizer
{
  def makeLeaf(
    label : String, token : String, lemma : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, lemma, token)
  }

  def makeLeaf(
    label : String, token : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, token, token)
  }

  def makeLeaf(
    token : String) : SprSyntaxLeaf =
  {
    makeLeaf(token, token, token)
  }
}

abstract class SprTongue(wordnet : ShlurdWordnet)
    extends SprSynthesizer with SilGenderAnalyzer
{
  lazy val relLemmaMap = Map(SilRelationshipPredef.enumeration.map(
      rel => tupleN((getRelPredefLemma(rel), rel))):_*)

  def newSentencePrinter(
    genderAnalyzer : SilGenderAnalyzer) : SilSentencePrinter

  def getWordnet = wordnet

  def getStopList : Set[String]

  def getRelPredefLemma(predef : SilRelationshipPredef) : String

  def getStatePredefLemma(predef : SilStatePredef) : String

  def getStatePredefFromLemma(lemma : String) : SilStatePredef

  def isBeingLemma(lemma : String) : Boolean

  def isPossessionLemma(lemma : String) : Boolean

  def isExistsLemma(lemma : String) : Boolean

  def getPronounMap(
    gender : SilBasicGender,
    count : SilCount
  ) : SilPronounMap

  override def deriveGender(ref : SilReference) : SilGender = GENDER_NEUTER

  def correctGenderCount(
    lemma : String, gender : SilGender, count : SilCount,
    isModifier : Boolean) : String = lemma

  def combineGenders(genders : Seq[SilGender]) : SilGender =
  {
    if (genders.size == 1) {
      genders.head
    } else {
      GENDER_NEUTER
    }
  }

  def maybeDeterminerFor(lemma : String) : Option[SilDeterminer]

  def isCoordinatingDeterminer(lemma : String) : Boolean

  def isCoordinatingConjunction(lemma : String) : Boolean

  def isFlexiblePronoun(token : String) : Boolean

  def isReflexivePronoun(token : String) : Boolean

  def isPossessiveAdjective(token : String) : Boolean

  def isAdposition(lemma : String) : Boolean

  def isSubordinatingConjunction(lemma : String) : Boolean

  def isProper(lemma : String) : Boolean

  def isPronounWord(lemma : String) : Boolean

  def analyzePronoun(lemma : String) :
      (SilPerson, SilCount, SilGender, Option[SilProximity])

  def labelVerb(token : String, lemma : String) : Set[String]

  def synthesizeMembersRef(
    annotator : SilAnnotator,
    determiner : SilDeterminer,
    gender : SilGender) : SilReference

  def synthesizeSubsetRef(
    annotator : SilAnnotator,
    membersRef : SilReference,
    setRef : SilReference) : SilReference =
  {
    annotator.stateSpecifiedRef(
      membersRef,
      SilAdpositionalState(
        SilAdposition.OF,
        setRef))
  }
}
