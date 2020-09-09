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

import net.sf.extjwnl.data._

import SprPennTreebankLabels._

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

sealed trait SprMagicWord
{
  def toLemma(implicit tongue : SprTongue) = magicToString(this)
}
sealed trait SprQuestionMagicWord extends SprMagicWord
sealed trait SprGenderMagicWord extends SprMagicWord
sealed trait SprBeliefMagicWord extends SprMagicWord
sealed trait SprAdpositionMagicWord extends SprMagicWord

// question words
case object MW_HOW_MANY extends SprQuestionMagicWord
case object MW_WHAT extends SprQuestionMagicWord
case object MW_WHERE extends SprQuestionMagicWord
case object MW_WHICH extends SprQuestionMagicWord
case object MW_WHO extends SprQuestionMagicWord
case object MW_WHOM extends SprQuestionMagicWord
case object MW_WHOSE extends SprQuestionMagicWord

// genders
case object MW_MASCULINE extends SprGenderMagicWord
case object MW_FEMININE extends SprGenderMagicWord
case object MW_NEUTER extends SprGenderMagicWord

// keywords used in belief statements
case object MW_ALSO extends SprBeliefMagicWord
case object MW_AND extends SprBeliefMagicWord
case object MW_ANOTHER extends SprBeliefMagicWord
case object MW_BELIEVE extends SprBeliefMagicWord
case object MW_CONSEQUENTLY extends SprBeliefMagicWord
case object MW_EITHER extends SprBeliefMagicWord
case object MW_EQUIVALENTLY extends SprBeliefMagicWord
case object MW_EXIST extends SprBeliefMagicWord
case object MW_GENERALLY extends SprBeliefMagicWord
case object MW_IF extends SprBeliefMagicWord
case object MW_KIND extends SprBeliefMagicWord
case object MW_NEITHER extends SprBeliefMagicWord
case object MW_OR extends SprBeliefMagicWord
case object MW_NOR extends SprBeliefMagicWord
case object MW_OTHERWISE extends SprBeliefMagicWord
case object MW_SAME extends SprBeliefMagicWord
case object MW_SUBSEQUENTLY extends SprBeliefMagicWord
case object MW_THEN extends SprBeliefMagicWord

// important adpositions
case object MW_AMONG extends SprAdpositionMagicWord
case object MW_EXCEPT extends SprAdpositionMagicWord
case object MW_IN extends SprAdpositionMagicWord
case object MW_INSIDE extends SprAdpositionMagicWord
case object MW_WITHIN extends SprAdpositionMagicWord
case object MW_OUTSIDE extends SprAdpositionMagicWord
case object MW_AT extends SprAdpositionMagicWord
case object MW_WITH extends SprAdpositionMagicWord
case object MW_AS extends SprAdpositionMagicWord
case object MW_NEAR extends SprAdpositionMagicWord
case object MW_NEARBY extends SprAdpositionMagicWord
case object MW_ON extends SprAdpositionMagicWord
case object MW_ABOVE extends SprAdpositionMagicWord
case object MW_OVER extends SprAdpositionMagicWord
case object MW_BELOW extends SprAdpositionMagicWord
case object MW_UNDER extends SprAdpositionMagicWord
case object MW_BENEATH extends SprAdpositionMagicWord
case object MW_UNDERNEATH extends SprAdpositionMagicWord
case object MW_BEFORE extends SprAdpositionMagicWord with SprBeliefMagicWord
case object MW_AFTER extends SprAdpositionMagicWord with SprBeliefMagicWord
case object MW_LEFT extends SprAdpositionMagicWord
case object MW_RIGHT extends SprAdpositionMagicWord
case object MW_FRONT extends SprAdpositionMagicWord
case object MW_BACK extends SprAdpositionMagicWord
case object MW_BEHIND extends SprAdpositionMagicWord
case object MW_TO extends SprAdpositionMagicWord
case object MW_FROM extends SprAdpositionMagicWord
case object MW_OF extends SprAdpositionMagicWord
case object MW_GENITIVE_OF extends SprAdpositionMagicWord
case object MW_ADVERBIAL_TMP extends SprAdpositionMagicWord

object SilMagicWord
{
  def apply(keyword : SprMagicWord)(implicit tongue : SprTongue) =
  {
    SilWord(tongue.keywordLemma(keyword))
  }

  def unapply(w : SilSimpleWord)(implicit tongue : SprTongue)
      : Option[SprMagicWord] =
  {
    tongue.keywordForLemma(w.lemma)
  }
}

abstract class SprTongue(wordnet : ShlurdWordnet)
    extends SprSynthesizer with SilGenderAnalyzer
{
  lazy val relLemmaMap = Map(SilRelationshipPredef.enumeration.map(
    rel => tupleN((getRelPredefLemma(rel), rel))):_*)

  private implicit val tongue = this

  def newSentencePrinter(
    genderAnalyzer : SilGenderAnalyzer) : SilSentencePrinter

  def newSyntaxAnalyzer(
    context : SprContext,
    guessedQuestion : Boolean,
    strictness : SprStrictness = SPR_STRICTNESS_LOOSE,
    enforceTransitive : Boolean = true
  ) : SprSyntaxAnalyzer

  def getWordnet = wordnet

  def getStopList : Set[String]

  def getPhraseScorers : Seq[SilWordnetScorer.PhraseScorer] = Seq.empty

  def getRelPredefLemma(predef : SilRelationshipPredef) : String

  def getStatePredefLemma(predef : SilStatePredef) : String

  def getStatePredefFromLemma(lemma : String) : SilStatePredef

  def isBeingLemma(lemma : String) : Boolean

  def isBeingLemma(verb : SilWord) : Boolean =
    isBeingLemma(verb.toLemma)

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

  def maybeDeterminerFor(
    lemma : String) : Option[SilDeterminer]

  def isCoordinatingDeterminer(lemma : String) : Boolean

  def isCoordinatingConjunction(lemma : String) : Boolean

  def isFlexiblePronoun(token : String) : Boolean

  def isReflexivePronoun(token : String) : Boolean

  def isPossessiveAdjective(token : String) : Boolean

  def isAdposition(lemma : String) : Boolean

  def isSubordinatingConjunction(lemma : String) : Boolean

  def isProper(lemma : String) : Boolean

  def getPronounLemmas() : Set[String]

  def isPronounWord(lemma : String) : Boolean =
  {
    getPronounLemmas.contains(lemma)
  }

  def analyzePronoun(lemma : String) :
      (
        SilPerson, SilCount, SilGender,
        SilInflection, Option[SilProximity], SilCount
      )

  def isSpecialAdposition(lemma : String) : Boolean = false

  def isAdpositionablePronoun(lemma : String) : Boolean = true

  def labelVerb(token : String, lemma : String) : Set[String]

  def labelPronoun(
    word : String,
    token : String,
    foldEphemeralLabels : Boolean) : Set[SprSyntaxTree] =
  {
    if (isPronounWord(token)) {
      val leaf = makeLeaf(word, token)
      if (isFlexiblePronoun(token)) {
        Set(SptPRP_POS(leaf), SptPRP(leaf))
      } else if (isPossessiveAdjective(token)) {
        Set(SptPRP_POS(leaf))
      } else if (isReflexivePronoun(token) && !foldEphemeralLabels)
      {
        Set(SprSyntaxRewriter.recompose(LABEL_PRP_REFLEXIVE, Seq(leaf)))
      } else {
        Set(SptPRP(leaf))
      }
    } else {
      Set.empty
    }
  }

  def labelSpecial(
    word : String,
    token : String
  ) : Set[SprSyntaxTree] = Set.empty

  def analyzeVerbConjugation(word : SilWord)
      : (SilPerson, SilCount, SilGender, SilTam) =
  {
    tupleN((PERSON_THIRD, COUNT_SINGULAR, GENDER_NEUTER, SilTam.indicative))
  }

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
        SilAdposition(MW_OF),
        setRef))
  }

  def shouldForceSQ(tree : SprSyntaxTree) : Boolean = false

  def proximityLemma(proximity : SilProximity) : String

  def proximityForLemma(lemma : String) : Option[SilProximity]

  def keywordLemma(keyword : SprMagicWord) : String

  def keywordForLemma(lemma : String) : Option[SprMagicWord]

  def pronounLemma(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity,
    inflection : SilInflection,
    possesseeCount : SilCount = COUNT_SINGULAR
  ) : String

  def filterIndexWords(
    token : String,
    tokenSuffix : String,
    rawWords : Set[IndexWord]
  ) : Set[IndexWord] = rawWords

  def overThere : SilProximity = PROXIMITY_OVER_THERE

  def possibleCompoundNoun(seq : Seq[SprSyntaxTree]) : Boolean = true

  def possibleCompoundVerb(seq : Seq[String]) : Boolean = true

  def pluralizeNoun(lemma : String) : String

  protected def usageScore(lemma : String, pos : POS) : SilPhraseScore =
  {
    if (lemma.contains('-')) {
      return SilPhraseScore.neutral
    }
    val score = wordnet.getUsageScore(lemma, pos)
    if (score == 0) {
      SilPhraseScore.proSmall
    } else if (score < 0) {
      SilPhraseScore.neutral
    } else {
      SilPhraseScore.pro(1 + score)
    }
  }
}
