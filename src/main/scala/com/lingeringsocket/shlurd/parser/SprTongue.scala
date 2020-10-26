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
import scala.jdk.CollectionConverters._

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

sealed trait SprPredef
{
  def toLemma(implicit tongue : SprTongue) = predefToString(this)
}
sealed trait SprQuestionPredef extends SprPredef
sealed trait SprGenderPredef extends SprPredef
sealed trait SprBeliefPredef extends SprPredef
sealed trait SprAdpositionPredef extends SprPredef

// question words
case object PD_HOW_MANY extends SprQuestionPredef
case object PD_WHAT extends SprQuestionPredef
case object PD_WHEN extends SprQuestionPredef
case object PD_WHERE extends SprQuestionPredef
case object PD_WHICH extends SprQuestionPredef
case object PD_WHO extends SprQuestionPredef
case object PD_WHOM extends SprQuestionPredef
case object PD_WHOSE extends SprQuestionPredef

// genders
case object PD_MASCULINE extends SprGenderPredef
case object PD_FEMININE extends SprGenderPredef
case object PD_NEUTER extends SprGenderPredef

// predefs used in belief statements
case object PD_ALSO extends SprBeliefPredef
case object PD_AND extends SprBeliefPredef
case object PD_ANOTHER extends SprBeliefPredef
// FIXME make this one less squishy across languages
case object PD_BE extends SprBeliefPredef
case object PD_BELIEVE extends SprBeliefPredef
case object PD_BOTH extends SprBeliefPredef
case object PD_CONSEQUENTLY extends SprBeliefPredef
case object PD_EITHER extends SprBeliefPredef
case object PD_EQUIVALENTLY extends SprBeliefPredef
case object PD_EXIST extends SprBeliefPredef
case object PD_FORMER extends SprBeliefPredef
case object PD_GENERALLY extends SprBeliefPredef
case object PD_IF extends SprBeliefPredef
case object PD_KIND extends SprBeliefPredef
case object PD_LATTER extends SprBeliefPredef
case object PD_NEITHER_NOUN extends SprBeliefPredef
case object PD_NEITHER_DETERMINER extends SprBeliefPredef
case object PD_NOTHING extends SprBeliefPredef
case object PD_NOWHERE extends SprBeliefPredef
case object PD_ONE extends SprBeliefPredef
case object PD_OTHER extends SprBeliefPredef
case object PD_OR extends SprBeliefPredef
case object PD_NONE_NOUN extends SprBeliefPredef
case object PD_NONE_DETERMINER extends SprBeliefPredef
case object PD_NOR extends SprBeliefPredef
case object PD_OTHERWISE extends SprBeliefPredef
case object PD_SAME extends SprBeliefPredef
case object PD_SUBSEQUENTLY extends SprBeliefPredef
case object PD_THAT extends SprBeliefPredef
case object PD_THEN extends SprBeliefPredef
case object PD_WHENEVER extends SprBeliefPredef

// important adpositions
case object PD_AMONG extends SprAdpositionPredef
case object PD_EXCEPT extends SprAdpositionPredef
case object PD_IN extends SprAdpositionPredef
case object PD_INSIDE extends SprAdpositionPredef
case object PD_WITHIN extends SprAdpositionPredef
case object PD_OUTSIDE extends SprAdpositionPredef
case object PD_AT extends SprAdpositionPredef
case object PD_WITH extends SprAdpositionPredef
case object PD_AS extends SprAdpositionPredef
case object PD_NEAR extends SprAdpositionPredef
case object PD_NEARBY extends SprAdpositionPredef
case object PD_ON extends SprAdpositionPredef
case object PD_ABOVE extends SprAdpositionPredef
case object PD_OVER extends SprAdpositionPredef
case object PD_BEFORE extends SprAdpositionPredef with SprBeliefPredef
case object PD_AFTER extends SprAdpositionPredef with SprBeliefPredef
case object PD_LEFT extends SprAdpositionPredef
case object PD_RIGHT extends SprAdpositionPredef
case object PD_FRONT extends SprAdpositionPredef
case object PD_BACK extends SprAdpositionPredef
case object PD_BEHIND extends SprAdpositionPredef
case object PD_TO extends SprAdpositionPredef
case object PD_DATIVE_TO extends SprAdpositionPredef
case object PD_FROM extends SprAdpositionPredef
case object PD_OF extends SprAdpositionPredef
case object PD_GENITIVE_OF extends SprAdpositionPredef
case object PD_ADVERBIAL_TMP extends SprAdpositionPredef

object SprPredefWord
{
  def apply(predef : SprPredef)(implicit tongue : SprTongue) =
  {
    SilWord(tongue.predefLemma(predef))
  }

  def unapply(w : SilWord)(
    implicit tongue : SprTongue,
    label : String = LABEL_AMBIGUOUS)
      : Option[SprPredef] =
  {
    tongue.predefForLemma(w.toLemma, label)
  }
}

object SprPredefDeterminerWord
{
  def unapply(w : SilWord)(
    implicit tongue : SprTongue)
      : Option[SprPredef] =
  {
    tongue.predefForLemma(w.toLemma, LABEL_DT)
  }
}

object SprPredefAdposition
{
  def apply(
    predef : SprPredef)(implicit tongue : SprTongue) : SilAdposition =
  {
    SilAdposition(SprPredefWord(predef))
  }

  def unapply(
    adposition : SilAdposition)(implicit tongue : SprTongue)
      : Option[SprPredef] =
  {
    tongue.predefForLemma(adposition.word.toLemma, LABEL_IN)
  }
}

object SprProximityWord
{
  def apply(proximity : SilProximity)(implicit tongue : SprTongue) =
  {
    SilWord(tongue.proximityLemma(proximity))
  }

  def unapply(w : SilSimpleWord)(implicit tongue : SprTongue) =
  {
    tongue.proximityForLemma(w.lemma)
  }
}

abstract class SprTongue(wordnet : SprWordnet)
    extends SprSynthesizer with SilTongue
{
  lazy val relLemmaMap = Map(SprRelationshipPredef.enumeration.map(
    rel => tupleN(getRelPredefLemma(rel), rel)).toSeq:_*)

  private lazy val phrasePatternMatcher = loadMatcher

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

  def getPhraseScorers : Seq[SprWordnetScorer.PhraseScorer] = Seq.empty

  def getNormalizationRules(
    annotator : SilAnnotator,
    genderAnalyzer : SilGenderAnalyzer
  ) : Seq[SilPhraseReplacementMatcher] = Seq.empty

  def getResponseRules(
    refToPronoun : (SilReference, Boolean) => SilReference
  ) : Seq[SilPhraseReplacementMatcher] = Seq.empty

  def getRelPredefLemma(predef : SprRelationshipPredef) : String

  def getStatePredefLemma(predef : SprStatePredef) : String

  def getStatePredefFromLemma(lemma : String) : SprStatePredef

  def isModalAuxLemma(lemma : String) : Boolean = false

  def isProgressiveAuxLemma(lemma : String) : Boolean = false

  def tamForAuxLemma(
    auxLemma : String, verbLemma : String) : SilTam = SilTam.indicative

  def isRelationshipLemma(lemma : String) : Boolean =
    isBeingLemma(lemma) || isPossessionLemma(lemma)

  def isPossessionLemma(lemma : String) : Boolean

  def isExistsLemma(lemma : String) : Boolean

  def isImpersonalVerbLemma(lemma : String) : Boolean = false

  def isPotentialGerund(inflected : String) : Boolean = false

  def getPronounMap(
    gender : SilBasicGender,
    count : SilCount
  ) : SilPronounMap

  override def getPronounUsage(
    inflection : SilInflection,
    proximity : SilProximity) : String =
  {
    proximity match {
      case PROXIMITY_REFLEXIVE => LABEL_PRP_REFLEXIVE
      case _ => {
        inflection match {
          case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LABEL_PRP_OBJ
          case INFLECT_DATIVE => LABEL_PRP_DATIVE
          case INFLECT_GENITIVE => LABEL_PRP_POS
          case _ => LABEL_PRP
        }
      }
    }
  }

  def correctGenderCount(
    lemma : String, gender : SilGender, count : SilCount,
    isModifier : Boolean) : String = lemma

  override def combineGenders(genders : Seq[SilGender]) : SilGender =
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

  def isDemonstrative(lemma : String) : Boolean

  def isFlexiblePronoun(token : String) : Boolean

  def isReflexivePronoun(token : String) : Boolean

  def isPossessiveAdjective(token : String) : Boolean

  def isAdposition(lemma : String) : Boolean

  def adpositionForAux(auxLemma : String) : String

  def auxVerbForModal(modality : SilModality) : String

  def isSubordinatingConjunction(lemma : String) : Boolean

  def isProper(lemma : String) : Boolean

  def getPronounLemmas : Set[String]

  def isPronounWord(lemma : String) : Boolean =
  {
    getPronounLemmas.contains(lemma)
  }

  def analyzePronoun(lemma : String) :
      (
        SilPerson, SilCount, SilGender,
        Set[SilInflection], Option[SilProximity], SilCount,
        SilPoliteness
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
    tupleN(PERSON_THIRD, COUNT_SINGULAR, GENDER_NEUTER, SilTam.indicative)
  }

  def synthesizeSummaryRef(
    annotator : SilAnnotator,
    determiner : SilDeterminer,
    summarizedRef : SilReference,
    gender : SilGender,
    genderAnalyzer : SilGenderAnalyzer
  ) : SilReference =
  {
    annotator.stateSpecifiedRef(
      annotator.determinedRef(summarizedRef, determiner),
      SilAdpositionalState(
        SprPredefAdposition(PD_OF),
        annotator.pronounRef(
          PERSON_THIRD,
          gender,
          COUNT_PLURAL,
          genderAnalyzer)))
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
        SprPredefAdposition(PD_OF),
        setRef))
  }

  def shouldForceSQ(tree : SprSyntaxTree) : Boolean = false

  def proximityLemma(proximity : SilProximity) : String

  def proximityForLemma(lemma : String) : Option[SilProximity]

  def predefLemma(predef : SprPredef) : String

  def predefForLemma(
    lemma : String,
    label : String = LABEL_AMBIGUOUS
  ) : Option[SprPredef]

  def pronounLemma(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity,
    politeness : SilPoliteness,
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

  def isPluralNoun(
    token : String,
    lemma : String,
    indexWord : IndexWord) : Boolean =
  {
    if (token != lemma) {
      true
    } else {
      val bases = wordnet.getMorphology.lookupAllBaseForms(
        POS.NOUN, lemma).asScala
      bases.nonEmpty && bases.forall(b => pluralizeNoun(b) == lemma)
    }
  }

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

  private def loadMatcher =
  {
    val matcher = new SprPhrasePatternMatcher
    val source = ResourceUtils.getResourceSource(
      getMatcherResource)
    SprGrammar.buildMatcher(source, matcher)
    matcher
  }

  protected def getMatcherResource : String

  def getPhrasePatternMatcher : SprPhrasePatternMatcher =
    phrasePatternMatcher

  override def isGenitiveVariableLemma(lemma : String) : Boolean =
  {
    lemma == SprPredefWord(PD_WHO).toLemma
  }

  def chooseLemma(lemmas : Seq[String]) = lemmas.head
}
