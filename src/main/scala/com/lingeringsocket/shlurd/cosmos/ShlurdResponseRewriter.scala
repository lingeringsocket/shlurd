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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import ShlurdEnglishLemmas._

class ShlurdResponseRewriter[E<:ShlurdEntity, P<:ShlurdProperty](
  mind : ShlurdMind[E,P]) extends SilPhraseRewriter
{
  private val cosmos = mind.getCosmos

  def normalizeResponse(
    predicate : SilPredicate,
    resultCollector : ResultCollector[E],
    params : ShlurdInterpreterParams)
      : (SilPredicate, Boolean) =
  {
    var negateCollection = false
    val entityDeterminer = predicate match {
      case SilStatePredicate(subject, SilExistenceState()) => {
        DETERMINER_NONSPECIFIC
      }
      case _ => {
        DETERMINER_UNIQUE
      }
    }

    def normalizeConjunctionWrapper(
      separator : SilSeparator,
      allRef : => SilReference) =
    {
      val (rr, rn) = normalizeConjunction(
        resultCollector, entityDeterminer, separator, params)
      if (rn) {
        negateCollection = true
      }
      rr.getOrElse(allRef)
    }

    def replaceReferences = replacementMatcher {
      case SilStateSpecifiedReference(
        ref, _
      ) if (!ref.acceptsSpecifiers) => {
        ref
      }
      case SilConjunctiveReference(determiner, references, separator) => {
        determiner match {
          case DETERMINER_ANY => {
            normalizeDisjunction(
              resultCollector, entityDeterminer,
              separator, params).getOrElse
            {
              negateCollection = true
              SilConjunctiveReference(
                DETERMINER_NONE, references, separator)
            }
          }
          case DETERMINER_ALL => {
            normalizeConjunctionWrapper(
              separator,
              SilConjunctiveReference(
                DETERMINER_ALL, references, separator))
          }
          case _ => {
            SilConjunctiveReference(determiner, references, separator)
          }
        }
      }
      case SilNounReference(
        noun, DETERMINER_ANY | DETERMINER_SOME, count
      ) => {
        normalizeDisjunction(
          resultCollector, entityDeterminer,
          SEPARATOR_OXFORD_COMMA, params).getOrElse
        {
          negateCollection = true
          val responseNoun = noun match {
            case SilWord(LEMMA_WHO, LEMMA_WHO) => {
              SilWord(LEMMA_ONE)
            }
            case _ => noun
          }
          SilNounReference(responseNoun, DETERMINER_NONE, count)
        }
      }
      case SilNounReference(
        noun, DETERMINER_ALL, count
      ) => {
        normalizeConjunctionWrapper(
          SEPARATOR_OXFORD_COMMA,
          SilNounReference(noun, DETERMINER_ALL, count))
      }
    }

    val firstRules = {
      if (getTrueEntities(resultCollector).isEmpty ||
        resultCollector.isCategorization)
      {
        replacePronounsSpeakerListener
      } else {
        combineRules(
          flipPredicateQueries,
          replacePronounsSpeakerListener)
      }
    }

    val rewrite1 = rewrite(
      firstRules,
      predicate)
    val rewrite2 = rewrite(
      combineRules(
        coerceCountAgreement,
        replaceReferences),
      rewrite1)
    val rewrite3 = rewrite(
      avoidTautologies,
      rewrite2)
    val rewriteLast = rewrite(
      replaceResolvedReferences,
      rewrite3)

    SilPhraseValidator.validatePhrase(rewriteLast)

    (rewriteLast, negateCollection)
  }

  def replacePronounsSpeakerListener = replacementMatcher {
    case SilPronounReference(person, gender, count)=> {
      val speakerListenerReversed = person match {
        case PERSON_FIRST => PERSON_SECOND
        case PERSON_SECOND => PERSON_FIRST
        case PERSON_THIRD => PERSON_THIRD
      }
      SilPronounReference(speakerListenerReversed, gender, count)
    }
  }

  // "Who is Slothrop?" becomes "Slothrop is who" so that the response
  // comes out more naturally as "Slothrop is a lieutenant"
  private def flipPredicateQueries = replacementMatcher {
    case SilRelationshipPredicate(
      lhs,
      rhs,
      REL_IDENTITY
    ) if (containsWildcard(lhs) && !containsWildcard(rhs)) =>
      {
        SilRelationshipPredicate(
          rhs,
          lhs,
          REL_IDENTITY)
      }
  }

  private def avoidTautologies = replacementMatcher {
    case SilRelationshipPredicate(
      rr : SilResolvedReference[E],
      other : SilReference,
      REL_IDENTITY)
        if (rr.entities.size == 1) =>
      {
        SilRelationshipPredicate(
          chooseReference(rr.entities.head, other, rr.determiner),
          other,
          REL_IDENTITY)
      }
    case SilRelationshipPredicate(
      other : SilReference,
      rr : SilResolvedReference[E],
      REL_IDENTITY)
        if (rr.entities.size == 1) =>
      {
        SilRelationshipPredicate(
          other,
          chooseReference(rr.entities.head, other, rr.determiner),
          REL_IDENTITY)
      }
  }

  private def resolveReference(
    entity : E,
    determiner : SilDeterminer) : SilReference =
  {
    // FIXME:  something better for SilWord
    SilResolvedReference(Set(entity), SilWord(entity.toString), determiner)
  }

  private def chooseReference(
    entity : E,
    other : SilReference,
    determiner : SilDeterminer) : SilReference =
  {
    val equivs = mind.equivalentReferences(entity, determiner)
    equivs.find(_ != other) match {
      case Some(ref) => ref
      case _ => equivs.head
    }
  }

  private def replaceResolvedReferences = replacementMatcher {
    case rr : SilResolvedReference[E] if (rr.entities.size == 1) => {
      cosmos.specificReference(rr.entities.head, rr.determiner)
    }
  }

  private def coerceCountAgreement = replacementMatcher {
    case SilRelationshipPredicate(subject, complement, REL_IDENTITY) => {
      val subjectCount = SilReference.getCount(subject)
      val complementCount = SilReference.getCount(complement)
      if (subjectCount != complementCount) {
        val subjectCoercible =
          SilReference.isCountCoercible(subject)
        val complementCoercible =
          SilReference.isCountCoercible(complement)
        val agreedCount = {
          if (subjectCoercible && complementCoercible) {
            COUNT_PLURAL
          } else if (subjectCoercible) {
            complementCount
          } else {
            subjectCount
          }
        }
        def coerceIfNeeded(reference : SilReference, count : SilCount) =
        {
          if (count == agreedCount) {
            reference
          } else {
            coerceCount(reference, agreedCount)
          }
        }
        SilRelationshipPredicate(
          coerceIfNeeded(subject, subjectCount),
          coerceIfNeeded(complement, complementCount),
          REL_IDENTITY)
      } else {
        SilRelationshipPredicate(subject, complement, REL_IDENTITY)
      }
    }
  }

  private def coerceCount(
    reference : SilReference, agreedCount : SilCount) : SilReference =
  {
    reference match {
      case SilStateSpecifiedReference(subReference, state) => {
        SilStateSpecifiedReference(
          coerceCount(subReference, agreedCount), state)
      }
      case SilGenitiveReference(possessor, possessee) => {
        SilGenitiveReference(
          possessor, coerceCount(possessee, agreedCount))
      }
      case nounRef : SilNounReference => {
        if (nounRef.count == agreedCount) {
          nounRef
        } else {
          val newDeterminer = {
            if (agreedCount == COUNT_PLURAL) {
              if (nounRef.determiner == DETERMINER_NONSPECIFIC) {
                DETERMINER_UNSPECIFIED
              } else {
                nounRef.determiner
              }
            } else {
              nounRef.determiner
            }
          }
          SilNounReference(
            SilWord("", nounRef.noun.lemma),
            newDeterminer, agreedCount)
        }
      }
      case _ => reference
    }
  }

  private def getTrueEntities(resultCollector : ResultCollector[E]) =
  {
    ShlurdParseUtils.orderedSet(
      resultCollector.entityMap.filter(
        _._2.assumeFalse).keySet)
  }

  private def getFalseEntities(resultCollector : ResultCollector[E]) =
  {
    ShlurdParseUtils.orderedSet(
      resultCollector.entityMap.filterNot(
        _._2.assumeTrue).keySet)
  }

  private def normalizeDisjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : ShlurdInterpreterParams)
      : Option[SilReference] =
  {
    val trueEntities = getTrueEntities(resultCollector)
    val exhaustive =
      (trueEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    (if (trueEntities.isEmpty) {
      None
    } else if ((trueEntities.size == 1) && !params.alwaysSummarize) {
      Some(resolveReference(trueEntities.head, entityDeterminer))
    } else if (exhaustive || (trueEntities.size > params.listLimit)) {
      summarizeList(trueEntities, exhaustive, existence, false)
    } else {
      Some(SilConjunctiveReference(
        DETERMINER_ALL,
        trueEntities.map(
          resolveReference(_, entityDeterminer)).toSeq,
        separator))
    })
  }

  private def normalizeConjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : ShlurdInterpreterParams)
      : (Option[SilReference], Boolean) =
  {
    val falseEntities = getFalseEntities(resultCollector)
    val exhaustive =
      (falseEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    if (falseEntities.isEmpty) {
      (None, false)
    } else if ((falseEntities.size == 1) && !params.alwaysSummarize) {
      (Some(resolveReference(falseEntities.head, entityDeterminer)),
        false)
    } else if (exhaustive || (falseEntities.size > params.listLimit)) {
      (summarizeList(falseEntities, exhaustive, existence, true),
        exhaustive)
    } else {
      (Some(SilConjunctiveReference(
        DETERMINER_NONE,
        falseEntities.map(
          resolveReference(_, entityDeterminer)).toSeq,
        separator)),
        true)
    }
  }

  private def summarizeList(
    entities : Iterable[ShlurdEntity],
    exhaustive : Boolean,
    existence : Boolean,
    conjunction : Boolean) =
  {
    var all = exhaustive && (entities.size > 1)
    // FIXME:  make this language-independent
    val number = {
      if (conjunction && exhaustive) {
        all = false
        LEMMA_NONE
      } else  if ((entities.size == 2) && exhaustive && !existence) {
        all = false
        LEMMA_BOTH
      } else {
        entities.size.toString
      }
    }
    val determiner = {
      if (all && !existence) {
        DETERMINER_ALL
      } else {
        DETERMINER_UNSPECIFIED
      }
    }
    // FIXME:  derive gender from entities
    val count = number match {
      case "1" => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
    Some(
      SilStateSpecifiedReference(
        SilNounReference(SilWord(number), determiner, count),
        SilAdpositionalState(
          ADP_OF,
          SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL))))
  }

  def containsWildcard(phrase : SilPhrase) : Boolean =
  {
    val querier = new SilPhraseRewriter
    var wildcard = false
    def matchWildcard = querier.queryMatcher {
      case SilConjunctiveReference(
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _,
        _
      ) => {
        wildcard = true
      }
      case SilNounReference(
        _,
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _
      ) => {
        wildcard = true
      }
      case SilNounReference(
        SilWord(LEMMA_WHO, LEMMA_WHO),
        _,
        _
      ) => {
        wildcard = true
      }
    }
    querier.query(matchWildcard, phrase)
    wildcard
  }
}
