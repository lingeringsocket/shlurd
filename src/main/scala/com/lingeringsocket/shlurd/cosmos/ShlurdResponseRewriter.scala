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
  cosmos : ShlurdCosmos[E,P]) extends SilPhraseRewriter
{
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
      case SilStateSpecifiedReference(outer, state) => {
        outer match {
          case SilStateSpecifiedReference(
            inner, SilNullState()) =>
            {
              inner
            }
          case _ => {
            SilStateSpecifiedReference(outer, state)
          }
        }
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

    def allRules = combineRules(
      replacePronounsSpeakerListener,
      replaceReferences,
      coerceCountAgreement)

    val rewritten = rewrite(allRules, predicate)
    (rewritten, negateCollection)
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

  private def coerceCountAgreement = replacementMatcher {
    case SilRelationshipPredicate(subject, complement, REL_IDENTITY) => {
      val subjectCount = SilReference.getCount(subject)
      val complementCount = SilReference.getCount(complement)
      if (subjectCount != complementCount) {
        val subjectCoercible =
          SilReference.isCountCoercible(subject)
        val complementCoercible =
          SilReference.isCountCoercible(complement)
        assert(subjectCoercible || complementCoercible)
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

  private def normalizeDisjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : ShlurdInterpreterParams)
      : Option[SilReference] =
  {
    val trueEntities = resultCollector.entityMap.filter(
      _._2.assumeFalse).keySet
    val exhaustive =
      (trueEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    (if (trueEntities.isEmpty) {
      None
    } else if ((trueEntities.size == 1) && !params.alwaysSummarize) {
      Some(cosmos.specificReference(trueEntities.head, entityDeterminer))
    } else if (exhaustive || (trueEntities.size > params.listLimit)) {
      summarizeList(trueEntities, exhaustive, existence, false)
    } else {
      Some(SilConjunctiveReference(
        DETERMINER_ALL,
        trueEntities.map(
          cosmos.specificReference(_, entityDeterminer)).toSeq,
        separator))
    }).map(r => SilStateSpecifiedReference(r, SilNullState()))
  }

  private def normalizeConjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : ShlurdInterpreterParams)
      : (Option[SilReference], Boolean) =
  {
    val falseEntities = resultCollector.entityMap.filterNot(
      _._2.assumeTrue).keySet
    val exhaustive =
      (falseEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    val tuple = (if (falseEntities.isEmpty) {
      (None, false)
    } else if ((falseEntities.size == 1) && !params.alwaysSummarize) {
      (Some(cosmos.specificReference(falseEntities.head, entityDeterminer)),
        false)
    } else if (exhaustive || (falseEntities.size > params.listLimit)) {
      (summarizeList(falseEntities, exhaustive, existence, true),
        exhaustive)
    } else {
      (Some(SilConjunctiveReference(
        DETERMINER_NONE,
        falseEntities.map(
          cosmos.specificReference(_, entityDeterminer)).toSeq,
        separator)),
        true)
    })
    tuple.copy(_1 = tuple._1.map(
      r => SilStateSpecifiedReference(r, SilNullState())))
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
        "none"
      } else  if ((entities.size == 2) && exhaustive && !existence) {
        all = false
        "both"
      } else {
        entities.size.toString
      }
    }
    val prefix = {
      if (all && !existence) {
        Seq(SilWord("all"))
      } else {
        Seq.empty
      }
    }
    val qualifiers = prefix ++ Seq(SilWord(number))
    // FIXME:  derive gender from entities
    val count = number match {
      case "1" => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
    Some(
      SilReference.qualified(
        SilPronounReference(PERSON_THIRD, GENDER_N, count),
        qualifiers))
  }
}
