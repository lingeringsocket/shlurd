// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

import org.kiama.rewriting._

class ShlurdResponseRewrite[E<:ShlurdEntity, P<:ShlurdProperty](
  world : ShlurdWorld[E,P])
{
  def normalizeResponse(
    predicate : ShlurdPredicate,
    resultCollector : ResultCollector[E],
    params : ShlurdInterpreterParams)
      : (ShlurdPredicate, Boolean) =
  {
    var negateCollection = false
    val entityDeterminer = predicate match {
      case ShlurdStatePredicate(subject, ShlurdExistenceState()) => {
        DETERMINER_NONSPECIFIC
      }
      case _ => {
        DETERMINER_UNIQUE
      }
    }
    def normalizeConjunctionWrapper(
      separator : ShlurdSeparator,
      allRef : => ShlurdReference) =
    {
      val (rr, rn) = normalizeConjunction(
        resultCollector, entityDeterminer, separator, params)
      if (rn) {
        negateCollection = true
      }
      rr.getOrElse(allRef)
    }
    val rewriteReferences = Rewriter.rule[ShlurdPhrase] {
      case ShlurdStateSpecifiedReference(outer, state) => {
        outer match {
          case ShlurdStateSpecifiedReference(
            inner, ShlurdNullState()) =>
            {
              inner
            }
          case _ => {
            ShlurdStateSpecifiedReference(outer, state)
          }
        }
      }
      case ShlurdPronounReference(person, gender, count)=> {
        val speakerListenerReversed = person match {
          case PERSON_FIRST => PERSON_SECOND
          case PERSON_SECOND => PERSON_FIRST
          case PERSON_THIRD => PERSON_THIRD
        }
        ShlurdPronounReference(speakerListenerReversed, gender, count)
      }
      case ShlurdConjunctiveReference(determiner, references, separator) => {
        determiner match {
          case DETERMINER_ANY => {
            normalizeDisjunction(
              resultCollector, entityDeterminer,
              separator, params).getOrElse
            {
              negateCollection = true
              ShlurdConjunctiveReference(
                DETERMINER_NONE, references, separator)
            }
          }
          case DETERMINER_ALL => {
            normalizeConjunctionWrapper(
              separator,
              ShlurdConjunctiveReference(
                DETERMINER_ALL, references, separator))
          }
          case _ => {
            ShlurdConjunctiveReference(determiner, references, separator)
          }
        }
      }
    }

    val expandReferences = Rewriter.rule[ShlurdPhrase] {
      case ShlurdEntityReference(
        entity, DETERMINER_ANY | DETERMINER_SOME, count
      ) => {
        normalizeDisjunction(
          resultCollector, entityDeterminer,
          SEPARATOR_OXFORD_COMMA, params).getOrElse
        {
          negateCollection = true
          ShlurdEntityReference(entity, DETERMINER_NONE, count)
        }
      }
      case ShlurdEntityReference(
        entity, DETERMINER_ALL, count
      ) => {
        normalizeConjunctionWrapper(
          SEPARATOR_OXFORD_COMMA,
          ShlurdEntityReference(entity, DETERMINER_ALL, count))
      }
    }

    val coerceCountAgreement = Rewriter.rule[ShlurdPhrase] {
      case ShlurdRelationshipPredicate(subject, complement, REL_IDENTITY) => {
        val subjectCount = ShlurdReference.getCount(subject)
        val complementCount = ShlurdReference.getCount(complement)
        if (subjectCount != complementCount) {
          val subjectCoercible =
            ShlurdReference.isCountCoercible(subject)
          val complementCoercible =
            ShlurdReference.isCountCoercible(complement)
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
          def coerceIfNeeded(reference : ShlurdReference, count : ShlurdCount) =
          {
            if (count == agreedCount) {
              reference
            } else {
              coerceCount(reference, agreedCount)
            }
          }
          ShlurdRelationshipPredicate(
            coerceIfNeeded(subject, subjectCount),
            coerceIfNeeded(complement, complementCount),
            REL_IDENTITY)
        } else {
          ShlurdRelationshipPredicate(subject, complement, REL_IDENTITY)
        }
      }
    }

    val allRules = rewriteReferences + expandReferences + coerceCountAgreement

    val rewritten = Rewriter.rewrite(
      Rewriter.everywherebu("normalizeResponse", allRules)
    )(predicate)

    (rewritten, negateCollection)
  }

  private def coerceCount(
    reference : ShlurdReference, agreedCount : ShlurdCount) : ShlurdReference =
  {
    reference match {
      case ShlurdStateSpecifiedReference(subReference, state) => {
        ShlurdStateSpecifiedReference(
          coerceCount(subReference, agreedCount), state)
      }
      case ShlurdGenitiveReference(genitive, subReference) => {
        ShlurdGenitiveReference(
          genitive, coerceCount(subReference, agreedCount))
      }
      case er : ShlurdEntityReference => {
        if (er.count == agreedCount) {
          er
        } else {
          ShlurdEntityReference(
            ShlurdWord("", er.entity.lemma),
            er.determiner, agreedCount)
        }
      }
      case _ => reference
    }
  }

  private def normalizeDisjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : ShlurdDeterminer,
    separator : ShlurdSeparator,
    params : ShlurdInterpreterParams)
      : Option[ShlurdReference] =
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
      Some(world.specificReference(trueEntities.head, entityDeterminer))
    } else if (exhaustive || (trueEntities.size > params.listLimit)) {
      summarizeList(trueEntities, exhaustive, existence, false)
    } else {
      Some(ShlurdConjunctiveReference(
        DETERMINER_ALL,
        trueEntities.map(
          world.specificReference(_, entityDeterminer)).toSeq,
        separator))
    }).map(r => ShlurdStateSpecifiedReference(r, ShlurdNullState()))
  }

  private def normalizeConjunction(
    resultCollector : ResultCollector[E],
    entityDeterminer : ShlurdDeterminer,
    separator : ShlurdSeparator,
    params : ShlurdInterpreterParams)
      : (Option[ShlurdReference], Boolean) =
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
      (Some(world.specificReference(falseEntities.head, entityDeterminer)),
        false)
    } else if (exhaustive || (falseEntities.size > params.listLimit)) {
      (summarizeList(falseEntities, exhaustive, existence, true),
        exhaustive)
    } else {
      (Some(ShlurdConjunctiveReference(
        DETERMINER_NONE,
        falseEntities.map(
          world.specificReference(_, entityDeterminer)).toSeq,
        separator)),
        true)
    })
    tuple.copy(_1 = tuple._1.map(
      r => ShlurdStateSpecifiedReference(r, ShlurdNullState())))
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
        Seq(ShlurdWord("all", "all"))
      } else {
        Seq.empty
      }
    }
    val qualifiers = prefix ++ Seq(ShlurdWord(number, number))
    // FIXME:  derive gender from entities
    val count = number match {
      case "1" => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
    Some(
      ShlurdReference.qualified(
        ShlurdPronounReference(PERSON_THIRD, GENDER_N, count),
        qualifiers))
  }
}
