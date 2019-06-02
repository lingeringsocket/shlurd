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
package com.lingeringsocket.shlurd.mind

import scala.collection._

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import SprEnglishLemmas._

class SmcResponseRewriter[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType]
](
  mind : SmcMind[EntityType, PropertyType, CosmosType],
  communicationContext : SmcCommunicationContext[EntityType]
) extends SmcPhraseRewriter
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  private val cosmos = mind.getCosmos

  private val entityMap =
    new mutable.LinkedHashMap[String, EntityType]

  def normalizeResponse(
    predicate : SilPredicate,
    resultCollector : ResultCollectorType,
    params : SmcResponseParams,
    question : Option[SilQuestion] = None)
      : (SilPredicate, Boolean) =
  {
    // for incomplete responses or where-questions, prevent flipping
    // subject/complement so that we can more easily find the answer
    // to the question asked
    val allowFlips = params.verbosity match {
      case RESPONSE_COMPLETE => {
        question match {
          case Some(QUESTION_WHERE) => false
          case Some(QUESTION_HOW_MANY) => false
          case _ => {
            predicate match {
              case SilRelationshipPredicate(
                _,
                SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
                SilGenitiveReference(
                  _,
                  SilNounReference(
                    SilWordLemma(SmcLemmas.LEMMA_CONTAINEE),
                    DETERMINER_UNSPECIFIED,
                    _)
                ),
                _
              ) => {
                false
              }
              case _ => true
            }
          }
        }
      }
      case _ => false
    }
    var negateCollection = false
    val entityDeterminer = predicate match {
      case SilStatePredicate(subject, _, SilExistenceState(_), _) => {
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

    def replaceReferences = replacementMatcher(
      "replaceReferences", {
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
            val (responseDeterminer, responseNoun) = noun match {
              case SilWordLemma(LEMMA_WHO) => {
                tupleN((DETERMINER_NONE, SilWord(LEMMA_ONE)))
              }
              case SilWordLemma(LEMMA_WHOM) => {
                tupleN((DETERMINER_NONE, SilWord(LEMMA_ONE)))
              }
              case SilWordLemma(LEMMA_WHERE) => {
                tupleN((DETERMINER_UNSPECIFIED, SilWord(LEMMA_NOWHERE)))
              }
              case SilWordLemma(LEMMA_WHAT) => {
                tupleN((DETERMINER_UNSPECIFIED, SilWord(LEMMA_NOTHING)))
              }
              case _ => (DETERMINER_NONE, noun)
            }
            SilNounReference(responseNoun, responseDeterminer, count)
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
    )

    val rewrite1 = {
      if (getTrueEntities(resultCollector).isEmpty ||
        resultCollector.isCategorization || !allowFlips)
      {
        rewrite(
          swapPronounsSpeakerListener(resultCollector.referenceMap),
          predicate)
      } else {
        rewrite(
          combineRules(
            swapPronounsSpeakerListener(resultCollector.referenceMap),
            flipPredicateQueries
          ),
          predicate)
      }
    }
    val rewrite2 = rewrite(
      combineRules(
        coerceCountAgreement,
        answerPropertyQueries(resultCollector.states),
        replaceReferences),
      rewrite1)
    val rewrite3 = rewrite(
      combineRules(
        clearActionInflection,
        avoidTautologies(resultCollector.referenceMap),
        removeResolvedReferenceQualifiers),
      rewrite2)
    val rewrite4 = rewrite(
      replaceResolvedReferences(resultCollector.referenceMap),
      rewrite3)
    val rewrite5 = {
      val useThirdPersonPronouns = predicate match {
        case SilStatePredicate(_, _, SilExistenceState(_), _) => false
        case _ => params.thirdPersonPronouns
      }
      if (useThirdPersonPronouns) {
        rewriteThirdPersonReferences(resultCollector, rewrite4)
      } else {
        rewrite(
          replaceThirdPersonPronouns(resultCollector.referenceMap),
          rewrite4)
      }
    }
    val rewriteLast = {
      if (allowFlips) {
        rewrite(
          flipPronouns,
          rewrite5)
      } else {
        rewrite5
      }
    }

    querier.query(clearInflectedCounts, rewriteLast)

    val normalized = transformQuestionResponse(
      rewriteLast, params, question, negateCollection)
    SilPhraseValidator.validatePhrase(normalized)
    tupleN((normalized, negateCollection))
  }

  def swapPronounsSpeakerListener(
    referenceMap : mutable.Map[SilReference, Set[EntityType]]
  ) = replacementMatcher(
    "swapPronounSpeakerListener", {
      case oldPronoun @ SilPronounReference(person, gender, count, distance)=> {
        val speakerListenerReversed = person match {
          case PERSON_FIRST => PERSON_SECOND
          case PERSON_SECOND => PERSON_FIRST
          case PERSON_THIRD => PERSON_THIRD
        }
        val newPronoun =
          SilPronounReference(speakerListenerReversed, gender, count, distance)
        referenceMap.get(oldPronoun).foreach(entities =>
          referenceMap.put(newPronoun, entities))
        newPronoun
      }
    }
  )

  class AmbiguousRefDetector(
    val referenceMap : mutable.Map[SilReference, Set[EntityType]])
  {
    val replacedRefMap =
      new mutable.LinkedHashMap[SilReference, Set[EntityType]]
    val ambiguousRefs =
      new mutable.LinkedHashSet[SilReference]

    def analyze(originalRef : SilReference)
    {
      val newEntitySet = referenceMap(originalRef)
      mind.thirdPersonReference(newEntitySet) match {
        case Some(replacedRef) => {
          replacedRefMap.get(replacedRef) match {
            case Some(existingEntitySet) => {
              if (existingEntitySet != newEntitySet) {
                // FIXME be a little smarter and choose the "best"
                // entity to preserve instead of the first one
                ambiguousRefs += originalRef
              }
            }
            case _ => {
              replacedRefMap.put(replacedRef, newEntitySet)
            }
          }
        }
        case _ =>
      }
    }
  }

  private def transformQuestionResponse(
    predicate : SilPredicate,
    params : SmcResponseParams,
    question : Option[SilQuestion],
    negateCollection : Boolean) : SilPredicate =
  {
    params.verbosity match {
      case RESPONSE_TERSE | RESPONSE_ELLIPSIS => {
        // in this case we just want to keep the container as the
        // subject for easy extraction, so don't transform back
        return predicate
      }
      case _ =>
    }
    tupleN((predicate, question)) match {
      case (rp @
          SilRelationshipPredicate(
            container,
            verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
            SilGenitiveReference(
              containee,
              SilNounReference(
                SilWordLemma(SmcLemmas.LEMMA_CONTAINER),
                DETERMINER_UNSPECIFIED,
                _)),
            verbModifiers
          ),
        Some(QUESTION_WHERE)
      ) => {
        if (negateCollection) {
          // FIXME this isn't right--I guess "nowhere" should really
          // be an adverb in this context?
          SilRelationshipPredicate(
            containee,
            verb.toUninflected,
            container,
            verbModifiers)
        } else {
          SilStatePredicate(
            containee,
            verb.toUninflected,
            SilAdpositionalState(
              SilAdposition.IN,
              container),
            verbModifiers)
        }
      }
      case (rp @
          SilRelationshipPredicate(
            containee,
            SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
            SilGenitiveReference(
              container,
              SilNounReference(
                SilWordLemma(SmcLemmas.LEMMA_CONTAINEE),
                DETERMINER_UNSPECIFIED,
                _)),
            verbModifiers
          ),
        _
      ) => {
        SilStatePredicate(
          containee,
          STATE_PREDEF_BE.toVerb,
          SilAdpositionalState(
            SilAdposition.IN,
            container),
          verbModifiers)
      }
      case _ => {
        predicate
      }
    }
  }

  private def rewriteThirdPersonReferences(
    resultCollector : ResultCollectorType,
    predicate : SilPredicate) : SilPredicate =
  {
    val referenceMap = resultCollector.referenceMap
    val detector = new AmbiguousRefDetector(referenceMap)
    querier.query(disqualifyThirdPersonReferences(referenceMap), predicate)
    querier.query(disambiguateThirdPersonReferences(detector), predicate)
    referenceMap --= detector.ambiguousRefs
    predicate match {
      case SilRelationshipPredicate(
        subject, SilRelationshipPredefVerb(REL_PREDEF_IDENTITY), complement, _
      ) => {
        tupleN((referenceMap.get(subject), referenceMap.get(complement))) match
        {
          case (Some(subjectEntities), Some(complementEntities)) => {
            if (subjectEntities == complementEntities) {
              // prevent a tautology
              referenceMap -= complement
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
    // use top down rewrite so that replacement of leaf references
    // does not mess up replacement of containing references
    rewrite(
      replaceThirdPersonReferences(referenceMap),
      predicate,
      SilRewriteOptions(topDown = true))
  }

  // "Groot is I" becomes "I am Groot"
  private def flipPronouns = replacementMatcher(
    "flipPronouns", {
      case SilRelationshipPredicate(
        lhs,
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        rhs : SilPronounReference,
        modifiers
      ) => {
        SilRelationshipPredicate(
          rhs,
          verb.toUninflected,
          lhs,
          modifiers)
      }
      case SilRelationshipPredicate(
        lhs,
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        rhs @ SilConjunctiveReference(_, references, _),
        modifiers
      ) if (references.exists(_.isInstanceOf[SilPronounReference])) => {
        SilRelationshipPredicate(
          rhs,
          verb.toUninflected,
          lhs,
          modifiers)
      }
    }
  )

  // "Who is Slothrop?" becomes "Slothrop is who" so that the response
  // comes out more naturally as "Slothrop is a lieutenant"
  private def flipPredicateQueries = replacementMatcher(
    "flipPredicateQueries",
    {
      case SilRelationshipPredicate(
        lhs,
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        rhs,
        verbModifiers
      ) if (containsWildcard(lhs) && !containsWildcard(rhs)) =>
        {
          SilRelationshipPredicate(
            rhs,
            verb.toUninflected,
            lhs,
            verbModifiers)
        }
    }
  )

  private def avoidTautologies(
    referenceMap : mutable.Map[SilReference, Set[EntityType]]
  ) = replacementMatcher(
    "avoidTautologies", {
      case SilRelationshipPredicate(
        SilMappedReference(key, determiner),
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        other : SilReference,
        verbModifiers
      ) => {
        val entity = entityMap(key)
        SilRelationshipPredicate(
          chooseReference(referenceMap, entity, other, determiner),
          verb.toUninflected,
          other,
          verbModifiers)
      }
      case SilRelationshipPredicate(
        other : SilReference,
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        SilMappedReference(key, determiner),
        verbModifiers
      ) => {
        val entity = entityMap(key)
        SilRelationshipPredicate(
          other,
          verb.toUninflected,
          chooseReference(referenceMap, entity, other, determiner),
          verbModifiers)
      }
    }
  )

  private def resolveReference(
    entity : EntityType,
    determiner : SilDeterminer,
    resultCollector : ResultCollectorType) : SilReference =
  {
    val key = entity.getUniqueIdentifier
    entityMap.put(key, entity)
    SilMappedReference(key, determiner)
  }

  private def chooseReference(
    referenceMap : mutable.Map[SilReference, Set[EntityType]],
    entity : EntityType,
    other : SilReference,
    determiner : SilDeterminer) : SilReference =
  {
    val equivs = mind.equivalentReferences(
      communicationContext, entity, determiner).map(ref =>
      rewrite(swapPronounsSpeakerListener(referenceMap), ref))
    equivs.find(_ != other) match {
      case Some(ref) => ref
      case _ => equivs.head
    }
  }

  private def removeResolvedReferenceQualifiers = replacementMatcher(
    "removeResolvedReferenceQualifiers", {
      case SilStateSpecifiedReference(
        mr : SilMappedReference,
        _
      ) => {
        mr
      }
      case SilStateSpecifiedReference(
        cr : SilConjunctiveReference,
        _
      ) => {
        cr
      }
      case SilStateSpecifiedReference(
        sr @ SilStateSpecifiedReference(
          _,
          SilAdpositionalState(
            SilAdposition.OF,
            SilPronounReference(
              PERSON_THIRD, GENDER_N, COUNT_PLURAL, DISTANCE_UNSPECIFIED))),
        _
      ) => {
        sr
      }
    }
  )

  private def replaceResolvedReferences(
    referenceMap : mutable.Map[SilReference, Set[EntityType]]
  ) = replacementMatcher(
    "replaceResolvedReferences", {
      case SilMappedReference(key, determiner) => {
        val entity = entityMap(key)
        val ref = mind.specificReference(entity, determiner)
        referenceMap.remove(ref)
        ref
      }
    }
  )

  private def clearInflectedCounts = querier.queryMatcher {
    case predicate : SilPredicate => {
      predicate.setInflectedCount(COUNT_SINGULAR)
    }
  }

  // FIXME we should not be messing with the
  // resultCollector's referenceMap like this!
  private def disqualifyThirdPersonReferences(
    referenceMap : mutable.Map[SilReference, Set[EntityType]]
  ) = querier.queryMatcher {
    case nr @ SilNounReference(
      noun, determiner, _
    ) => {
      determiner match {
        case DETERMINER_UNIQUE =>
        case DETERMINER_UNSPECIFIED => {
          if (!noun.isProper) {
            referenceMap.remove(nr)
          }
        }
        case _ => {
          referenceMap.remove(nr)
        }
      }
    }
    case SilGenitiveReference(possessor, possessee) => {
      referenceMap.remove(possessee)
    }
    case SilStateSpecifiedReference(sub, state) => {
      referenceMap.remove(sub)
    }
    case SilAdpositionalState(_, sub) => {
      referenceMap.remove(sub)
    }
    case cr @ SilConjunctiveReference(determiner, references, _) => {
      if ((determiner != DETERMINER_ALL) ||
        !references.forall(r => referenceMap.contains(r)))
      {
        referenceMap --= references
      } else {
        referenceMap.put(
          cr,
          SprUtils.orderedSet(
            references.flatMap(r => referenceMap(r))))
      }
    }
  }

  // FIXME:  also need to disambiguate with respect to
  // pronouns that were present in the original sentence
  private def disambiguateThirdPersonReferences(
    detector : AmbiguousRefDetector
  ) = querier.queryMatcher {
    case ref : SilReference if (detector.referenceMap.contains(ref)) => {
      ref match {
        case SilNounReference(_, DETERMINER_UNIQUE, _) |
            SilStateSpecifiedReference(_, _) |
            SilConjunctiveReference(_, _, _) =>
          {
            detector.analyze(ref)
          }
        case SilNounReference(
          noun, DETERMINER_UNSPECIFIED, _) if (noun.isProper) =>
          {
            detector.analyze(ref)
          }
        case _ : SilMappedReference =>
          {
            detector.analyze(ref)
          }
        case _ =>
      }
    }
  }

  private def replaceThirdPersonReferences(
    referenceMap : Map[SilReference, Set[EntityType]]
  ) = replacementMatcher(
    "replaceThirdPersonReferences", {
      case ref : SilReference => {
        ref match {
          case SilNounReference(_, _, _) |
              SilStateSpecifiedReference(_, _) |
              SilConjunctiveReference(_, _, _) =>
            {
              referenceMap.get(ref).flatMap(
                entities => mind.thirdPersonReference(entities)).getOrElse(ref)
            }
          case _ => ref
        }
      }
    }
  )

  private def replaceThirdPersonPronouns(
    referenceMap : Map[SilReference, Set[EntityType]]
  ) = replacementMatcher(
    "replaceThirdPersonPronouns", {
      case pr @ SilPronounReference(PERSON_THIRD, _, _, _) => {
        referenceMap.get(pr).map(
          entities => mind.specificReferences(entities)
        ).getOrElse(pr)
      }
    }
  )

  private def answerPropertyQueries(
    states : Set[SilWord]
  ) = replacementMatcher(
    "answerPropertyQueries", {
      case SilPropertyQueryState(_) => {
        assert(states.size == 1)
        SilPropertyState(states.head)
      }
    }
  )

  private def clearActionInflection = replacementMatcher(
    "clearActionInflection", {
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        SilActionPredicate(
          subject, verb.toUninflected, directObject, modifiers)
      }
    }
  )

  private def coerceCountAgreement = replacementMatcher(
    "coerceCountAgreement", {
      case SilRelationshipPredicate(
        subject,
        verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        complement, verbModifiers
      ) => {
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
            verb.toUninflected,
            coerceIfNeeded(complement, complementCount),
            verbModifiers)
        } else {
          SilRelationshipPredicate(
            subject, verb.toUninflected, complement, verbModifiers)
        }
      }
    }
  )

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
            nounRef.noun.toNounUninflected,
            newDeterminer, agreedCount)
        }
      }
      case _ => reference
    }
  }

  private def getTrueEntities(resultCollector : ResultCollectorType) =
  {
    SprUtils.orderedSet(
      resultCollector.entityMap.filter(
        _._2.assumeFalse).keySet)
  }

  private def getFalseEntities(resultCollector : ResultCollectorType) =
  {
    SprUtils.orderedSet(
      resultCollector.entityMap.filterNot(
        _._2.assumeTrue).keySet)
  }

  private def normalizeDisjunction(
    resultCollector : ResultCollectorType,
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : SmcResponseParams)
      : Option[SilReference] =
  {
    val trueEntities = getTrueEntities(resultCollector)
    val exhaustive =
      (trueEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    if (trueEntities.isEmpty) {
      None
    } else if ((trueEntities.size == 1) && !params.alwaysSummarize) {
      Some(resolveReference(
        trueEntities.head, entityDeterminer, resultCollector))
    } else if (exhaustive || (trueEntities.size > params.listLimit)) {
      summarizeList(trueEntities, exhaustive, existence, false)
    } else {
      Some(SilConjunctiveReference(
        DETERMINER_ALL,
        trueEntities.map(
          resolveReference(_, entityDeterminer, resultCollector)).toSeq,
        separator))
    }
  }

  private def normalizeConjunction(
    resultCollector : ResultCollectorType,
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : SmcResponseParams)
      : (Option[SilReference], Boolean) =
  {
    val falseEntities = getFalseEntities(resultCollector)
    val exhaustive =
      (falseEntities.size == resultCollector.entityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    if (falseEntities.isEmpty) {
      tupleN((None, false))
    } else if ((falseEntities.size == 1) && !params.alwaysSummarize) {
      tupleN((Some(resolveReference(
        falseEntities.head, entityDeterminer, resultCollector)),
        false))
    } else if (exhaustive || (falseEntities.size > params.listLimit)) {
      tupleN((summarizeList(falseEntities, exhaustive, existence, true),
        exhaustive))
    } else {
      tupleN((Some(SilConjunctiveReference(
        DETERMINER_NONE,
        falseEntities.map(
          resolveReference(_, entityDeterminer, resultCollector)).toSeq,
        separator)),
        true))
    }
  }

  private def summarizeList(
    entities : Iterable[SmcEntity],
    exhaustive : Boolean,
    existence : Boolean,
    conjunction : Boolean) =
  {
    var all = exhaustive && (entities.size > 1)
    // FIXME:  make this language-independent
    val number = {
      if (conjunction && exhaustive) {
        all = false
        SilWord(LEMMA_NONE)
      } else  if ((entities.size == 2) && exhaustive && !existence) {
        all = false
        SilWord(LEMMA_BOTH)
      } else {
        SilWord.uninflected(entities.size.toString)
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
    val count = number.lemma match {
      case "1" => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
    Some(
      SilStateSpecifiedReference(
        SilNounReference(number, determiner, count),
        SilAdpositionalState(
          SilAdposition.OF,
          SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL))))
  }
}
