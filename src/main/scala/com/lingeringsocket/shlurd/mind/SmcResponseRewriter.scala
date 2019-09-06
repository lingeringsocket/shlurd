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
  communicationContext : SmcCommunicationContext[EntityType],
  annotator : SilAnnotator
) extends SilPhraseRewriter(annotator)
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  private val querier = new SilPhraseQuerier

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
                  SilNounLemmaReference(
                    SmcLemmas.LEMMA_CONTAINEE
                  )
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

    def neutralizePossesseeWildcard = replacementMatcher(
      "neutralizePossesseeWildcard", {
        case SilGenitiveReference(
          possessor,
          SilDeterminedReference(
            SilCountedNounReference(noun, count),
            DETERMINER_ANY)
        ) => {
          annotator.genitiveRef(
            possessor,
            annotator.nounRef(noun, count)
          )
        }
      }
    )

    def expandToConjunction(noun : SilWord, count : SilCount) =
    {
      normalizeConjunctionWrapper(
        SEPARATOR_OXFORD_COMMA,
        annotator.determinedNounRef(noun, DETERMINER_ALL, count))
    }

    def expandToDisjunction(
      nr : SilReference,
      noun : SilWord,
      count : SilCount) =
    {
      normalizeDisjunction(
        nr, resultCollector, entityDeterminer,
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
        annotator.determinedNounRef(responseNoun, responseDeterminer, count)
      }
    }

    def replaceReferences = replacementMatcher(
      "replaceReferences", {
        case SilStateSpecifiedReference(
          ref, _
        ) if (!ref.acceptsSpecifiers) => {
          ref
        }
        case cr @ SilConjunctiveReference(
          determiner, references, separator
        ) => {
          determiner match {
            case DETERMINER_ANY => {
              normalizeDisjunction(
                cr, resultCollector, entityDeterminer,
                separator, params).getOrElse
              {
                negateCollection = true
                annotator.conjunctiveRef(
                  DETERMINER_NONE, references, separator)
              }
            }
            case DETERMINER_ALL => {
              normalizeConjunctionWrapper(
                separator,
                annotator.conjunctiveRef(
                  DETERMINER_ALL, references, separator))
            }
            case _ => {
              annotator.conjunctiveRef(determiner, references, separator)
            }
          }
        }
        case dr @ SilDeterminedReference(
          SilStackedStateReference(
            nr @ SilCountedNounReference(noun, count),
            states),
          DETERMINER_ANY | DETERMINER_SOME
        ) => {
          // this is weird
          val varRef = {
            if (states.isEmpty) {
              dr
            } else {
              nr
            }
          }
          SilStackedStateReference(
            annotator,
            expandToDisjunction(varRef, noun, count),
            states)
        }
        case SilDeterminedReference(
          SilStackedStateReference(
            SilCountedNounReference(noun, count),
            states),
          DETERMINER_ALL
        ) => {
          SilStackedStateReference(
            annotator,
            expandToConjunction(noun, count),
            states)
        }
      }
    )

    val rewrite1 = {
      if (resultCollector.entityMap.filter(_._2.assumeFalse).isEmpty ||
        resultCollector.isCategorization || !allowFlips)
      {
        rewrite(
          combineRules(
            neutralizePossesseeWildcard,
            swapPronounsSpeakerListener(resultCollector.refMap)
          ),
          predicate)
      } else {
        rewrite(
          combineRules(
            neutralizePossesseeWildcard,
            swapPronounsSpeakerListener(resultCollector.refMap),
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
        avoidTautologies(resultCollector.refMap),
        removeResolvedReferenceQualifiers),
      rewrite2)
    val rewrite4 = rewrite(
      replaceResolvedReferences(resultCollector.refMap),
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
          replaceThirdPersonPronouns(resultCollector.refMap),
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
    refMap : SmcMutableRefMap[EntityType]
  ) = replacementMatcher(
    "swapPronounSpeakerListener", {
      case oldPronoun @ SilPronounReference(person, gender, count, distance)=> {
        val speakerListenerReversed = person match {
          case PERSON_FIRST => PERSON_SECOND
          case PERSON_SECOND => PERSON_FIRST
          case PERSON_THIRD => PERSON_THIRD
        }
        val newPronoun =
          annotator.pronounRef(speakerListenerReversed, gender, count, distance)
        refMap.get(oldPronoun).foreach(entities =>
          refMap.put(newPronoun, entities))
        newPronoun
      }
    }
  )

  class AmbiguousRefDetector(
    val refMap : SmcMutableRefMap[EntityType])
  {
    val replacedRefMap = SmcMutableRefMap.newByValue[EntityType]()
    val ambiguousRefs =
      new mutable.LinkedHashSet[SilReference]

    def analyze(originalRef : SilReference)
    {
      val newEntitySet = refMap(originalRef)
      mind.thirdPersonReference(
        annotator,newEntitySet
      ).foreach(replacedRef => {
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
      })
    }
  }

  private def transformQuestionResponse(
    predicate : SilPredicate,
    params : SmcResponseParams,
    question : Option[SilQuestion],
    negateCollection : Boolean) : SilPredicate =
  {
    params.verbosity matchPartial {
      case RESPONSE_TERSE | RESPONSE_ELLIPSIS => {
        // in this case we just want to keep the container as the
        // subject for easy extraction, so don't transform back
        return predicate
      }
    }
    tupleN((predicate, question)) match {
      case (rp @
          SilRelationshipPredicate(
            container,
            verb @ SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
            SilGenitiveReference(
              containee,
              SilNounLemmaReference(
                SmcLemmas.LEMMA_CONTAINER
              )),
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
              SilNounLemmaReference(
                SmcLemmas.LEMMA_CONTAINEE
              )),
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
    val refMap = resultCollector.refMap
    val detector = new AmbiguousRefDetector(refMap)
    querier.query(disqualifyThirdPersonReferences(refMap), predicate)
    querier.query(disambiguateThirdPersonReferences(detector), predicate)
    refMap --= detector.ambiguousRefs
    predicate matchPartial {
      case SilRelationshipPredicate(
        subject, SilRelationshipPredefVerb(REL_PREDEF_IDENTITY), complement, _
      ) => {
        tupleN(
          (refMap.get(subject),
            refMap.get(complement))
        ) matchPartial {
          case (Some(subjectEntities), Some(complementEntities)) => {
            if (subjectEntities == complementEntities) {
              // prevent a tautology
              refMap -= complement
            }
          }
        }
      }
    }
    // use top down rewrite so that replacement of leaf references
    // does not mess up replacement of containing references
    rewrite(
      replaceThirdPersonReferences(resultCollector),
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
      ) if (
        SmcPhraseQuerier.containsWildcard(lhs) &&
          !SmcPhraseQuerier.containsWildcard(rhs)
      ) =>
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
    refMap : SmcMutableRefMap[EntityType]
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
          chooseReference(refMap, entity, other, determiner),
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
          chooseReference(refMap, entity, other, determiner),
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
    annotator.mappedRef(key, determiner)
  }

  private def chooseReference(
    refMap : SmcMutableRefMap[EntityType],
    entity : EntityType,
    other : SilReference,
    determiner : SilDeterminer) : SilReference =
  {
    val equivs = mind.equivalentReferences(
      annotator, communicationContext, entity, determiner).map(ref =>
      rewrite(swapPronounsSpeakerListener(refMap), ref))
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
    refMap : SmcMutableRefMap[EntityType]
  ) = replacementMatcher(
    "replaceResolvedReferences", {
      case SilMappedReference(key, determiner) => {
        val entity = entityMap(key)
        val ref = mind.specificReference(annotator, entity, determiner)
        refMap.remove(ref)
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
  // resultCollector's refMap like this!
  private def disqualifyThirdPersonReferences(
    refMap : SmcMutableRefMap[EntityType]
  ) = querier.queryMatcher {
    case nr @ SilOptionallyDeterminedReference(
      SilNounReference(noun), determiner
    ) => {
      determiner match {
        case DETERMINER_UNIQUE =>
        case DETERMINER_UNSPECIFIED => {
          if (!noun.isProper) {
            refMap.remove(nr)
          }
        }
        case _ => {
          refMap.remove(nr)
        }
      }
    }
    case SilGenitiveReference(possessor, possessee) => {
      refMap.remove(possessee)
    }
    case SilStateSpecifiedReference(sub, state) => {
      refMap.remove(sub)
    }
    case SilAdpositionalState(_, sub) => {
      refMap.remove(sub)
    }
    case cr @ SilConjunctiveReference(determiner, references, _) => {
      if ((determiner != DETERMINER_ALL) ||
        !references.forall(r => refMap.contains(r)))
      {
        refMap --= references
      } else {
        refMap.put(
          cr,
          SprUtils.orderedSet(
            references.flatMap(r => refMap(r))))
      }
    }
  }

  // FIXME:  also need to disambiguate with respect to
  // pronouns that were present in the original sentence
  private def disambiguateThirdPersonReferences(
    detector : AmbiguousRefDetector
  ) = querier.queryMatcher {
    case ref : SilReference if (detector.refMap.contains(ref)) => {
      ref matchPartial {
        case SilDeterminedReference(_ : SilNounReference, DETERMINER_UNIQUE) |
            SilStateSpecifiedReference(_, _) |
            SilConjunctiveReference(_, _, _) =>
          {
            detector.analyze(ref)
          }
        case SilNounReference(
          noun
        ) if (noun.isProper) => {
          detector.analyze(ref)
        }
        case _ : SilMappedReference => {
          detector.analyze(ref)
        }
      }
    }
  }

  private def replaceThirdPersonReferences(
    resultCollector : ResultCollectorType
  ) = replacementMatcher(
    "replaceThirdPersonReferences", {
      case ref : SilReference => {
        ref match {
          case SilOptionallyDeterminedReference(_ : SilNounReference, _) |
              SilOptionallyDeterminedReference(
                SilStateSpecifiedReference(_, _),
                _
              ) |
              SilConjunctiveReference(_, _, _) =>
            {
              resultCollector.lookup(ref).flatMap(
                entities => mind.thirdPersonReference(
                  annotator, entities)).getOrElse(ref)
            }
          case _ => ref
        }
      }
    }
  )

  private def replaceThirdPersonPronouns(
    refMap : SmcRefMap[EntityType]
  ) = replacementMatcher(
    "replaceThirdPersonPronouns", {
      case pr @ SilPronounReference(PERSON_THIRD, _, _, _) => {
        refMap.get(pr).map(
          entities => mind.specificReferences(annotator, entities)
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
        val subjectCount = SilUtils.getCount(subject)
        val complementCount = SilUtils.getCount(complement)
        if (subjectCount != complementCount) {
          val subjectCoercible =
            SilUtils.isCountCoercible(subject)
          val complementCoercible =
            SilUtils.isCountCoercible(complement)
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
    reference : SilReference,
    agreedCount : SilCount
  ) : SilReference =
  {
    reference match {
      case SilStateSpecifiedReference(subReference, state) => {
        annotator.stateSpecifiedRef(
          coerceCount(subReference, agreedCount), state)
      }
      case SilGenitiveReference(possessor, possessee) => {
        annotator.genitiveRef(
          possessor, coerceCount(possessee, agreedCount))
      }
      // FIXME should have a case for arbitrary SilDeterminedReference
      // here too, but it's tricky
      case SilOptionallyDeterminedReference(
        oldNounRef @ SilCountedNounReference(noun, count),
        determiner
      ) => {
        if (count == agreedCount) {
          reference
        } else {
          val newDeterminer = {
            if (agreedCount == COUNT_PLURAL) {
              if (determiner == DETERMINER_NONSPECIFIC) {
                DETERMINER_UNSPECIFIED
              } else {
                determiner
              }
            } else {
              determiner
            }
          }
          val nounRef = annotator.nounRef(
            noun.toNounUninflected,
            agreedCount)
          annotator.determinedRef(nounRef, newDeterminer)
        }
      }
      case _ => reference
    }
  }

  private def getTrueEntities(
    resultCollector : ResultCollectorType,
    ref : SilReference) : Set[EntityType] =
  {
    resultCollector.lookup(ref) match {
      case Some(entities) => {
        SprUtils.orderedSet(
          entities.filter(e =>
            resultCollector.entityMap.get(e).map(_.assumeFalse).getOrElse(false)
          )
        )
      }
      case _ => {
        SprUtils.orderedSet(
          resultCollector.entityMap.filter(
            _._2.assumeFalse).keySet)
      }
    }
  }

  private def getFalseEntities(resultCollector : ResultCollectorType) =
  {
    SprUtils.orderedSet(
      resultCollector.entityMap.filterNot(
        _._2.assumeTrue).keySet)
  }

  private def normalizeDisjunction(
    ref : SilReference,
    resultCollector : ResultCollectorType,
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : SmcResponseParams)
      : Option[SilReference] =
  {
    val trueEntities = getTrueEntities(resultCollector, ref)
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
      Some(annotator.conjunctiveRef(
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
      tupleN((Some(annotator.conjunctiveRef(
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
    val nounRef = annotator.nounRef(number, count)
    Some(
      annotator.stateSpecifiedRef(
        annotator.determinedRef(nounRef, determiner),
        SilAdpositionalState(
          SilAdposition.OF,
          annotator.pronounRef(PERSON_THIRD, GENDER_N, COUNT_PLURAL))))
  }
}
