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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import scala.collection._

class SmcResponseRewriter[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType]
](
  mind : SmcMind[EntityType, PropertyType, CosmosType],
  communicationContext : SmcCommunicationContext[EntityType],
  annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]]
) extends SilPhraseRewriter(annotator)
{
  import SilPhraseRewriter._

  type ResultCollectorType = SmcResultCollector[EntityType]

  private val querier = new SilPhraseQuerier

  private val entityMap =
    new mutable.LinkedHashMap[String, EntityType]

  private val swapHereThere = mind.isDistantCommunication(communicationContext)

  private implicit val tongue = mind.getTongue

  private def markQueryAnswer(ref : SilReference) =
  {
    SilUtils.collectReferences(ref).foreach(subRef => {
      subRef matchPartial {
        case ar : SilAnnotatedReference => {
          annotator.getNote(ar).markQueryAnswer()
        }
      }
    })
    ref
  }

  private def containsQueryAnswer(ref : SilAnnotatedReference) : Boolean =
  {
    SilUtils.collectReferences(ref).exists(_ match {
      case ar : SilAnnotatedReference => {
        annotator.getNote(ar).isQueryAnswer
      }
      case _ => false
    })
  }

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
                SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
                SilGenitiveReference(
                  _,
                  SilNounLemmaReference(
                    SmcIdeals.ROLE_CONTAINEE
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
        DETERMINER_DEFINITE
      }
    }

    def normalizeConjunctionWrapper(
      fullRef : SilReference,
      separator : SilSeparator,
      allRef : => SilReference) =
    {
      val (rr, rn) = normalizeConjunction(
        fullRef, resultCollector, entityDeterminer, separator, params)
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
            _ : SilUnlimitedDeterminer)
        ) => {
          annotator.genitiveRef(
            possessor,
            annotator.nounRef(noun, count)
          )
        }
      }
    )

    def expandToConjunction(
      fullRef : SilReference, noun : SilWord, count : SilCount) =
    {
      normalizeConjunctionWrapper(
        fullRef,
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
          case SprPredefWord(PD_WHO) => {
            tupleN(DETERMINER_NONE, SprPredefWord(PD_ONE))
          }
          case SprPredefWord(PD_WHOM) => {
            tupleN(DETERMINER_NONE, SprPredefWord(PD_ONE))
          }
          case SprPredefWord(PD_WHERE) => {
            tupleN(DETERMINER_ABSENT, SprPredefWord(PD_NOWHERE))
          }
          case SprPredefWord(PD_WHAT) => {
            tupleN(DETERMINER_ABSENT, SprPredefWord(PD_NOTHING))
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
                cr,
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
          _ : SilUnlimitedDeterminer
        ) => {
          // this is weird
          val varRef = {
            if (states.isEmpty) {
              dr
            } else {
              nr
            }
          }
          markQueryAnswer(SilStackedStateReference(
            annotator,
            expandToDisjunction(varRef, noun, count),
            states))
        }
        case dr @ SilDeterminedReference(
          SilStackedStateReference(
            SilCountedNounReference(noun, count),
            states),
          DETERMINER_ALL
        ) => {
          markQueryAnswer(SilStackedStateReference(
            annotator,
            expandToConjunction(dr, noun, count),
            states))
        }
      }
    )
    val rewrite1 = {
      if (resultCollector.neutralizedEntityMap.
        filter(_._2.assumeFalse).isEmpty ||
        resultCollector.isCategorization || !allowFlips)
      {
        rewrite(
          combineRules(
            neutralizePossesseeWildcard,
            swapSpeakerListener(resultCollector.refMap)
          ),
          predicate)
      } else {
        rewrite(
          combineRules(
            neutralizePossesseeWildcard,
            swapSpeakerListener(resultCollector.refMap),
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

    val tongueRules = tongue.getResponseRules(
      refToPronoun(resultCollector.refMap))
    val rewriteTongue = {
      if (tongueRules.isEmpty) {
        rewriteLast
      } else {
        rewrite(
          combineRules(tongueRules.toSeq:_*),
          rewriteLast,
          SilRewriteOptions(repeat = true))
      }
    }

    querier.query(clearInflectedAttributes, rewriteTongue)

    val normalized = transformQuestionResponse(
      rewriteTongue, params, question, negateCollection)
    SilPhraseValidator.validatePhrase(normalized)
    tupleN(normalized, negateCollection)
  }

  private def refToPronoun(
    refMap : SmcMutableRefMap[EntityType])
    (ref : SilReference, updateMap : Boolean) : SilReference =
  {
    refMap.get(ref).flatMap(
      entities => {
        val newRef = mind.thirdPersonDeictic(
          annotator, entities)
        if (updateMap) {
          newRef.foreach(r => refMap.put(r, entities))
        }
        newRef
      }
    ).getOrElse {
      ref match {
        case SilDeterminedReference(sub, _) => {
          val replaced = refToPronoun(refMap)(sub, updateMap)
          replaced match {
            case pronoun : SilPronounReference => pronoun
            case _ => ref
          }
        }
        case _ => ref
      }
    }
  }

  def swapSpeakerListener(
    refMap : SmcMutableRefMap[EntityType]
  ) = replacementMatcher(
    "swapPronounSpeakerListener", {
      // FIXME need to transform word if we want to support
      // custom first/second person pronouns
      case oldPronoun @ SilPronounReference(
        person, gender, count, proximity, politeness
      ) => {
        val (swap, speakerListenerReversed) = person match {
          case PERSON_FIRST => tupleN(true, PERSON_SECOND)
          case PERSON_SECOND => tupleN(true, PERSON_FIRST)
          case PERSON_THIRD => tupleN(false, PERSON_THIRD)
        }
        if (swap) {
          val newPronoun = annotator.pronounRef(
            speakerListenerReversed, gender, count, mind,
            proximity, politeness)
          refMap.get(oldPronoun).foreach(entities =>
            refMap.put(newPronoun, entities))
          newPronoun
        } else {
          oldPronoun
        }
      }
      case SilBasicVerbModifier(
        SprProximityWord(PROXIMITY_SPEAKER_HERE)
      ) if (swapHereThere) => {
        SilBasicVerbModifier(SprProximityWord(PROXIMITY_LISTENER_THERE))
      }
      case SilBasicVerbModifier(
        SprProximityWord(PROXIMITY_LISTENER_THERE)
      ) if (swapHereThere) => {
        SilBasicVerbModifier(SprProximityWord(PROXIMITY_SPEAKER_HERE))
      }
    }
  )

  class AmbiguousRefDetector(
    val refMap : SmcMutableRefMap[EntityType])
  {
    val replacedRefMap = SmcMutableRefMap.newByValue[EntityType]
    val ambiguousRefs =
      new mutable.LinkedHashSet[SilReference]

    def analyze(originalRef : SilReference) : Unit =
    {
      val newEntitySet = refMap(originalRef)
      mind.thirdPersonDeictic(
        annotator, newEntitySet
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
    tupleN(predicate, question) match {
      case (rp @
          SilRelationshipPredicate(
            container,
            verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
            SilGenitiveReference(
              containee,
              SilNounLemmaReference(
                SmcIdeals.ROLE_CONTAINER
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
              SprPredefAdposition(PD_IN),
              container),
            verbModifiers)
        }
      }
      case (rp @
          SilRelationshipPredicate(
            containee,
            SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
            SilGenitiveReference(
              container,
              SilNounLemmaReference(
                SmcIdeals.ROLE_CONTAINEE
              )),
            verbModifiers
          ),
        _
      ) => {
        SilStatePredicate(
          containee,
          STATE_PREDEF_BE.toVerb,
          SilAdpositionalState(
            SprPredefAdposition(PD_IN),
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
    val refMap = SmcResultCollector.modifiableRefMap(
      resultCollector.refMap)
    querier.query(disqualifyThirdPersonReferences(refMap), predicate)
    querier.query(disqualifyQueryAnswers(refMap), predicate)
    val detector = new AmbiguousRefDetector(refMap)
    querier.query(disambiguateThirdPersonReferences(detector), predicate)
    refMap --= detector.ambiguousRefs
    predicate matchPartial {
      case SilRelationshipPredicate(
        subject, SprRelationshipPredefVerb(REL_PREDEF_IDENTITY), complement, _
      ) => {
        tupleN(
          refMap.get(subject),
          refMap.get(complement)
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
    val rewritten = rewrite(
      replaceThirdPersonReferences(refMap),
      predicate,
      SilRewriteOptions(topDown = true))
    if (tongue.allowElidedSubject) {
      rewrite(
        SilRewriteRules.elideSubjectPronouns(annotator),
        rewritten)
    } else {
      rewritten
    }
  }

  // "Groot is I" becomes "I am Groot"
  private def flipPronouns = replacementMatcher(
    "flipPronouns", {
      case SilRelationshipPredicate(
        lhs,
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
      case rp @ SilRelationshipPredicate(
        lhs,
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
    annotator.mappedRef(key, determiner, mind.deriveGender(entity))
  }

  private def chooseReference(
    refMap : SmcMutableRefMap[EntityType],
    entity : EntityType,
    other : SilReference,
    determiner : SilDeterminer) : SilReference =
  {
    val equivs = mind.equivalentReferences(
      annotator, communicationContext, entity, determiner)
    equivs.find(foldReference(_) != foldReference(other)) match {
      case Some(ref) => ref
      case _ => equivs.head
    }
  }

  private def foldReference(ref : SilReference) : SilReference =
  {
    ref match {
      case pr : SilPronounReference if (pr.isElided) => {
        annotator.pronounRef(
          pr.person,
          pr.gender,
          pr.count,
          mind,
          PROXIMITY_ENTITY,
          pr.politeness,
          pr.word,
          pr.pronounMap)
      }
      case _ => ref
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
            SprPredefAdposition(PD_OF),
            SilPronounReference(
              PERSON_THIRD, _, COUNT_PLURAL, PROXIMITY_ENTITY, _))),
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
        val ref = mind.responseReference(
          annotator, communicationContext, entity, determiner)
        refMap.put(ref, Set(entity))
        markQueryAnswer(ref)
        ref
      }
    }
  )

  private def clearInflectedAttributes = querier.queryMatcher {
    case predicate : SilPredicate => {
      predicate.setInflectedPerson(PERSON_THIRD)
      predicate.setInflectedGender(GENDER_NEUTER)
      predicate.setInflectedCount(COUNT_SINGULAR)
    }
  }

  private def disqualifyQueryAnswers(
    refMap : SmcMutableRefMap[EntityType]
  ) = querier.queryMatcher {
    case ar : SilAnnotatedReference => {
      if (containsQueryAnswer(ar)) {
        refMap.remove(ar)
      }
    }
  }

  private def disqualifyThirdPersonReferences(
    refMap : SmcMutableRefMap[EntityType]
  ) = querier.queryMatcher {
    case nr @ SilOptionallyDeterminedReference(
      SilNounReference(noun), determiner
    ) => {
      determiner match {
        case DETERMINER_DEFINITE => ;
        case DETERMINER_ABSENT => {
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
    case SilAdpositionalState(_, sub) => {
      refMap.remove(sub)
    }
  }

  // FIXME:  also need to disambiguate with respect to
  // pronouns that were present in the original sentence
  private def disambiguateThirdPersonReferences(
    detector : AmbiguousRefDetector
  ) = querier.queryMatcher {
    case ref : SilReference if (detector.refMap.contains(ref)) => {
      ref matchPartial {
        case SilDeterminedReference(_ : SilNounReference, DETERMINER_DEFINITE) |
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
    refMap : SmcMutableRefMap[EntityType]
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
              refToPronoun(refMap)(ref, false)
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
      case pr : SilPronounReference if (pr.person == PERSON_THIRD) => {
        refMap.get(pr).map(
          entities => {
            if (entities.isEmpty) {
              pr
            } else {
              mind.responseReferences(
                annotator, communicationContext, entities)
            }
          }
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
        verb @ SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
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
      case SilStatePredicate(
        subject,
        verb,
        state,
        modifiers
      ) => {
        val subjectGender = SilUtils.getGender(subject, mind)
        val subjectCount = SilUtils.getCount(subject)
        SilStatePredicate(
          subject,
          verb,
          coerceState(state, subjectGender, subjectCount),
          modifiers
        )
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
                DETERMINER_ABSENT
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
      case pr : SilPronounReference => {
        annotator.pronounRef(
          pr.person,
          pr.gender,
          agreedCount,
          mind,
          pr.proximity,
          pr.politeness,
          pr.word,
          pr.pronounMap)
      }
      case _ => reference
    }
  }

  private def coerceState(
    state : SilState,
    agreedGender : SilGender,
    agreedCount : SilCount
  ) : SilState =
  {
    state match {
      case SilPropertyState(sw : SilSimpleWord) => {
        val original = sw.inflected
        val corrected = tongue.correctGenderCount(
          original,
          agreedGender,
          agreedCount,
          false)
        SilPropertyState(SilWord(corrected, sw.lemmaUnfolded, sw.senseId))
      }
      case _ => state
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
            resultCollector.neutralizedEntityMap.get(e).
              map(_.assumeFalse).getOrElse(false)
          )
        )
      }
      case _ => {
        SprUtils.orderedSet(
          resultCollector.neutralizedEntityMap.filter(
            _._2.assumeFalse).keySet)
      }
    }
  }

  private def getFalseEntities(resultCollector : ResultCollectorType) =
  {
    SprUtils.orderedSet(
      resultCollector.neutralizedEntityMap.filterNot(
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
      (trueEntities.size == resultCollector.neutralizedEntityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    val resultOpt = {
      if (trueEntities.isEmpty) {
        None
      } else if ((trueEntities.size == 1) && !params.alwaysSummarize) {
        Some(resolveReference(
          trueEntities.head, entityDeterminer, resultCollector))
      } else if (exhaustive || (trueEntities.size > params.listLimit)) {
        summarizeList(ref, trueEntities, exhaustive, existence, false)
      } else {
        Some(annotator.conjunctiveRef(
          DETERMINER_ALL,
          trueEntities.map(
            resolveReference(_, entityDeterminer, resultCollector)).toSeq,
          separator))
      }
    }
    resultOpt.foreach(result => {
      resultCollector.refMap.put(result, trueEntities)
    })
    resultOpt
  }

  private def normalizeConjunction(
    fullRef : SilReference,
    resultCollector : ResultCollectorType,
    entityDeterminer : SilDeterminer,
    separator : SilSeparator,
    params : SmcResponseParams)
      : (Option[SilReference], Boolean) =
  {
    val falseEntities = getFalseEntities(resultCollector)
    val exhaustive =
      (falseEntities.size == resultCollector.neutralizedEntityMap.size) &&
        !params.neverSummarize
    val existence = resultCollector.states.isEmpty
    if (falseEntities.isEmpty) {
      tupleN(None, false)
    } else if ((falseEntities.size == 1) && !params.alwaysSummarize) {
      tupleN(Some(resolveReference(
        falseEntities.head, entityDeterminer, resultCollector)),
        false)
    } else if (exhaustive || (falseEntities.size > params.listLimit)) {
      tupleN(
        summarizeList(fullRef, falseEntities, exhaustive, existence, true),
        exhaustive)
    } else {
      tupleN(Some(annotator.conjunctiveRef(
        DETERMINER_NONE,
        falseEntities.map(
          resolveReference(_, entityDeterminer, resultCollector)).toSeq,
        separator)),
        true)
    }
  }

  private def summarizeList(
    fullRef : SilReference,
    entities : Iterable[EntityType],
    exhaustive : Boolean,
    existence : Boolean,
    conjunction : Boolean) =
  {
    val gender = SilUtils.getGender(fullRef, mind)
    var all = exhaustive && (entities.size > 1)
    // FIXME:  make this language-independent
    val (number, count) = {
      if (conjunction && exhaustive) {
        all = false
        val c = tongue.getNoneCount
        val n = SilWord(
          tongue.correctGenderCount(
            SprPredefWord(PD_NONE_NOUN).toLemma,
            gender, c, false))
        tupleN(n, c)
      } else  if ((entities.size == 2) && exhaustive && !existence) {
        all = false
        val c = COUNT_PLURAL
        val n = SilWord(
          tongue.correctGenderCount(
            SprPredefWord(PD_BOTH).toLemma,
            gender, c, false))
        tupleN(n, c)
      } else {
        val s = entities.size
        val n = SilWord.uninflected(s.toString)
        val c = s match {
          case 0 => tongue.getNoneCount
          case 1 => COUNT_SINGULAR
          case _ => COUNT_PLURAL
        }
        tupleN(n, c)
      }
    }
    val determiner = {
      if (all && !existence) {
        DETERMINER_ALL
      } else {
        DETERMINER_ABSENT
      }
    }
    // stuff like both/none should really be represented as
    // pronouns, not nouns
    val nounRef = annotator.nounRef(number, count, Some(gender))
    Some(markQueryAnswer(
      tongue.synthesizeSummaryRef(
        annotator,
        determiner,
        nounRef,
        gender,
        mind)))
  }
}
