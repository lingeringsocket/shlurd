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

import scala.collection._

import com.lingeringsocket.shlurd.parser._

import ShlurdEnglishLemmas._

class ShlurdResponseRewriter[E<:ShlurdEntity, P<:ShlurdProperty](
  mind : ShlurdMind[E,P]) extends SilPhraseRewriter
{
  private val cosmos = mind.getCosmos

  private val querier = new SilPhraseRewriter

  def normalizeResponse(
    predicate : SilPredicate,
    resultCollector : ResultCollector[E],
    params : ShlurdResponseParams,
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
          case _ => true
        }
      }
      case _ => false
    }
    var negateCollection = false
    val entityDeterminer = predicate match {
      case SilStatePredicate(subject, SilExistenceState(), _) => {
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
          val (responseDeterminer, responseNoun) = noun match {
            case SilWord(LEMMA_WHO, LEMMA_WHO) => {
              (DETERMINER_NONE, SilWord(LEMMA_ONE))
            }
            case SilWord(LEMMA_WHERE, LEMMA_WHERE) => {
              (DETERMINER_UNSPECIFIED, SilWord(LEMMA_NOWHERE))
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

    val rewrite1 = {
      if (getTrueEntities(resultCollector).isEmpty ||
        resultCollector.isCategorization || !allowFlips)
      {
        rewrite(
          swapPronounsSpeakerListener,
          predicate)
      } else {
        rewrite(
          combineRules(
            swapPronounsSpeakerListener,
            flipPredicateQueries
          ),
          predicate)
      }
    }
    val rewrite2 = rewrite(
      combineRules(
        coerceCountAgreement,
        replaceReferences),
      rewrite1)
    val rewrite3 = rewrite(
      avoidTautologies,
      rewrite2)
    val rewrite4 = rewrite(
      replaceResolvedReferences(resultCollector.referenceMap),
      rewrite3)
    val rewrite5 = {
      val useThirdPersonPronouns = predicate match {
        case SilStatePredicate(_, SilExistenceState(), _) => false
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
    (normalized, negateCollection)
  }

  def swapPronounsSpeakerListener = replacementMatcher {
    case SilPronounReference(person, gender, count, distance)=> {
      val speakerListenerReversed = person match {
        case PERSON_FIRST => PERSON_SECOND
        case PERSON_SECOND => PERSON_FIRST
        case PERSON_THIRD => PERSON_THIRD
      }
      SilPronounReference(speakerListenerReversed, gender, count, distance)
    }
  }

  class AmbiguousRefDetector(
    val referenceMap : mutable.Map[SilReference, Set[E]])
  {
    val replacedRefMap = new mutable.LinkedHashMap[SilReference, Set[E]]
    val ambiguousRefs = new mutable.LinkedHashSet[SilReference]

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
    params : ShlurdResponseParams,
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
    (predicate, question) match {
      case (rp @
          SilRelationshipPredicate(
            container,
            SilGenitiveReference(
              subject,
              SilNounReference(
                SilWord(LEMMA_CONTAINER, LEMMA_CONTAINER),
                DETERMINER_UNSPECIFIED,
                COUNT_SINGULAR)),
            REL_IDENTITY,
            verbModifiers
          ),
        Some(QUESTION_WHERE)
      ) => {
        if (negateCollection) {
          // FIXME this isn't right--I guess "nowhere" should really
          // be an adverb in this context?
          SilRelationshipPredicate(
            subject,
            container,
            REL_IDENTITY,
            verbModifiers)
        } else {
          SilStatePredicate(
            subject,
            SilAdpositionalState(
              SilAdposition.IN,
              container),
            verbModifiers)
        }
      }
      case _ => {
        predicate
      }
    }
  }

  private def rewriteThirdPersonReferences(
    resultCollector : ResultCollector[E],
    predicate : SilPredicate) : SilPredicate =
  {
    val referenceMap = resultCollector.referenceMap
    val detector = new AmbiguousRefDetector(referenceMap)
    querier.query(disqualifyThirdPersonReferences(referenceMap), predicate)
    querier.query(disambiguateThirdPersonReferences(detector), predicate)
    referenceMap --= detector.ambiguousRefs
    predicate match {
      case SilRelationshipPredicate(subject, complement, REL_IDENTITY, _) => {
        (referenceMap.get(subject), referenceMap.get(complement)) match {
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
      Set(REWRITE_TOP_DOWN))
  }

  // "Groot is I" becomes "I am Groot"
  private def flipPronouns = replacementMatcher {
    case SilRelationshipPredicate(
      lhs,
      rhs : SilPronounReference,
      REL_IDENTITY,
      _
    ) => {
      SilRelationshipPredicate(
        rhs,
        lhs,
        REL_IDENTITY)
    }
    case SilRelationshipPredicate(
      lhs,
      rhs @ SilConjunctiveReference(_, references, _),
      REL_IDENTITY,
      _
    ) if (references.exists(_.isInstanceOf[SilPronounReference])) => {
      SilRelationshipPredicate(
        rhs,
        lhs,
        REL_IDENTITY)
    }
  }

  // "Who is Slothrop?" becomes "Slothrop is who" so that the response
  // comes out more naturally as "Slothrop is a lieutenant"
  private def flipPredicateQueries = replacementMatcher {
    case SilRelationshipPredicate(
      lhs,
      rhs,
      REL_IDENTITY,
      _
    ) if (containsWildcard(lhs, false) && !containsWildcard(rhs, false)) =>
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
      REL_IDENTITY,
      _
    ) if (rr.entities.size == 1) => {
      SilRelationshipPredicate(
        chooseReference(rr.entities.head, other, rr.determiner),
        other,
        REL_IDENTITY)
    }
    case SilRelationshipPredicate(
      other : SilReference,
      rr : SilResolvedReference[E],
      REL_IDENTITY,
      _
    ) if (rr.entities.size == 1) => {
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
    SilResolvedReference(Set(entity), SilWord(""), determiner)
  }

  private def chooseReference(
    entity : E,
    other : SilReference,
    determiner : SilDeterminer) : SilReference =
  {
    val equivs = mind.equivalentReferences(entity, determiner).map(
      ref => rewrite(swapPronounsSpeakerListener, ref))
    equivs.find(_ != other) match {
      case Some(ref) => ref
      case _ => equivs.head
    }
  }

  private def replaceResolvedReferences(
    referenceMap : mutable.Map[SilReference, Set[E]]
  ) = replacementMatcher {
    case rr : SilResolvedReference[E] if (rr.entities.size == 1) => {
      val ref = cosmos.specificReference(rr.entities.head, rr.determiner)
      if (rr.noun.lemma.isEmpty) {
        referenceMap.remove(ref)
      }
      ref
    }
  }

  private def clearInflectedCounts = querier.queryMatcher {
    case predicate : SilPredicate => {
      predicate.setInflectedCount(COUNT_SINGULAR)
    }
  }

  private def disqualifyThirdPersonReferences(
    referenceMap : mutable.Map[SilReference, Set[E]]
  ) = querier.queryMatcher {
    case nr @ SilNounReference(noun, determiner, _) => {
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
          ShlurdParseUtils.orderedSet(
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
            SilConjunctiveReference(_, _, _) |
            SilResolvedReference(_, _, _) =>
          {
            detector.analyze(ref)
          }
        case SilNounReference(
          noun, DETERMINER_UNSPECIFIED, _) if (noun.isProper) =>
          {
            detector.analyze(ref)
          }
        case _ =>
      }
    }
  }

  private def replaceThirdPersonReferences(
    referenceMap : Map[SilReference, Set[E]]
  ) = replacementMatcher {
    case ref : SilReference => {
      ref match {
        case SilNounReference(_, _, _) |
            SilStateSpecifiedReference(_, _) |
            SilConjunctiveReference(_, _, _) |
            SilResolvedReference(_, _, _) =>
          {
            referenceMap.get(ref).flatMap(
              entities => mind.thirdPersonReference(entities)).getOrElse(ref)
          }
        case _ => ref
      }
    }
  }

  private def replaceThirdPersonPronouns(
    referenceMap : Map[SilReference, Set[E]]
  ) = replacementMatcher {
    case pr @ SilPronounReference(PERSON_THIRD, _, _, _) => {
      referenceMap.get(pr).map(
        entities => cosmos.specificReferences(entities, DETERMINER_UNIQUE)
      ).getOrElse(pr)
    }
  }

  private def coerceCountAgreement = replacementMatcher {
    case SilRelationshipPredicate(subject, complement, REL_IDENTITY, _) => {
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
    params : ShlurdResponseParams)
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
    params : ShlurdResponseParams)
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
          SilAdposition.OF,
          SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL))))
  }

  def containsWildcard(
    phrase : SilPhrase,
    includeConjunctions : Boolean = true) : Boolean =
  {
    var wildcard = false
    def matchWildcard = querier.queryMatcher {
      case SilConjunctiveReference(
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _,
        _
      ) => {
        if (includeConjunctions) {
          wildcard = true
        }
      }
      case SilNounReference(
        _,
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _
      ) => {
        wildcard = true
      }
      case SilNounReference(
        SilWord(LEMMA_WHO, LEMMA_WHO) | SilWord(LEMMA_WHERE, LEMMA_WHERE),
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
