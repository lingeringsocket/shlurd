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

import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

import spire.math._

class SmcMind[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType]
](
  cosmos : CosmosType)
{
  type ConversationType = SmcConversation[EntityType]
  type TimelineType = SmcTimeline[EntityType, PropertyType, CosmosType]

  private lazy val personFirst =
    cosmos.uniqueEntity(resolvePronoun(
      SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)))

  private lazy val personSecond =
    cosmos.uniqueEntity(resolvePronoun(
      SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR)))

  private var conversation : Option[ConversationType] = None

  private var timeline
      : Option[TimelineType] = None

  def getCosmos = cosmos

  def newParser(input : String) = cosmos.newParser(input)

  def analyzeSense(sentence : SilSentence) = sentence

  def startConversation()
  {
    conversation = Some(new ConversationType)
  }

  def stopConversation()
  {
    conversation = None
  }

  def startNarrative()
  {
    timeline = Some(new TimelineType)
  }

  def stopNarrative()
  {
    timeline = None
  }

  def hasNarrative() =
  {
    !timeline.isEmpty
  }

  def getNarrative() : TimelineType =
  {
    timeline.get
  }

  def isConversing() : Boolean =
  {
    !conversation.isEmpty
  }

  def getConversation() : ConversationType =
  {
    conversation.get
  }

  protected def initFrom(
    mind : SmcMind[EntityType, PropertyType, CosmosType])
  {
    conversation = mind.conversation
  }

  def spawn(newCosmos : CosmosType) =
  {
    val mind = new SmcMind[EntityType, PropertyType, CosmosType](newCosmos)
    mind.initFrom(this)
    mind
  }

  private def filterReferenceMap(
    phrase : SilPhrase,
    referenceMap : Map[SilReference, Set[EntityType]]) =
  {
    val refSet = new mutable.HashSet[SilReference]
    val phraseQuerier = new SilPhraseRewriter
    val rule = phraseQuerier.queryMatcher {
      case ref : SilReference => {
        refSet += ref
      }
    }
    phraseQuerier.query(rule, phrase)
    referenceMap.filterKeys(ref => {
      refSet.contains(ref) && isRetainableReference(ref)
    })
  }

  private def isRetainableReference(ref : SilReference) : Boolean =
  {
    ref match {
      case SilNounReference(
        _, DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED, _
      ) => {
        true
      }
      case _ : SilPronounReference => true
      case SilConjunctiveReference(_, references, _) => {
        references.forall(isRetainableReference)
      }
      case SilGenitiveReference(possessor, _) => {
        isRetainableReference(possessor)
      }
      case SilStateSpecifiedReference(sub, state) => {
        isRetainableReference(sub)
      }
      case _ => false
    }
  }

  def rememberSpeakerSentence(
    speakerName : String,
    sentence : SilSentence,
    text : String,
    referenceMap : Map[SilReference, Set[EntityType]] = Map.empty)
  {
    val savedText = {
      if (text.isEmpty) {
        sentence.toWordString
      } else {
        text
      }
    }
    conversation.foreach(_.addSpeakerSentence(
      speakerName, sentence, text,
      filterReferenceMap(sentence, referenceMap)))
  }

  def rememberSentenceAnalysis(
    referenceMap : Map[SilReference, Set[EntityType]])
  {
    conversation.foreach(c => c.updateSentenceAnalysis(
      filterReferenceMap(c.getUtterances.last.sentence, referenceMap)))
  }

  def getTemporalCosmos(interval : SmcTimeInterval) : CosmosType =
  {
    timeline match {
      case Some(tl) => {
       tl.findGlb(interval).map(_.updatedCosmos).getOrElse(getCosmos)
      }
      case _ => {
        getCosmos
      }
    }
  }

  def evaluateEntityAdpositionPredicate(
    entity : EntityType,
    objEntity : EntityType,
    adposition : SilAdposition,
    qualifiers : Set[SilWord] = Set.empty) : Try[Trilean] =
  {
    Success(Trilean.Unknown)
  }

  def evaluateEntityCategoryPredicate(
    entity : EntityType,
    noun : SilWord,
    qualifiers : Set[String] = Set.empty) : Try[Trilean] =
  {
    resolveQualifiedNoun(noun, REF_SUBJECT, qualifiers).map(
      set => Trilean(set.contains(entity)))
  }

  def reifyRole(
    possessor : EntityType, roleName : SilWord, onlyIfProven : Boolean)
  {
  }

  def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[EntityType]] =
  {
    cosmos.resolveQualifiedNoun(noun.lemma, context, qualifiers)
  }

  def isEquivalentVerb(
    verb1 : SilWord, verb2 : SilWord) : Boolean =
  {
    verb1.lemma == verb2.lemma
  }

  def resolvePronoun(
    reference : SilPronounReference) : Try[Set[EntityType]] =
  {
    // FIXME proper coreference resolution, including within current sentence;
    // also, there should probably be some limit on how far back to search.
    // Note that for the moment we exclude the current sentence completely.
    conversation.foreach(
      _.getUtterances.reverseIterator.drop(1).foreach(
        utterance => {
          findMatchingPronounReference(utterance, reference) match {
            case Some(set) => return Success(set)
            case _ =>
          }
        }
      )
    )
    cosmos.fail("pronoun cannot be resolved")
  }

  private def findMatchingPronounReference(
    utterance : SpeakerUtterance[EntityType],
    reference : SilPronounReference) : Option[Set[EntityType]] =
  {
    utterance.referenceMap.values.find(set => {
      thirdPersonReference(set) == Some(reference)
    })
  }

  def equivalentReferences(
    entity : EntityType,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    pronounReference(entity, personFirst, PERSON_FIRST) ++
    pronounReference(entity, personSecond, PERSON_SECOND) ++
    Seq(specificReference(entity, determiner))
  }

  def getFirstPerson = personFirst.get

  def getSecondPerson = personSecond.get

  def thirdPersonReference(entities : Set[EntityType]) : Option[SilReference] =
  {
    if (entities.isEmpty) {
      None
    } else if (entities.size == 1) {
      Some(SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_SINGULAR))
    } else {
      Some(SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL))
    }
  }

  private def pronounReference(
    entity : EntityType, pronounEntity : Try[EntityType],
    person : SilPerson)
      : Seq[SilReference] =
  {
    pronounEntity match {
      case Success(x) if (x == entity) => {
        Seq(SilPronounReference(person, GENDER_N, COUNT_SINGULAR))
      }
      case _ => Seq()
    }
  }

  def specificReference(
    entity : EntityType,
    determiner : SilDeterminer) : SilReference =
  {
    SilMappedReference(entity.getUniqueIdentifier, determiner)
  }

  def specificReferences(
    entities : Set[EntityType]) : SilReference =
  {
    assert(!entities.isEmpty)
    if (entities.size == 1) {
      specificReference(entities.head, DETERMINER_UNIQUE)
    } else {
      SilConjunctiveReference(
        DETERMINER_ALL,
        entities.toSeq.map(entity =>
          specificReference(
            entity, DETERMINER_UNIQUE)))
    }
  }
}
