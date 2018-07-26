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

import scala.collection._
import scala.util._

class ShlurdMind[E<:ShlurdEntity, P<:ShlurdProperty](
  cosmos : ShlurdCosmos[E,P])
{
  private lazy val personFirst =
    uniqueEntity(resolvePronoun(
      SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)))

  private lazy val personSecond =
    uniqueEntity(resolvePronoun(
      SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR)))

  private var conversation : Option[ShlurdConversation[E]] = None

  def getCosmos = cosmos

  def startConversation()
  {
    conversation = Some(new ShlurdConversation[E])
  }

  def stopConversation()
  {
    conversation = None
  }

  def isConversing() : Boolean =
  {
    !conversation.isEmpty
  }

  def getConversation() : ShlurdConversation[E] =
  {
    conversation.get
  }

  def rememberSpeakerSentence(
    speakerName : String,
    sentence : SilSentence,
    text : String,
    referenceMap : Map[SilReference, Set[E]] = Map.empty)
  {
    val savedText = {
      if (text.isEmpty) {
        sentence.toWordString
      } else {
        text
      }
    }
    conversation.foreach(_.addSpeakerSentence(
      speakerName, sentence, text, referenceMap))
  }

  def rememberSentenceAnalysis(
    referenceMap : Map[SilReference, Set[E]])
  {
    conversation.foreach(_.updateSentenceAnalysis(referenceMap))
  }

  def resolvePronoun(
    reference : SilPronounReference) : Try[Set[E]] =
  {
    // FIXME proper coreference resolution, including within current sentence;
    // also, there should probably be some limit on how far back to search,
    // and we should exclude nonspecific entries from the reference map
    conversation.foreach(_.getUtterances.reverseIterator.foreach(utterance => {
      findMatchingPronounReference(utterance, reference) match {
        case Some(set) => return Success(set)
        case _ =>
      }
    }))
    cosmos.fail("pronoun cannot be resolved")
  }

  private def findMatchingPronounReference(
    utterance : SpeakerUtterance[E],
    reference : SilPronounReference) : Option[Set[E]] =
  {
    utterance.referenceMap.values.find(set => {
      thirdPersonReference(set) == Some(reference)
    })
  }

  def equivalentReferences(
    entity : E,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    pronounReference(entity, personFirst, PERSON_FIRST) ++
    pronounReference(entity, personSecond, PERSON_SECOND) ++
    Seq(cosmos.specificReference(entity, determiner))
  }

  def thirdPersonReference(entities : Set[E]) : Option[SilReference] =
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
    entity : E, pronounEntity : Try[E],
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

  protected def uniqueEntity(result : Try[Set[E]]) : Try[E] =
  {
    result.flatMap(set => {
      if (set.size == 1) {
        Success(set.head)
      } else {
        cosmos.fail("unique entity expected")
      }
    })
  }
}
