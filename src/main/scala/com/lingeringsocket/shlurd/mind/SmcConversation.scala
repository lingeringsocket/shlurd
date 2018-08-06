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

object SmcConversation
{
  val SPEAKER_NAME_SHLURD = "SHLURD"

  val SPEAKER_NAME_PERSON = "PERSON"
}

case class SpeakerUtterance[EntityType <: SmcEntity](
  speakerName : String,
  sentence : SilSentence,
  text : String,
  referenceMap : Map[SilReference, Set[EntityType]] =
    Map.empty[SilReference, Set[EntityType]])
{
}

class SmcConversation[EntityType <: SmcEntity]
{
  type UtteranceType = SpeakerUtterance[EntityType]

  private val utterances =
    new mutable.ArrayBuffer[UtteranceType]

  def addSpeakerSentence(
    speakerName : String,
    sentence : SilSentence,
    text : String,
    referenceMap : Map[SilReference, Set[EntityType]] = Map.empty)
  {
    utterances += SpeakerUtterance(speakerName, sentence, text, referenceMap)
  }

  def updateSentenceAnalysis(referenceMap : Map[SilReference, Set[EntityType]])
  {
    val last = utterances.last
    utterances.reduceToSize(utterances.size - 1)
    utterances += SpeakerUtterance(
      last.speakerName, last.sentence, last.text, referenceMap)
  }

  def getUtterances() : Seq[UtteranceType] = utterances
}
