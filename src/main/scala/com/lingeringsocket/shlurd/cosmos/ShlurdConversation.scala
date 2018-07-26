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

object ShlurdConversation
{
  val SPEAKER_NAME_SHLURD = "SHLURD"

  val SPEAKER_NAME_PERSON = "PERSON"
}

case class SpeakerUtterance[E <: ShlurdEntity](
  speakerName : String,
  sentence : SilSentence,
  referenceMap : Map[SilReference, Set[E]] = Map.empty[SilReference, Set[E]])
{
}

class ShlurdConversation[E <: ShlurdEntity]
{
  private val utterances = new mutable.ArrayBuffer[SpeakerUtterance[E]]

  def addSpeakerSentence(
    speakerName : String,
    sentence : SilSentence,
    referenceMap : Map[SilReference, Set[E]] = Map.empty)
  {
    utterances += SpeakerUtterance(speakerName, sentence, referenceMap)
  }

  def updateSentenceAnalysis(referenceMap : Map[SilReference, Set[E]])
  {
    val last = utterances.last
    utterances.reduceToSize(utterances.size - 1)
    utterances += SpeakerUtterance(
      last.speakerName, last.sentence, referenceMap)
  }

  def getUtterances() : Seq[SpeakerUtterance[E]] = utterances
}
