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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

class SmcConversationSpec extends Specification
{
  private val SPEAKER_FRED = "Fred"

  private val SPEAKER_BARNEY = "Barney"

  private val SENTENCE_A = makeSentence("a")

  private val SENTENCE_B = makeSentence("b")

  private val SENTENCE_C = makeSentence("c")

  private val REFERENCE_D = makeReference("d")

  private val TEXT_A = "a"

  private val TEXT_B = "b"

  private val TEXT_C = "c"

  private val ENTITY_1 = new SmcEntity {
    override def getUniqueIdentifier = "1"
  }

  private val ENTITY_2 = new SmcEntity {
    override def getUniqueIdentifier = "2"
  }

  private val REF_MAP_1 = Map[SilReference, Set[SmcEntity]](
    REFERENCE_D -> Set(ENTITY_1)
  )

  private val REF_MAP_2 = Map[SilReference, Set[SmcEntity]](
    REFERENCE_D -> Set(ENTITY_2)
  )

  private def makeLeaf(s : String) =
    SprSyntaxLeaf(s, s, s)

  private def makeSentence(s : String) =
    SilUnrecognizedSentence(makeLeaf(s))

  private def makeReference(s : String) =
    SilUnrecognizedReference(makeLeaf(s))

  private def utterance(
    speakerName : String,
    sentence : SilSentence,
    text : String,
    refMap : Map[SilReference, Set[SmcEntity]] =
      Map.empty[SilReference, Set[SmcEntity]]) =
  {
    SpeakerUtterance(speakerName, sentence, text, refMap)
  }

  "SmcConversation" should
  {
    "remember utterances" in
    {
      val conversation = new SmcConversation[SmcEntity]
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_1)
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_B, TEXT_B)
      conversation.addSpeakerSentence(
        SPEAKER_BARNEY, SENTENCE_C, TEXT_C)
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_C, TEXT_C)
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_B, TEXT_B)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_1),
        utterance(SPEAKER_FRED, SENTENCE_B, TEXT_B),
        utterance(SPEAKER_BARNEY, SENTENCE_C, TEXT_C),
        utterance(SPEAKER_FRED, SENTENCE_C, TEXT_C),
        utterance(SPEAKER_FRED, SENTENCE_B, TEXT_B))
    }

    "update sentence analysis" in
    {
      val conversation = new SmcConversation[SmcEntity]
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_A, TEXT_A)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A))
      conversation.updateSentenceAnalysis(REF_MAP_1)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_1))
      conversation.updateSentenceAnalysis(REF_MAP_2)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_2))
      conversation.addSpeakerSentence(
        SPEAKER_FRED, SENTENCE_B, TEXT_B)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_2),
        utterance(SPEAKER_FRED, SENTENCE_B, TEXT_B))
      conversation.updateSentenceAnalysis(REF_MAP_1)
      conversation.getUtterances must be equalTo Seq(
        utterance(SPEAKER_FRED, SENTENCE_A, TEXT_A, REF_MAP_2),
        utterance(SPEAKER_FRED, SENTENCE_B, TEXT_B, REF_MAP_1))
    }
  }
}
