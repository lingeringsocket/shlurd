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

import org.specs2.mutable._

class ShlurdConversationSpec extends Specification
{
  private val SPEAKER_FRED = "Fred"

  private val SPEAKER_BARNEY = "Barney"

  private val SENTENCE_A = makeSentence("a")

  private val SENTENCE_B = makeSentence("b")

  private val SENTENCE_C = makeSentence("c")

  private def makeSentence(s : String) =
  {
    SilUnrecognizedSentence(ShlurdSyntaxLeaf(s, s, s))
  }

  "ShlurdConversation" should
  {
    "add utterances" in
    {
      val conversation = new ShlurdConversation
      conversation.addSpeakerSentence(SPEAKER_FRED, SENTENCE_A)
      conversation.addSpeakerSentence(SPEAKER_FRED, SENTENCE_B)
      conversation.addSpeakerSentence(SPEAKER_BARNEY, SENTENCE_C)
      conversation.addSpeakerSentence(SPEAKER_FRED, SENTENCE_C)
      conversation.addSpeakerSentence(SPEAKER_FRED, SENTENCE_B)
      conversation.getUtterances must be equalTo Seq(
        SpeakerUtterance(SPEAKER_FRED, SENTENCE_A),
        SpeakerUtterance(SPEAKER_FRED, SENTENCE_B),
        SpeakerUtterance(SPEAKER_BARNEY, SENTENCE_C),
        SpeakerUtterance(SPEAKER_FRED, SENTENCE_C),
        SpeakerUtterance(SPEAKER_FRED, SENTENCE_B))
    }
  }
}
