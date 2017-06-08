// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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
package shlurd.print

import org.specs2.mutable._

import shlurd.parser._

class ShlurdSentenceTranslatorSpec extends Specification
{
  private val printer =
    new ShlurdSentencePrinter(LimitedKoreanParlance)

  private def translate(s : String) =
    printer.print(ShlurdParser(s).parse)

  "ShlurdSentencePrinter" should
  {
    "translate sentences" in
    {
      translate("the door is closed") must be equalTo
        "문이 닫았어요."
      translate("is the door closed") must be equalTo
        "문이 닫았어요?"
      translate("are all doors closed") must be equalTo
        "모든 문들이 닫았어요?"
      translate("close the door") must be equalTo
        "문을 닫으세요."
      translate("close the blind") must be equalTo
        "블라인드를 닫으세요."
      translate("I am hungry") must be equalTo
        "내가 배고파요."
      translate("is she hungry") must be equalTo
        "그녀가 배고파요?"
      translate("close it") must be equalTo
        "그것을 닫으세요."
    }
  }
}

object LimitedKoreanParlance extends ShlurdParlance
{
  override def newSentenceBundle() = new KoreanSentenceBundle {
    override def inflectNoun(
      lemma : String, count : ShlurdCount, inflection : ShlurdInflection) =
    {
      lemma match {
        case "door" => super.inflectNoun("문", count, inflection)
        case "blind" => super.inflectNoun("블라인드", count, inflection)
        case _ => super.inflectNoun(lemma, count, inflection)
      }
    }

    override def conjugateImperative(lemma : String) =
    {
      lemma match {
        case "close" => phrase("닫으세요")
        case _ => super.conjugateImperative(lemma)
      }
    }

    override def conjugateAdjective(lemma : String) =
    {
      lemma match {
        case "close" => phrase("닫았어요")
        case "hungry" => phrase("배고파요")
        case _ => super.conjugateAdjective(lemma)
      }
    }
  }
}
