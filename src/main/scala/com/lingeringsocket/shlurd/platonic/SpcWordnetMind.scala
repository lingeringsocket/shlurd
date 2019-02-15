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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

import scala.collection._
import scala.util._

class SpcWordnetMind(cosmos : SpcCosmos)
    extends SpcMind(cosmos)
{
  private def getWordnet() = new SpcWordnet(cosmos)

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcWordnetMind(newCosmos)
    mind.initFrom(this)
    mind
  }

  override def analyzeSense(sentence : SilSentence) =
  {
    val analyzer = new SilWordnetSenseAnalyzer
    analyzer.analyze(sentence)
  }

  override def resolveForm(noun : SilWord) : Option[SpcForm] =
  {
    super.resolveForm(noun).orElse {
      val senses = ShlurdWordnet.findSenses(noun.senseId)
      val wordnet = getWordnet
      senses.toStream.flatMap(wordnet.getSynsetForm).headOption
    }
  }

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[SpcEntity]] =
  {
    val superResult = super.resolveQualifiedNoun(noun, context, qualifiers)
    if (superResult.isSuccess) {
      superResult
    } else {
      resolveForm(noun) match {
        case Some(form) => {
          cosmos.resolveQualifiedNoun(form.name, context, qualifiers)
        }
        case _ => superResult
      }
    }
  }
}
