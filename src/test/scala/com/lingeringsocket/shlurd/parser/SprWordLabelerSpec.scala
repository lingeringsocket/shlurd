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
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.nlang._

import SprPennTreebankLabels._

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._

class SprWordLabelerSpec extends Specification
{
  trait LabelingContext extends Scope
  {
    protected val labeler = SnlUtils.defaultWordLabeler

    protected def labelWord(token : String) : Set[String] =
    {
      labeler.labelWords(Seq(tupleN((token, token, 1)))).head.map(_.label)
    }

    protected def lemmatizeNoun(token : String) : String =
    {
      labeler.labelWords(Seq(tupleN((token, token, 1)))).head.
        filter(_.isNoun).head.firstChild.lemma
    }
  }

  "SprWordnetLabeler" should
  {
    "label words correctly" in new LabelingContext
    {
      labelWord("cow") must be equalTo Set(LABEL_NN, LABEL_VBP)
      labelWord("cows") must be equalTo Set(LABEL_NNS, LABEL_VBZ)
      labelWord("ox") must be equalTo Set(LABEL_NN)
      labelWord("oxen") must be equalTo Set(LABEL_NNS)
      labelWord("people") must be equalTo Set(LABEL_NNS, LABEL_VBP)
      labelWord("agenda") must be equalTo Set(LABEL_NN)
      labelWord("cattle") must be equalTo Set(LABEL_NNS)
      labelWord("mice") must be equalTo Set(LABEL_NNS)
      labelWord("boss") must be equalTo Set(LABEL_NN, LABEL_VBP, LABEL_JJ)
      labelWord("bully") must be equalTo Set(LABEL_NN, LABEL_VBP, LABEL_JJ)
    }

    "label quotations correctly" in new LabelingContext
    {
      labelWord("\"what the actual f\"") must be equalTo Set(LABEL_NNQ)
    }

    "lemmatize words correctly" in new LabelingContext
    {
      // FIXME special rules needed for people/cattle/etc
      lemmatizeNoun("cow") must be equalTo "cow"
      lemmatizeNoun("cows") must be equalTo "cow"
      lemmatizeNoun("ox") must be equalTo "ox"
      lemmatizeNoun("oxen") must be equalTo "ox"
      lemmatizeNoun("mice") must be equalTo "mouse"
      lemmatizeNoun("agenda") must be equalTo "agenda"
      lemmatizeNoun("boss") must be equalTo "boss"
    }

    "allow open rules to be added" in new LabelingContext
    {
      labeler.addRule(SprWordRule(
        Seq("bully"),
        Seq(LABEL_RB),
        false
      ))
      labelWord("bully") must be equalTo Set(
        LABEL_NN, LABEL_VBP, LABEL_JJ, LABEL_RB)
    }

    "allow closed rules to be added" in new LabelingContext
    {
      labeler.addRule(SprWordRule(
        Seq("bully"),
        Seq(LABEL_RB),
        true
      ))
      labelWord("bully") must be equalTo Set(LABEL_RB)
    }
  }
}
