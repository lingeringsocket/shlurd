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

class SprWordLabelerSpecification extends Specification
{
  trait SprLabelingContext extends Scope
  {
    protected def labeler : SprWordLabeler

    protected def labelWord(token : String) : Set[String] =
    {
      labeler.labelWords(Seq(tupleN(token, token, 1))).head.map(_.label)
    }

    private def lemmatizePos(
      token : String, accept : (SprSyntaxTree) => Boolean) : Set[String] =
    {
      labeler.labelWords(Seq(tupleN(token, token, 1))).head.
        filter(accept).map(_.firstChild.lemma)
    }

    protected def lemmatizeNoun(token : String) : String =
    {
      val set = lemmatizePos(token, x => x.isNoun)
      assert(set.size == 1, set)
      set.head
    }

    protected def lemmatizeAmbiguousNoun(token : String) : Set[String] =
    {
      lemmatizePos(token, x => x.isNoun)
    }

    protected def lemmatizeAdjective(token : String) : String =
    {
      val set = lemmatizePos(token, x => x.isAdjective)
      assert(set.size == 1, set)
      set.head
    }

    protected def lemmatizeVerb(token : String) : String =
    {
      val set = lemmatizePos(token, x => x.isVerb)
      assert(set.size == 1, set)
      set.head
    }
  }
}

class SprWordLabelerSpec extends SprWordLabelerSpecification
{
  trait LabelingContext extends SprLabelingContext
  {
    private val wordLabeler = SnlUtils.defaultWordLabeler

    override protected def labeler = wordLabeler
  }

  "SprWordnetLabeler" should
  {
    "label words" in new LabelingContext
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

    "label quotations" in new LabelingContext
    {
      labelWord("\"what the actual f\"") must be equalTo Set(LABEL_NNQ)
    }

    "lemmatize words" in new LabelingContext
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
