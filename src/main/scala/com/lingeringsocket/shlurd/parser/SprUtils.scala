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

import scala.collection._

// FIXME some of these, like LEMMA_HIS, are not real lemmas
object SprEnglishLemmas
{
  val LEMMA_THERE = "there"
  val LEMMA_BE = "be"
  val LEMMA_EXIST = "exist"
  val LEMMA_HAVE = "have"
  val LEMMA_WHO = "who"
  val LEMMA_WHOM = "whom"
  val LEMMA_WHERE = "where"
  val LEMMA_NOWHERE = "nowhere"
  val LEMMA_NOTHING = "nothing"
  val LEMMA_ONE = "one"
  val LEMMA_HOW = "how"
  val LEMMA_MANY = "many"
  val LEMMA_WHICH = "which"
  val LEMMA_WHAT = "what"
  val LEMMA_PROPERTY = "property"
  val LEMMA_GENDER = "gender"
  val LEMMA_MASCULINE = "masculine"
  val LEMMA_FEMININE = "feminine"
  val LEMMA_OTHERWISE = "otherwise"
  val LEMMA_NO = "no"
  val LEMMA_NOT = "not"
  val LEMMA_NOR = "nor"
  val LEMMA_NONE = "none"
  val LEMMA_BOTH = "both"
  val LEMMA_AND = "and"
  val LEMMA_OR = "or"
  val LEMMA_A = "a"
  val LEMMA_THE = "the"
  val LEMMA_ALL = "all"
  val LEMMA_ANY = "any"
  val LEMMA_EVERY = "every"
  val LEMMA_SOME = "some"
  val LEMMA_EITHER = "either"
  val LEMMA_NEITHER = "neither"
  val LEMMA_MUST = "must"
  val LEMMA_MAY = "may"
  val LEMMA_COULD = "could"
  val LEMMA_CAN = "can"
  val LEMMA_MIGHT = "might"
  val LEMMA_SHOULD = "should"
  val LEMMA_DO = "do"
  val LEMMA_IF = "if"
  val LEMMA_WHEN = "when"
  val LEMMA_THEN = "then"
  val LEMMA_EQUIVALENTLY = "equivalently"
  val LEMMA_IT = "it"
  val LEMMA_ITS = "its"
  val LEMMA_I = "I"
  val LEMMA_ME = "me"
  val LEMMA_WE = "we"
  val LEMMA_US = "us"
  val LEMMA_MY = "my"
  val LEMMA_OUR = "our"
  val LEMMA_MINE = "mine"
  val LEMMA_OURS = "ours"
  val LEMMA_YOU = "you"
  val LEMMA_YOUR = "your"
  val LEMMA_YOURS = "yours"
  val LEMMA_THEY = "they"
  val LEMMA_THEIR = "their"
  val LEMMA_THEM = "them"
  val LEMMA_HE = "he"
  val LEMMA_HIM = "him"
  val LEMMA_HIS = "his"
  val LEMMA_SHE = "she"
  val LEMMA_HER = "her"
  val LEMMA_HERS = "hers"
  val LEMMA_THIS = "this"
  val LEMMA_THAT = "that"
  val LEMMA_THESE = "these"
  val LEMMA_THOSE = "those"
  val LEMMA_IN = "in"
  val LEMMA_INSIDE = "inside"
  val LEMMA_WITHIN = "within"
  val LEMMA_OUTSIDE = "outside"
  val LEMMA_AT = "at"
  val LEMMA_WITH = "with"
  val LEMMA_AS = "as"
  val LEMMA_NEAR = "near"
  val LEMMA_NEARBY = "nearby"
  val LEMMA_TO = "to"
  val LEMMA_FROM = "from"
  val LEMMA_ON = "on"
  val LEMMA_BEFORE = "before"
  val LEMMA_AFTER = "after"
  val LEMMA_LEFT = "left"
  val LEMMA_RIGHT = "right"
  val LEMMA_FRONT = "front"
  val LEMMA_BACK = "back"
  val LEMMA_ABOVE = "above"
  val LEMMA_OVER = "over"
  val LEMMA_BELOW = "below"
  val LEMMA_UNDER = "under"
  val LEMMA_BENEATH = "beneath"
  val LEMMA_UNDERNEATH = "underneath"
  val LEMMA_BEHIND = "behind"
  val LEMMA_KIND = "kind"
  val LEMMA_OF = "of"
  val LEMMA_GENITIVE_OF = "_of_"
  val LEMMA_ADVERBIAL_TMP = "_TEMPORAL_"
}

object SprUtils
{
  def capitalize(s : String) = s.head.toUpper + s.tail

  def orderedSet[T](iterable : Iterable[T]) =
    (new mutable.LinkedHashSet[T] ++= iterable)

  def requireUnique(seq : Seq[SprSyntaxTree]) : SprSyntaxTree =
  {
    assert(seq.size == 1, seq)
    seq.head
  }

  def requireLeaf(seq : Seq[SprSyntaxTree]) : SprSyntaxLeaf =
  {
    requireUnique(seq) match {
      case leaf : SprSyntaxLeaf => leaf
      case nonLeaf => {
        throw new IllegalArgumentException("leaf expected but got " + nonLeaf)
      }
    }
  }

}
