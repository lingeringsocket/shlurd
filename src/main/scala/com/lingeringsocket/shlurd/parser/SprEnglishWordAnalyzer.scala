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

import com.lingeringsocket.shlurd.ilang._

import SprEnglishLemmas._

trait SprEnglishWordAnalyzer
{
  def maybeDeterminerFor(lemma : String) : Option[SilDeterminer] =
  {
    val matcher : PartialFunction[String, SilDeterminer] = {
      case LEMMA_NO | LEMMA_NEITHER | LEMMA_NOR => DETERMINER_NONE
      case LEMMA_BOTH | LEMMA_AND | LEMMA_ALL | LEMMA_EVERY => DETERMINER_ALL
      // FIXME LEMMA_ONE should really map to SilIntegerDeterminer
      case LEMMA_ONE | LEMMA_A => DETERMINER_NONSPECIFIC
      case LEMMA_THE | LEMMA_EITHER => DETERMINER_UNIQUE
      case LEMMA_SOME => DETERMINER_SOME
      case LEMMA_ANY => DETERMINER_ANY
    }
    matcher.lift(lemma)
  }

  def isCoordinatingConjunction(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_AND | LEMMA_OR | LEMMA_NOR => true
      case _ => false
    }
  }

  def isPossessiveAdjective(token : String) : Boolean =
  {
    token match {
      case LEMMA_MY | LEMMA_OUR | LEMMA_YOUR |
          LEMMA_ITS | LEMMA_THEIR | LEMMA_HIS | LEMMA_HER => true
      case _ => false
    }
  }

  def isPronounWord(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_MY |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS |
          LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS |
          LEMMA_US | LEMMA_THEY | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_IT | LEMMA_ITS | LEMMA_THEIR |
          LEMMA_HE | LEMMA_HIM | LEMMA_HIS |
          LEMMA_SHE | LEMMA_HER | LEMMA_HERS |
          LEMMA_THIS | LEMMA_THESE | LEMMA_THAT => true
      case _ => false
    }
  }
}
