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
import com.lingeringsocket.shlurd.ilang._

import SprEnglishLemmas._

object SprEnglishLexicon
{
  val prepositions = readLexicon("/english/prepositions.txt")

  val subordinates = readLexicon("/english/subordinates.txt")

  val proper = readLexicon("/english/proper.txt")

  private def readLexicon(resource : String) : Set[String] =
  {
    val words = ResourceUtils.getResourceSource(resource).getLines
    Set(words.toSeq:_*)
  }
}

trait SprEnglishWordAnalyzer
{
  def maybeDeterminerFor(lemma : String) : Option[SilDeterminer] =
  {
    val matcher : PartialFunction[String, SilDeterminer] = {
      case LEMMA_NO | LEMMA_NEITHER | LEMMA_NOR => DETERMINER_NONE
      case LEMMA_BOTH | LEMMA_AND | LEMMA_ALL | LEMMA_EVERY => DETERMINER_ALL
      // FIXME LEMMA_ONE should really map to SilIntegerDeterminer
      case LEMMA_ONE | LEMMA_A => DETERMINER_NONSPECIFIC
      case LEMMA_THE | LEMMA_EITHER => DETERMINER_DEFINITE
      case LEMMA_SOME => DETERMINER_SOME
      case LEMMA_ANY => DETERMINER_ANY
      case LEMMA_WHICH => DETERMINER_VARIABLE
    }
    matcher.lift(lemma)
  }

  def isCoordinatingDeterminer(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_EITHER | LEMMA_NEITHER | LEMMA_BOTH => true
      case _ => false
    }
  }

  def isCoordinatingConjunction(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_AND | LEMMA_OR | LEMMA_NOR => true
      case _ => false
    }
  }

  def isFlexiblePronoun(token : String) : Boolean =
  {
    token match {
      case LEMMA_HER | LEMMA_THIS | LEMMA_THAT |
          LEMMA_THESE | LEMMA_THOSE => true
      case _ => false
    }
  }

  def isReflexivePronoun(token : String) : Boolean =
  {
    token match {
      case LEMMA_MYSELF | LEMMA_YOURSELF | LEMMA_HIMSELF |
          LEMMA_HERSELF | LEMMA_ITSELF | LEMMA_OURSELF |
          LEMMA_OURSELVES | LEMMA_YOURSELVES | LEMMA_THEMSELF |
          LEMMA_THEMSELVES => true
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

  def isAdposition(lemma : String) : Boolean =
  {
    SprEnglishLexicon.prepositions.contains(lemma)
  }

  def isSubordinatingConjunction(lemma : String) : Boolean =
  {
    SprEnglishLexicon.subordinates.contains(lemma)
  }

  def isProper(lemma : String) : Boolean =
  {
    SprEnglishLexicon.proper.contains(lemma)
  }

  def isPronounWord(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_MY | LEMMA_MYSELF |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS |
          LEMMA_OURSELF | LEMMA_OURSELVES |
          LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS |
          LEMMA_YOURSELF | LEMMA_YOURSELVES |
          LEMMA_US | LEMMA_THEY | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_IT | LEMMA_ITS | LEMMA_THEM | LEMMA_THEIR |
          LEMMA_THEMSELF | LEMMA_THEMSELVES |
          LEMMA_HE | LEMMA_HIM | LEMMA_HIS | LEMMA_HIMSELF |
          LEMMA_SHE | LEMMA_HER | LEMMA_HERS | LEMMA_HERSELF |
          LEMMA_THIS | LEMMA_THAT => true
      case _ => false
    }
  }

  def makeLeaf(
    label : String, token : String, lemma : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, lemma, token)
  }

  def makeLeaf(
    label : String, token : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, token, token)
  }

  def makeLeaf(
    token : String) : SprSyntaxLeaf =
  {
    makeLeaf(token, token, token)
  }
}
