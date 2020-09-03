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
import SprPennTreebankLabels._

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

class SprEnglishTongue(wordnet : ShlurdWordnet)
    extends SprTongue(wordnet)
{
  override def getPronounMap(
    gender : SilBasicGender,
    count : SilCount
  ) : SilPronounMap =
  {
    tupleN((gender, count)) match {
      case (GENDER_MASCULINE, COUNT_SINGULAR) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_HE),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_HIM),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_HIMSELF),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_HIS)
        )
      }
      case (GENDER_FEMININE, COUNT_SINGULAR) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_SHE),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_HER),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_HERSELF),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_HER)
        )
      }
      case (GENDER_NEUTER, COUNT_SINGULAR) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_IT),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_IT),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_ITSELF),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_ITS)
        )
      }
      case (GENDER_NEUTER, COUNT_PLURAL) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_THEY),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_THEM),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_THEMSELVES),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_THEIR)
        )
      }
      case _ => SilPronounMap()
    }
  }

  override def maybeDeterminerFor(lemma : String) : Option[SilDeterminer] =
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

  override def isCoordinatingDeterminer(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_EITHER | LEMMA_NEITHER | LEMMA_BOTH => true
      case _ => false
    }
  }

  override def isCoordinatingConjunction(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_AND | LEMMA_OR | LEMMA_NOR => true
      case _ => false
    }
  }

  override def isFlexiblePronoun(token : String) : Boolean =
  {
    token match {
      case LEMMA_HER | LEMMA_THIS | LEMMA_THAT |
          LEMMA_THESE | LEMMA_THOSE => true
      case _ => false
    }
  }

  override def isReflexivePronoun(token : String) : Boolean =
  {
    token match {
      case LEMMA_MYSELF | LEMMA_YOURSELF | LEMMA_HIMSELF |
          LEMMA_HERSELF | LEMMA_ITSELF | LEMMA_OURSELF |
          LEMMA_OURSELVES | LEMMA_YOURSELVES | LEMMA_THEMSELF |
          LEMMA_THEMSELVES => true
      case _ => false
    }
  }

  override def isPossessiveAdjective(token : String) : Boolean =
  {
    token match {
      case LEMMA_MY | LEMMA_OUR | LEMMA_YOUR |
          LEMMA_ITS | LEMMA_THEIR | LEMMA_HIS | LEMMA_HER => true
      case _ => false
    }
  }

  override def isAdposition(lemma : String) : Boolean =
  {
    SprEnglishLexicon.prepositions.contains(lemma)
  }

  override def isSubordinatingConjunction(lemma : String) : Boolean =
  {
    SprEnglishLexicon.subordinates.contains(lemma)
  }

  override def isProper(lemma : String) : Boolean =
  {
    SprEnglishLexicon.proper.contains(lemma)
  }

  override def isPronounWord(lemma : String) : Boolean =
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

  override def analyzePronoun(lemma : String) =
  {
    val isCustomPronoun = !isPronounWord(lemma)
    val person = lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_MY | LEMMA_MYSELF |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS |
          LEMMA_OURSELF | LEMMA_OURSELVES => PERSON_FIRST
      case LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS |
          LEMMA_YOURSELF | LEMMA_YOURSELVES => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case LEMMA_WE | LEMMA_US | LEMMA_THEY | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_OUR | LEMMA_THEM | LEMMA_THEIR |
          LEMMA_OURSELF | LEMMA_OURSELVES | LEMMA_YOURSELVES |
          LEMMA_THEMSELF | LEMMA_THEMSELVES => COUNT_PLURAL
      case _ => COUNT_SINGULAR
    }
    val gender = lemma match {
      case LEMMA_HE | LEMMA_HIM | LEMMA_HIS | LEMMA_HIMSELF => GENDER_MASCULINE
      case LEMMA_SHE | LEMMA_HER | LEMMA_HERS | LEMMA_HERSELF => GENDER_FEMININE
      case LEMMA_HERE | LEMMA_THERE => GENDER_SOMEWHERE
      case _ => {
        person match {
          case PERSON_FIRST | PERSON_SECOND => GENDER_SOMEONE
          case _ => {
            if (isCustomPronoun) {
              GENDER_SOMEONE
            } else {
              // FIXME what we really want here is an uknown between
              // NEUTER and SOMEONE, to be resolved downstream
              GENDER_NEUTER
            }
          }
        }
      }
    }
    val distanceOpt = lemma match {
      case LEMMA_HERE | LEMMA_THIS | LEMMA_THESE => Some(DISTANCE_HERE)
      case LEMMA_THERE | LEMMA_THAT | LEMMA_THOSE => Some(DISTANCE_THERE)
      case _ => None
    }
    tupleN((person, count, gender, distanceOpt))
  }

  override def synthesizeMembersRef(
    annotator : SilAnnotator,
    determiner : SilDeterminer,
    gender : SilGender) : SilReference =
  {
    val lemma = determiner match {
      case DETERMINER_NONE => LEMMA_NONE
      case SilIntegerDeterminer(1) | DETERMINER_NONSPECIFIC => LEMMA_ONE
      case DETERMINER_ANY => LEMMA_ANY
      case DETERMINER_SOME => LEMMA_SOME
      case DETERMINER_ALL => LEMMA_ALL
      case SilIntegerDeterminer(n) => n.toString
      case _ => throw new IllegalArgumentException(determiner.toString)
    }
    annotator.nounRef(SilWord(lemma))
  }
}
