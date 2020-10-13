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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.data._

import scala.collection._
import scala.collection.JavaConverters._

import org.atteo.evo.inflector.{English => EnglishPluralizer}

import SprPennTreebankLabels._

// FIXME some of these, like LEMMA_HIS, are not real lemmas
object SnlEnglishLemmas
{
  val LEMMA_HERE = "here"
  val LEMMA_THERE = "there"
  val LEMMA_BE = "be"
  val LEMMA_BECOME = "become"
  val LEMMA_EXIST = "exist"
  val LEMMA_HAVE = "have"
  val LEMMA_WHO = "who"
  val LEMMA_WHOM = "whom"
  val LEMMA_WHOSE = "whose"
  val LEMMA_WHERE = "where"
  val LEMMA_NOWHERE = "nowhere"
  val LEMMA_NOTHING = "nothing"
  val LEMMA_ONE = "one"
  val LEMMA_HOW = "how"
  val LEMMA_MANY = "many"
  val LEMMA_WHICH = "which"
  val LEMMA_WHAT = "what"
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
  val LEMMA_OTHER = "other"
  val LEMMA_FORMER = "former"
  val LEMMA_LATTER = "latter"
  val LEMMA_MUST = "must"
  val LEMMA_MAY = "may"
  val LEMMA_COULD = "could"
  val LEMMA_CAN = "can"
  val LEMMA_MIGHT = "might"
  val LEMMA_SHOULD = "should"
  val LEMMA_DO = "do"
  val LEMMA_IF = "if"
  val LEMMA_WHEN = "when"
  val LEMMA_WHENEVER = "whenever"
  val LEMMA_THEN = "then"
  val LEMMA_EQUIVALENTLY = "equivalently"
  val LEMMA_BELIEVE = "believe"
  val LEMMA_IT = "it"
  val LEMMA_ITS = "its"
  val LEMMA_ITSELF = "itself"
  val LEMMA_I = "I"
  val LEMMA_ME = "me"
  val LEMMA_WE = "we"
  val LEMMA_US = "us"
  val LEMMA_MY = "my"
  val LEMMA_MYSELF = "myself"
  val LEMMA_OUR = "our"
  val LEMMA_MINE = "mine"
  val LEMMA_OUGHT = "ought"
  val LEMMA_OURS = "ours"
  val LEMMA_OURSELF = "ourself"
  val LEMMA_OURSELVES = "ourselves"
  val LEMMA_YOU = "you"
  val LEMMA_YOUR = "your"
  val LEMMA_YOURS = "yours"
  val LEMMA_YOURSELF = "yourself"
  val LEMMA_YOURSELVES = "yourselves"
  val LEMMA_THEY = "they"
  val LEMMA_THEIR = "their"
  val LEMMA_THEIRS = "theirs"
  val LEMMA_THEM = "them"
  val LEMMA_THEMSELVES = "themselves"
  val LEMMA_THEMSELF = "themself"
  val LEMMA_HE = "he"
  val LEMMA_HIM = "him"
  val LEMMA_HIMSELF = "himself"
  val LEMMA_HIS = "his"
  val LEMMA_SHE = "she"
  val LEMMA_HER = "her"
  val LEMMA_HERS = "hers"
  val LEMMA_HERSELF = "herself"
  val LEMMA_THIS = "this"
  val LEMMA_THAT = "that"
  val LEMMA_THESE = "these"
  val LEMMA_THOSE = "those"
  val LEMMA_AMONG = "among"
  val LEMMA_EXCEPT = "except"
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
  val LEMMA_BEHIND = "behind"
  val LEMMA_OF = "of"
  val LEMMA_GENITIVE_OF = "_of_"
}
import SnlEnglishLemmas._

object SnlEnglishAffixes
{
  val SUFFIX_ING = "ing"
}
import SnlEnglishAffixes._

object SnlEnglishLexicon
{
  import SnlUtils._

  val prepositions = readLexicon("/english/prepositions.txt")

  val subordinates = readLexicon("/english/subordinates.txt")

  val proper = readLexicon("/proper_names.txt")

  // adapted from
  // http://www.d.umn.edu/~tpederse/Group01/WordNet/wordnet-stoplist.html
  val stopList = Set(
    "I", "i", "an", "as", "at", "by", "he", "it", "do", "at", "off",
    "his", "me", "or", "thou", "us", "who", "must", "ca", "may", "in",
    "does", "have", "my", "might"
  ) ++ stopListPunct

  val pronounLemmas = Set(
    LEMMA_I, LEMMA_ME, LEMMA_WE, LEMMA_MY, LEMMA_MYSELF,
    LEMMA_OUR, LEMMA_MINE, LEMMA_OURS,
    LEMMA_OURSELF, LEMMA_OURSELVES,
    LEMMA_YOU, LEMMA_YOUR, LEMMA_YOURS,
    LEMMA_YOURSELF, LEMMA_YOURSELVES,
    LEMMA_US, LEMMA_THEY, LEMMA_THESE, LEMMA_THOSE,
    LEMMA_IT, LEMMA_ITS, LEMMA_THEM, LEMMA_THEIR,
    LEMMA_THEMSELF, LEMMA_THEMSELVES,
    LEMMA_HE, LEMMA_HIM, LEMMA_HIS, LEMMA_HIMSELF,
    LEMMA_SHE, LEMMA_HER, LEMMA_HERS, LEMMA_HERSELF,
    LEMMA_THIS, LEMMA_THAT
  )

  val predefToLemma : Map[SprPredef, String] = Map(
    PD_ABOVE -> LEMMA_ABOVE,
    PD_ADVERBIAL_TMP -> LEMMA_ADVERBIAL_TMP,
    PD_AFTER -> LEMMA_AFTER,
    PD_ALSO -> "also",
    PD_AMONG -> LEMMA_AMONG,
    PD_AND -> LEMMA_AND,
    PD_ANOTHER -> "another",
    PD_AS -> LEMMA_AS,
    PD_AT -> LEMMA_AT,
    PD_BACK -> LEMMA_BACK,
    PD_BE -> LEMMA_BE,
    PD_BEFORE -> LEMMA_BEFORE,
    PD_BEHIND -> LEMMA_BEHIND,
    PD_BELIEVE -> "believe",
    PD_BOTH -> LEMMA_BOTH,
    PD_CONSEQUENTLY -> "consequently",
    PD_EITHER -> LEMMA_EITHER,
    PD_EQUIVALENTLY -> LEMMA_EQUIVALENTLY,
    PD_EXCEPT -> LEMMA_EXCEPT,
    PD_EXIST -> LEMMA_EXIST,
    PD_FEMININE -> "feminine",
    PD_FORMER -> LEMMA_FORMER,
    PD_FROM -> LEMMA_FROM,
    PD_FRONT -> LEMMA_FRONT,
    PD_GENERALLY -> "generally",
    PD_GENITIVE_OF -> LEMMA_GENITIVE_OF,
    // FIXME: handle compounds properly
    PD_HOW_MANY -> "how many",
    PD_IF -> LEMMA_IF,
    PD_IN -> LEMMA_IN,
    PD_INSIDE -> LEMMA_INSIDE,
    PD_KIND -> "kind",
    PD_LATTER -> LEMMA_LATTER,
    PD_LEFT -> LEMMA_LEFT,
    PD_MASCULINE -> "masculine",
    PD_NEAR -> LEMMA_NEAR,
    PD_NEARBY -> LEMMA_NEARBY,
    PD_NEITHER_NOUN -> LEMMA_NEITHER,
    PD_NEITHER_DETERMINER -> LEMMA_NEITHER,
    PD_NONE_NOUN -> LEMMA_NONE,
    PD_NONE_DETERMINER -> LEMMA_NO,
    PD_NOR -> LEMMA_NOR,
    PD_NOTHING -> LEMMA_NOTHING,
    PD_NOWHERE -> LEMMA_NOWHERE,
    PD_NEUTER -> "neuter",
    PD_OF -> LEMMA_OF,
    PD_ON -> LEMMA_ON,
    PD_ONE -> LEMMA_ONE,
    PD_OR -> LEMMA_OR,
    PD_OTHER -> LEMMA_OTHER,
    PD_OTHERWISE -> "otherwise",
    PD_OUTSIDE -> LEMMA_OUTSIDE,
    PD_OVER -> LEMMA_OVER,
    PD_RIGHT -> LEMMA_RIGHT,
    PD_SAME -> "same",
    PD_SUBSEQUENTLY -> "subsequently",
    PD_THAT -> LEMMA_THAT,
    PD_THEN -> LEMMA_THEN,
    PD_TO -> LEMMA_TO,
    PD_WHAT -> LEMMA_WHAT,
    PD_WHEN -> LEMMA_WHEN,
    PD_WHENEVER -> LEMMA_WHENEVER,
    PD_WHERE -> LEMMA_WHERE,
    PD_WHICH -> LEMMA_WHICH,
    PD_WHO -> LEMMA_WHO,
    PD_WHOM -> LEMMA_WHOM,
    PD_WHOSE -> LEMMA_WHOSE,
    PD_WITH -> LEMMA_WITH,
    PD_WITHIN -> LEMMA_WITHIN
  )

  // note that this mapping may not be one-to-one; in case of collisions,
  // it's necessary to sort them out via special cases in the
  // tongue's predefForLemma implementation
  val lemmaToPredef = predefToLemma.map(_.swap)
}

class SnlEnglishTongue(wordnet : SprWordnet)
    extends SprTongue(wordnet)
{
  import SnlEnglishLexicon._
  import SprWordnetScorer._

  private implicit val tongue = this

  private val phraseScorers = Seq(
    scoreVerbModifiers,
    scoreSpecialEnglishAdpositions,
    scoreEnglishUsage
  )

  override def newSentencePrinter(
    genderAnalyzer : SilGenderAnalyzer) =
  {
    new SilSentencePrinter(this, genderAnalyzer)
  }

  def newSentenceBundle() : SilSentenceBundle =
  {
    new SnlEnglishSentenceBundle(this)
  }

  override def newSyntaxAnalyzer(
    context : SprContext,
    guessedQuestion : Boolean,
    strictness : SprStrictness = SPR_STRICTNESS_LOOSE,
    enforceTransitive : Boolean = true
  ) : SprSyntaxAnalyzer =
  {
    new SnlEnglishSyntaxAnalyzer(
      context, guessedQuestion, strictness, enforceTransitive)
  }

  override def getAdjectivePosition = MOD_BEFORE_ALWAYS

  override def getStopList = stopList

  override def getPhraseScorers : Seq[SprWordnetScorer.PhraseScorer] =
  {
    phraseScorers
  }

  override def getRelPredefLemma(predef : SprRelationshipPredef) : String =
  {
    predef match {
      case REL_PREDEF_IDENTITY => LEMMA_BE
      case REL_PREDEF_BECOME => LEMMA_BECOME
      case REL_PREDEF_ASSOC => LEMMA_HAVE
    }
  }

  override def getStatePredefLemma(predef : SprStatePredef) : String =
  {
    predef match {
      case STATE_PREDEF_BE => LEMMA_BE
      case STATE_PREDEF_BECOME => LEMMA_BECOME
    }
  }

  override def getStatePredefFromLemma(lemma : String) : SprStatePredef =
  {
    lemma match {
      case LEMMA_EXIST | LEMMA_BE => STATE_PREDEF_BE
      case LEMMA_BECOME => STATE_PREDEF_BECOME
      case _ => throw new IllegalArgumentException(
        "Non-predef state verb " + lemma)
    }
  }

  override def isProgressiveAuxLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_BE
  }

  override def isBeingLemma(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_BE | LEMMA_EXIST | LEMMA_BECOME => true
      case _ => false
    }
  }

  override def isPossessionLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_HAVE
  }

  override def isExistsLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_EXIST
  }

  override def isPotentialGerund(inflected : String) : Boolean =
  {
    if (!inflected.endsWith(SUFFIX_ING)) {
      false
    } else {
      wordnet.isPotentialAdjective(inflected)
    }
  }

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

  override def maybeDeterminerFor(
    lemma : String) : Option[SilDeterminer] =
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

  override def isDemonstrative(lemma : String) =
  {
    lemma match {
      case LEMMA_THIS | LEMMA_THAT | LEMMA_THESE | LEMMA_THOSE => true
      case _ => false
    }
  }

  override def isModalAuxLemma(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_DO => true
      case _ => false
    }
  }

  override def tamForAuxLemma(
    auxLemma : String, verbLemma : String) : SilTam =
  {
    val tam = SilTam.indicative
    auxLemma match {
      case LEMMA_HAVE | LEMMA_MUST => tam.withModality(MODAL_MUST)
      case LEMMA_MAY => tam.withModality(MODAL_MAY)
      case LEMMA_COULD | LEMMA_CAN => tam.withModality(MODAL_CAPABLE)
      case LEMMA_MIGHT => tam.withModality(MODAL_POSSIBLE)
      case LEMMA_SHOULD | LEMMA_OUGHT => tam.withModality(MODAL_SHOULD)
      case LEMMA_DO => tam.withModality(MODAL_EMPHATIC)
      case LEMMA_BE => tam.progressive
      case _ => tam
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

  override def isSpecialAdposition(lemma : String) : Boolean =
  {
    // Penn Treebank has special "TO" label instead of "IN"
    (lemma == LEMMA_TO)
  }

  override def isAdpositionablePronoun(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_ME | LEMMA_MYSELF | LEMMA_MINE | LEMMA_OURS |
          LEMMA_OURSELF | LEMMA_OURSELVES | LEMMA_YOU |
          LEMMA_YOURS | LEMMA_YOURSELF | LEMMA_YOURSELVES |
          LEMMA_US | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_IT | LEMMA_ITS | LEMMA_THEM | LEMMA_THEMSELF |
          LEMMA_THEMSELVES | LEMMA_HIM | LEMMA_HIS |
          LEMMA_HIMSELF | LEMMA_HER | LEMMA_HERS | LEMMA_HERSELF |
          LEMMA_THIS | LEMMA_THAT => true
      case _ => {
        if (isPronounWord(lemma)) {
          false
        } else {
          true
        }
      }
    }
  }

  override def isAdposition(lemma : String) : Boolean =
  {
    prepositions.contains(lemma)
  }

  override def adpositionForAux(auxLemma : String) : String =
  {
    // FIXME all of 'em
    auxLemma match {
      case LEMMA_HAVE | LEMMA_OUGHT => LEMMA_TO
      case _ => ""
    }
  }

  override def auxVerbForModal(modality : SilModality) : String =
  {
    // FIXME "have to", etc
    ""
  }

  override def isSubordinatingConjunction(lemma : String) : Boolean =
  {
    subordinates.contains(lemma)
  }

  override def isProper(lemma : String) : Boolean =
  {
    proper.contains(lemma)
  }

  override def getPronounLemmas() : Set[String] =
  {
    pronounLemmas
  }

  override def analyzePronoun(lemma : String) =
  {
    val isCustomPronoun = !isPronounWord(lemma)
    val person = lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_US | LEMMA_MY | LEMMA_MYSELF |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS |
          LEMMA_OURSELF | LEMMA_OURSELVES => PERSON_FIRST
      case LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS |
          LEMMA_YOURSELF | LEMMA_YOURSELVES => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case LEMMA_WE | LEMMA_US | LEMMA_THEY | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_OUR | LEMMA_THEM | LEMMA_THEIR | LEMMA_THEIRS |
          LEMMA_OURS | LEMMA_OURSELF | LEMMA_OURSELVES | LEMMA_YOURSELVES |
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
    val inflection = lemma match {
      case LEMMA_I | LEMMA_WE| LEMMA_HE | LEMMA_THEY | LEMMA_SHE =>
        INFLECT_NOMINATIVE
      case LEMMA_ME | LEMMA_US | LEMMA_THEM | LEMMA_HIM | LEMMA_HER =>
        INFLECT_ACCUSATIVE
      case LEMMA_MY | LEMMA_OUR | LEMMA_YOUR | LEMMA_ITS |
          LEMMA_THEIR | LEMMA_HIS =>
        INFLECT_GENITIVE
      case _ => INFLECT_NONE
    }
    val proximityOpt = lemma match {
      case LEMMA_HERE | LEMMA_THIS | LEMMA_THESE =>
        Some(PROXIMITY_SPEAKER_HERE)
      case LEMMA_THERE | LEMMA_THAT | LEMMA_THOSE =>
        Some(PROXIMITY_LISTENER_THERE)
      case LEMMA_MINE | LEMMA_YOURS | LEMMA_OURS |
          LEMMA_YOURS | LEMMA_THEIRS | LEMMA_HERS =>
        Some(PROXIMITY_POSSESSEE)
      case _ if (isReflexivePronoun(lemma)) =>
        Some(PROXIMITY_REFLEXIVE)
      case _ if (!isCustomPronoun) =>
        Some(PROXIMITY_ENTITY)
      case _ => None
    }
    tupleN((
      person, count, gender, inflection,
      proximityOpt, COUNT_SINGULAR, SilPoliteness.DEFAULT))
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

  override def labelVerb(token : String, lemma : String) : Set[String] =
  {
    val label = {
      if (token != lemma) {
        if (token.endsWith(SUFFIX_ING)) {
          LABEL_VBG
        } else {
          // FIXME this is lame
          if (lemma == LEMMA_BE) {
            token match {
              case "was" | "were" => LABEL_VBD
              case "is" => LABEL_VBZ
              case _ => LABEL_VBP
            }
          } else if (token.endsWith("d") ||
            (token.take(2) != lemma.take(2)))
          {
            LABEL_VBD
          } else {
            LABEL_VBZ
          }
        }
      } else {
        LABEL_VBP
      }
    }
    if ((label == LABEL_VBD) && (lemma != LEMMA_BE) && (lemma != LEMMA_DO)) {
      Set(label, LABEL_VBN)
    } else {
      Set(label)
    }
  }

  override def labelPronoun(
    word : String,
    token : String,
    foldEphemeralLabels : Boolean) : Set[SprSyntaxTree] =
  {
    if (token == "i") {
      Set(SptPRP(makeLeaf(word, token, LEMMA_I)))
    } else if ((token == LEMMA_THEM) && !foldEphemeralLabels) {
      Set(SprSyntaxRewriter.recompose(
        LABEL_PRP_OBJ, Seq(makeLeaf(word, token))))
    } else {
      super.labelPronoun(word, token, foldEphemeralLabels)
    }
  }

  override def labelSpecial(
    word : String,
    token : String) : Set[SprSyntaxTree] =
  {
    def leaf = makeLeaf(word, token)
    token match {
      case (
        LEMMA_MUST | LEMMA_MAY | LEMMA_MIGHT |
          LEMMA_COULD | LEMMA_SHOULD | LEMMA_CAN | LEMMA_OUGHT
      ) => {
        Set(SptMD(leaf))
      }
      case LEMMA_HAVE | LEMMA_DO => {
        Set(SptMD(leaf), SptVBP(leaf))
      }
      case "does" => {
        val inflected = makeLeaf(word, token, LEMMA_DO)
        Set(SptVBZ(inflected))
      }
      case "has" => {
        val inflected = makeLeaf(word, token, LEMMA_HAVE)
        Set(SptMD(inflected), SptVBZ(inflected))
      }
      case "had" => {
        val inflected = makeLeaf(word, token, LEMMA_HAVE)
        Set(SptMD(inflected), SptVBD(inflected))
      }
      case LEMMA_THERE => {
        Set(SptNP(SptEX(leaf)), SptJJ(leaf))
      }
      case LEMMA_THAT => {
        Set(SptIN(leaf),
          SptWDT(leaf))
      }
      case LEMMA_WHO | LEMMA_WHOM => Set(SptWP(leaf))
      case LEMMA_WHOSE => Set(SptWP_POS(leaf))
      case LEMMA_HOW | LEMMA_WHERE => {
        Set(SptWRB(leaf))
      }
      case LEMMA_WHAT | LEMMA_WHICH => {
        Set(SptWP(leaf),
          SptWDT(leaf))
      }
      case LEMMA_EQUIVALENTLY => {
        Set(SptRB(leaf))
      }
      case LEMMA_NO => {
        Set(SptRB(leaf))
      }
      case "an" => {
        Set(SptDT(makeLeaf(word, token, LEMMA_A)))
      }
      case "off" => {
        Set(SptJJ(leaf), SptRB(leaf))
      }
      // FIXME proper handling for all contractions
      case "ca" => Set(SptMD(makeLeaf(token, token, LEMMA_CAN)))
      case "n't" => Set(SptRB(makeLeaf(token, token, LEMMA_NOT)))
      case "'" | "'s" => Set(SptPOS(leaf))
      case LEMMA_TO => Set(SptTO(leaf))
      case _ => {
        Set.empty
      }
    }
  }

  override def shouldForceSQ(tree : SprSyntaxTree) : Boolean =
  {
    tree.firstChild.firstChild.isBeingVerb(this)
  }

  override def pronounLemma(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity,
    politeness : SilPoliteness,
    inflection : SilInflection,
    possesseeCount : SilCount = COUNT_SINGULAR
  ) : String =
  {
    // FIXME:  old school thee/thou politeness
    person match {
      case PERSON_FIRST => count match {
        case COUNT_PLURAL => inflection match {
          case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_US
          case INFLECT_GENITIVE => LEMMA_OUR
          case _ => proximity match {
            case PROXIMITY_REFLEXIVE => LEMMA_OURSELVES
            case PROXIMITY_POSSESSEE => LEMMA_OURS
            case PROXIMITY_ELIDED => ""
            case _ => LEMMA_WE
          }
        }
        case _ => proximity match {
          case PROXIMITY_REFLEXIVE => LEMMA_MYSELF
          case PROXIMITY_POSSESSEE => LEMMA_MINE
          case PROXIMITY_ELIDED => ""
          case _ => inflection match {
            case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_ME
            case INFLECT_GENITIVE => LEMMA_MY
            case _ => "I"
          }
        }
      }
      case PERSON_SECOND => inflection match {
        case INFLECT_GENITIVE => LEMMA_YOUR
        case _ => {
          proximity match {
            case PROXIMITY_REFLEXIVE => count match {
              case COUNT_PLURAL => LEMMA_YOURSELVES
              case _ => LEMMA_YOURSELF
            }
            case PROXIMITY_POSSESSEE => LEMMA_YOURS
            case PROXIMITY_ELIDED => ""
            case _ => LEMMA_YOU
          }
        }
      }
      case PERSON_THIRD => count match {
        case COUNT_PLURAL => proximity match {
          case _ : SilHereProximity => LEMMA_THESE
          case _ : SilThereProximity => LEMMA_THOSE
          case PROXIMITY_ENTITY => inflection match {
            case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_THEM
            case INFLECT_GENITIVE => LEMMA_THEIR
            case _ => LEMMA_THEY
          }
          case PROXIMITY_REFLEXIVE => LEMMA_THEMSELVES
          case PROXIMITY_POSSESSEE => LEMMA_THEIRS
          case PROXIMITY_ELIDED => ""
        }
        case _ => gender.maybeBasic match {
          case Some(GENDER_MASCULINE | GENDER_SOMEONE) => proximity match {
            case PROXIMITY_REFLEXIVE => LEMMA_HIMSELF
            case PROXIMITY_ELIDED => ""
            case PROXIMITY_POSSESSEE => LEMMA_HIS
            case _ => inflection match {
              case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_HIM
              case INFLECT_GENITIVE => LEMMA_HIS
              case _ => LEMMA_HE
            }
          }
          case Some(GENDER_FEMININE) => proximity match {
            case PROXIMITY_REFLEXIVE => LEMMA_HERSELF
            case PROXIMITY_ELIDED => ""
            case PROXIMITY_POSSESSEE => LEMMA_HERS
            case _ => inflection match {
              case INFLECT_ACCUSATIVE | INFLECT_GENITIVE |
                  INFLECT_ADPOSITIONED => LEMMA_HER
              case _ => LEMMA_SHE
            }
          }
          case Some(GENDER_NEUTER) => proximity match {
            case _ : SilHereProximity => LEMMA_THIS
            case _ : SilThereProximity => LEMMA_THAT
            case PROXIMITY_POSSESSEE => LEMMA_ITS
            case PROXIMITY_ENTITY => inflection match {
              case INFLECT_GENITIVE => LEMMA_ITS
              case _ => LEMMA_IT
            }
            case PROXIMITY_REFLEXIVE => LEMMA_ITSELF
            case PROXIMITY_ELIDED => ""
          }
          case Some(GENDER_SOMEWHERE) => proximity match {
            case _ : SilHereProximity => LEMMA_HERE
            case PROXIMITY_ELIDED => ""
            case _ => LEMMA_THERE
          }
          case _ => {
            throw new IllegalArgumentException("custom pronoun word required")
          }
        }
      }
    }
  }

  override def proximityLemma(proximity : SilProximity) : String =
  {
    proximity match {
      case PROXIMITY_SPEAKER_HERE => LEMMA_HERE
      case PROXIMITY_AROUND_HERE => LEMMA_HERE
      case PROXIMITY_LISTENER_THERE => LEMMA_THERE
      case PROXIMITY_OVER_THERE => LEMMA_THERE
      case PROXIMITY_WAY_OVER_THERE => LEMMA_THERE
      case _ => ""
    }
  }

  override def proximityForLemma(lemma : String) : Option[SilProximity] =
  {
    lemma match {
      case LEMMA_HERE => Some(PROXIMITY_SPEAKER_HERE)
      case LEMMA_THERE => Some(PROXIMITY_LISTENER_THERE)
      case _ => None
    }
  }

  override def predefLemma(predef : SprPredef) : String =
  {
    predefToLemma(predef)
  }

  def predefForLemma(
    lemma : String,
    label : String = LABEL_AMBIGUOUS
  ) : Option[SprPredef] =
  {
    lemma match {
      case LEMMA_NEITHER => Some(label match {
        case LABEL_DT => PD_NEITHER_DETERMINER
        case _ => PD_NEITHER_NOUN
      })
      case _ => lemmaToPredef.get(lemma)
    }
  }

  override def filterIndexWords(
    token : String,
    tokenSuffix : String,
    rawWords : Set[IndexWord]
  ) : Set[IndexWord] =
  {
    if (rawWords.exists(_.getLemma == LEMMA_BE)) {
      rawWords.filter(
        indexWord => (indexWord.getLemma == LEMMA_BE) &&
          indexWord.getPOS == POS.VERB)
    } else if ((token == LEMMA_THERE) || (token == LEMMA_HERE)) {
      rawWords.filterNot(_.getPOS == POS.NOUN)
    } else if (token == LEMMA_OR) {
      rawWords.filterNot(_.getLemma == LEMMA_OR)
    } else if (tokenSuffix == "boss") {
      // FIXME ugh
      rawWords.filterNot(_.getLemma == "bos")
    } else {
      rawWords.filterNot(raw => (raw.getLemma == tokenSuffix) &&
        rawWords.exists(other =>
          ((other != raw) && (other.getPOS == raw.getPOS) &&
            (other.getLemma != tokenSuffix))))
    }
  }

  // since English doesn't discriminate between PROXIMITY_LISTENER_THERE
  // and PROXIMITY_OVER_THERE, we fold them together
  override def overThere : SilProximity = PROXIMITY_LISTENER_THERE

  override def possibleCompoundNoun(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    !seq.head.hasTerminalLemma(LEMMA_A)
  }

  override def possibleCompoundVerb(seq : Seq[String]) : Boolean =
  {
    // meh
    !(seq.contains(LEMMA_BE) || seq.contains(LEMMA_TO))
  }

  override def pluralizeNoun(lemma : String) : String =
  {
    EnglishPluralizer.plural(lemma)
  }

  def isPotentialPlural(noun : String) : Boolean =
  {
    val bases = wordnet.getMorphology.lookupAllBaseForms(
      POS.NOUN, noun).asScala
    return (bases.size > 1) || (!bases.isEmpty && !bases.contains(noun))
  }

  override def isPluralNoun(
    token : String,
    lemma : String,
    indexWord : IndexWord) : Boolean =
  {
    if (token != lemma) {
      true
    } else {
      val senses = indexWord.getSenses.asScala
      senses.exists(s => {
        val equivalents = s.getWords.asScala.
          filter(w => wordnet.isPlainWord(w.getLemma)).
          filter(_.getLemma != indexWord.getLemma)
        s.getGloss.startsWith("(plural) ") ||
        (equivalents.count(w => isPotentialPlural(w.getLemma)) > 1)
      })
    }
  }

  override protected def getMatcherResource() =
    "/english/phrase-structure.txt"

  private def scoreVerbModifiers = phraseScorer {
    case SilBasicVerbModifier(word) => {
      if (word.toLemma == LEMMA_NO) {
        SilPhraseScore.conBig
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  private def scoreSpecialEnglishAdpositions = phraseScorer {
    case ap : SilAdpositionalPhrase => {
      val words = ap.adposition.word.decomposed
      if ((words.size > 1) && words.exists(_.lemma == LEMMA_THERE)) {
        SilPhraseScore.conBig
      } else if (words.exists(_.lemma == PD_ADVERBIAL_TMP.toLemma)) {
        SilPhraseScore.proBig
      } else if (ap.adposition != SprPredefAdposition(PD_TO)) {
        // in a phrase like "he went up the steps", we boost the
        // interpretation of "up" as an adposition vs adverb
        SilPhraseScore.pro(20)
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  private def scoreEnglishUsage = phraseScorer {
    case SilNounReference(noun) => {
      usageScore(noun.toNounLemma, POS.NOUN)
    }
    case SilPropertyState(sw : SilSimpleWord) => {
      val lemma = sw.toLemma
      if (lemma == LEMMA_THERE) {
        SilPhraseScore.conSmall
      } else {
        usageScore(lemma, POS.ADJECTIVE)
      }
    }
    case SilActionPredicate(_, sw : SilSimpleWord, _, _) => {
      usageScore(sw.toLemma, POS.VERB)
    }
    case SilBasicVerbModifier(sw : SilSimpleWord) => {
      val lemma = sw.toLemma
      if (lemma.toLowerCase == "yesterday") {
        SilPhraseScore.pro(10)
      } else if (lemma == LEMMA_THERE) {
        SilPhraseScore.conSmall
      } else {
        usageScore(lemma, POS.ADVERB)
      }
    }
  }
}
