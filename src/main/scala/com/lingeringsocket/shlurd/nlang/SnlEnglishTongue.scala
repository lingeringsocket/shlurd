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

import org.atteo.evo.inflector.{English => EnglishPluralizer}

import SprPennTreebankLabels._
import ShlurdEnglishAffixes._

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
  val LEMMA_BELOW = "below"
  val LEMMA_UNDER = "under"
  val LEMMA_BENEATH = "beneath"
  val LEMMA_UNDERNEATH = "underneath"
  val LEMMA_BEHIND = "behind"
  val LEMMA_OF = "of"
  val LEMMA_GENITIVE_OF = "_of_"
}
import SnlEnglishLemmas._

object SprLexicon
{
  def readLexicon(resource : String) : Set[String] =
  {
    val words = ResourceUtils.getResourceSource(resource).getLines
    Set(words.toSeq:_*)
  }

  val stopListPunct = Set(
    LABEL_LPAREN, LABEL_RPAREN, LABEL_LCURLY, LABEL_RCURLY
  )
}

object SnlEnglishLexicon
{
  import SprLexicon._

  val prepositions = readLexicon("/english/prepositions.txt")

  val subordinates = readLexicon("/english/subordinates.txt")

  val proper = readLexicon("/english/proper.txt")

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

  val keywordToLemma : Map[SprMagicWord, String] = Map(
    MW_ABOVE -> LEMMA_ABOVE,
    MW_ADVERBIAL_TMP -> LEMMA_ADVERBIAL_TMP,
    MW_AFTER -> LEMMA_AFTER,
    MW_ALSO -> "also",
    MW_AMONG -> LEMMA_AMONG,
    MW_AND -> LEMMA_AND,
    MW_ANOTHER -> "another",
    MW_AS -> LEMMA_AS,
    MW_AT -> LEMMA_AT,
    MW_BACK -> LEMMA_BACK,
    MW_BE -> LEMMA_BE,
    MW_BEFORE -> LEMMA_BEFORE,
    MW_BEHIND -> LEMMA_BEHIND,
    MW_BELIEVE -> "believe",
    MW_BELOW -> LEMMA_BELOW,
    MW_BENEATH -> LEMMA_BENEATH,
    MW_BOTH -> LEMMA_BOTH,
    MW_CONSEQUENTLY -> "consequently",
    MW_EITHER -> LEMMA_EITHER,
    MW_EQUIVALENTLY -> LEMMA_EQUIVALENTLY,
    MW_EXCEPT -> LEMMA_EXCEPT,
    MW_EXIST -> LEMMA_EXIST,
    MW_FEMININE -> "feminine",
    MW_FORMER -> LEMMA_FORMER,
    MW_FROM -> LEMMA_FROM,
    MW_FRONT -> LEMMA_FRONT,
    MW_GENERALLY -> "generally",
    MW_GENITIVE_OF -> LEMMA_GENITIVE_OF,
    // FIXME: handle compounds properly
    MW_HOW_MANY -> "how many",
    MW_IF -> LEMMA_IF,
    MW_IN -> LEMMA_IN,
    MW_INSIDE -> LEMMA_INSIDE,
    MW_KIND -> "kind",
    MW_LATTER -> LEMMA_LATTER,
    MW_LEFT -> LEMMA_LEFT,
    MW_MASCULINE -> "masculine",
    MW_NEAR -> LEMMA_NEAR,
    MW_NEARBY -> LEMMA_NEARBY,
    MW_NEITHER -> LEMMA_NEITHER,
    MW_NONE -> LEMMA_NONE,
    MW_NOR -> LEMMA_NOR,
    MW_NOTHING -> LEMMA_NOTHING,
    MW_NOWHERE -> LEMMA_NOWHERE,
    MW_NEUTER -> "neuter",
    MW_OF -> LEMMA_OF,
    MW_ON -> LEMMA_ON,
    MW_ONE -> LEMMA_ONE,
    MW_OR -> LEMMA_OR,
    MW_OTHER -> LEMMA_OTHER,
    MW_OTHERWISE -> "otherwise",
    MW_OUTSIDE -> LEMMA_OUTSIDE,
    MW_OVER -> LEMMA_OVER,
    MW_RIGHT -> LEMMA_RIGHT,
    MW_SAME -> "same",
    MW_SUBSEQUENTLY -> "subsequently",
    MW_THAT -> LEMMA_THAT,
    MW_THEN -> LEMMA_THEN,
    MW_TO -> LEMMA_TO,
    MW_UNDER -> LEMMA_UNDER,
    MW_WHAT -> LEMMA_WHAT,
    MW_WHEN -> LEMMA_WHEN,
    MW_WHENEVER -> LEMMA_WHENEVER,
    MW_WHERE -> LEMMA_WHERE,
    MW_WHICH -> LEMMA_WHICH,
    MW_WHO -> LEMMA_WHO,
    MW_WHOM -> LEMMA_WHOM,
    MW_WHOSE -> LEMMA_WHOSE,
    MW_WITH -> LEMMA_WITH,
    MW_WITHIN -> LEMMA_WITHIN,
    MW_UNDERNEATH -> LEMMA_UNDERNEATH
  )

  val lemmaToKeyword = keywordToLemma.map(_.swap)
  assert(keywordToLemma.size == lemmaToKeyword.size)
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
    lemma == LEMMA_DO
  }

  override def tamForAuxLemma(lemma : String) : SilTam =
  {
    val tam = SilTam.indicative
    lemma match {
      case LEMMA_MUST => tam.withModality(MODAL_MUST)
      case LEMMA_MAY => tam.withModality(MODAL_MAY)
      case LEMMA_COULD | LEMMA_CAN => tam.withModality(MODAL_CAPABLE)
      case LEMMA_MIGHT => tam.withModality(MODAL_POSSIBLE)
      case LEMMA_SHOULD => tam.withModality(MODAL_SHOULD)
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
    tupleN((person, count, gender, inflection, proximityOpt, COUNT_SINGULAR))
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
          LEMMA_COULD | LEMMA_SHOULD | LEMMA_CAN
      )=> {
        Set(SptMD(leaf))
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
      case LEMMA_DO => {
        Set(SptVBP(leaf))
      }
      case "does" => {
        Set(SptVBZ(makeLeaf(word, token, LEMMA_DO)))
      }
      case LEMMA_HAVE => {
        Set(SptVBP(leaf))
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
    inflection : SilInflection,
    possesseeCount : SilCount = COUNT_SINGULAR
  ) : String =
  {
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

  override def keywordLemma(keyword : SprMagicWord) : String =
  {
    keywordToLemma(keyword)
  }

  def keywordForLemma(lemma : String) : Option[SprMagicWord] =
  {
    lemmaToKeyword.get(lemma)
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
      } else if (words.exists(_.lemma == MW_ADVERBIAL_TMP.toLemma)) {
        SilPhraseScore.proBig
      } else if (ap.adposition != SprMagicAdposition(MW_TO)) {
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
