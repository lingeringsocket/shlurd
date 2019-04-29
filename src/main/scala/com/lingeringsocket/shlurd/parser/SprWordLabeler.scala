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

import SprPennTreebankLabels._
import SprEnglishLemmas._
import ShlurdEnglishAffixes._

import net.sf.extjwnl.data._

import scala.collection._
import scala.collection.JavaConverters._

case class SprContext(
  wordLabeler : SprWordLabeler = new SprWordnetLabeler,
  scorer : SilPhraseScorer = new SilWordnetScorer
)
{
  def newParser(input : String) = SprParser(input, this)
}

trait SprWordLabeler
{
  def labelWord(
    token : String, word : String, iToken : Int) : Set[SprSyntaxTree]

  def isCompoundNoun(seq : Seq[SprSyntaxTree]) : Boolean = false

  def isCompoundAdverb(seq : Seq[SprSyntaxTree]) : Boolean = false

  def isCompoundVerb(seq : Seq[SprSyntaxTree]) : Boolean = false
}

object SprWordnetLabeler
{
  // adapted from
  // http://www.d.umn.edu/~tpederse/Group01/WordNet/wordnet-stoplist.html
  private val stopList = Set(
    "I", "an", "as", "at", "by", "he", "it", "do", "at", "off",
    "his", "me", "or", "thou", "us", "who", "must", "ca", "may", "in",
    "does", "have", "my", "might"
  )

  private val partsOfSpeech = POS.getAllPOS.asScala.toSet

  private val dictionary = ShlurdWordnet.dictionary

  private val morphology = ShlurdWordnet.morphology
}

class SprWordnetLabeler extends SprWordLabeler with SprEnglishWordAnalyzer
{
  import SprWordnetLabeler._

  override def labelWord(
    token : String, word : String, iToken : Int) =
  {
    val (tokenPrefix, tokenSuffix) = {
      val iHyphen = token.lastIndexOf('-')
      if (iHyphen < 0) {
        tupleN(("", token))
      } else {
        tupleN((token.take(iHyphen + 1), token.drop(iHyphen + 1)))
      }
    }
    val indexWords : Set[SprSyntaxTree] = {
      if (token.contains('_')) {
        Set(SptNN(makeLeaf(word, word, word)))
      } else if (stopList.contains(tokenSuffix) ||
        maybeDeterminerFor(token).nonEmpty)
      {
        // FIXME some determiners may have other POS roles, e.g.
        // "no" can be a noun or interjection
        Set.empty
      } else if (((token != word) && (iToken > 0)) ||
        (isProper(token) && (iToken == 0)))
      {
        Set(SptNNP(makeLeaf(word, word, word)))
      } else {
        val pairs = partsOfSpeech.flatMap(pos => {
          morphology.lookupAllBaseForms(pos, tokenSuffix).asScala.map(
            lemma => tupleN((pos, lemma))).toSet
        })
        val rawWords = pairs.flatMap {
          case (pos, lemma) => {
            Option(dictionary.getIndexWord(pos, lemma))
          }
        }
        val filteredWords = {
          if (rawWords.exists(_.getLemma == LEMMA_BE)) {
            rawWords.filter(
              indexWord => (indexWord.getLemma == LEMMA_BE) &&
                indexWord.getPOS == POS.VERB)
          } else if (token == LEMMA_THERE) {
            rawWords.filterNot(_.getPOS == POS.NOUN)
          } else if (token == LEMMA_OR) {
            rawWords.filterNot(_.getLemma == LEMMA_OR)
          } else {
            rawWords.filterNot(raw => (raw.getLemma == tokenSuffix) &&
              rawWords.exists(other =>
                ((other != raw) && (other.getPOS == raw.getPOS) &&
                  (other.getLemma != tokenSuffix))))
          }
        }
        filteredWords.filterNot(ShlurdWordnet.isAcronym).flatMap(
          indexWord => makePreTerminals(
            word, token, tokenPrefix, tokenSuffix,
            indexWord, (iToken == 0), filteredWords))
      }
    }
    val combined = {
      if (isCoordinatingConjunction(token)) {
        Set(SptCC(makeLeaf(word, token)))
      } else {
        maybeDeterminerFor(token).map(
          determiner => (SptDT(makeLeaf(word, token)))).toSet
      }
    } ++ {
      if (isPronounWord(token)) {
        if (isPossessiveAdjective(token)) {
          Set(SptPRP_POS(makeLeaf(word, token)))
        } else {
          Set(SptPRP(makeLeaf(word, token)))
        }
      } else {
        Set.empty
      }
    } ++ {
      def leaf = makeLeaf(word, token)
      token match {
        case (
          LEMMA_MUST | LEMMA_MAY | LEMMA_MIGHT |
            LEMMA_COULD | LEMMA_SHOULD | LEMMA_CAN
        )=> {
          Set(SptMD(leaf))
        }
        case LEMMA_THERE => {
          Set(SptNP(SptEX(leaf)))
        }
        case LEMMA_THAT => {
          Set(SptIN(leaf),
            SptWDT(leaf))
        }
        case LEMMA_WHO | LEMMA_WHOM => Set(SptWP(leaf))
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
        case _ => {
          Set.empty
        }
      }
    } ++ {
      if ((isAdposition(token) ||
        isSubordinatingConjunction(token)) &&
        (token != LEMMA_TO))
      {
        Set(SptIN(makeLeaf(word, token)))
      } else {
        Set.empty
      }
    } ++ {
      indexWords
    }
    if (combined.nonEmpty) {
      combined
    } else {
      def leaf = makeLeaf(word, token)
      val set : Set[SprSyntaxTree] = token match {
        case LABEL_COMMA => Set(SptCOMMA(leaf))
        case LABEL_SEMICOLON => Set(SptSEMICOLON(leaf))
        case "'" | "'s" => Set(SptPOS(leaf))
        // FIXME proper handling for all contractions
        case "ca" => Set(SptMD(makeLeaf(token, token, LEMMA_CAN)))
        case "n't" => Set(SptRB(makeLeaf(token, token, LEMMA_NOT)))
        case LEMMA_TO => Set(SptTO(leaf))
        case _ => {
          if (SprParser.isTerminator(token)) {
            Set(SptDOT(leaf))
          } else {
            val noun = {
              if (iToken == 0) {
                SptNNP(makeLeaf(word, word, word))
              } else {
                SptNN(leaf)
              }
            }
            Set(noun)
          }
        }
      }
      set
    }
  }

  override def isCompoundNoun(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    if (seq.size < 2 || !seq.forall(_.isPreTerminal)) {
      false
    } else {
      def isProperNoun(tree : SprSyntaxTree) = tree match {
        case noun : SprSyntaxNoun => noun.isProper
        case _ => false
      }
      if (!seq.last.isNoun) {
        false
      } else if (seq.forall(isProperNoun)) {
        true
      } else {
        val spaced = (seq.dropRight(1).map(_.firstChild.foldedToken) :+
          seq.last.firstChild.lemma).mkString(" ")
        ShlurdWordnet.isPotentialNoun(spaced)
      }
    }
  }

  override def isCompoundAdverb(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    if (seq.size < 2 || !seq.forall(_.isAdverb)) {
      false
    } else {
      val spaced = seq.map(_.firstChild.foldedToken).mkString(" ")
      ShlurdWordnet.isPotentialAdverb(spaced)
    }
  }

  override def isCompoundVerb(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    if (seq.size < 2 || !seq.forall(_.isPreTerminal)) {
      false
    } else {
      // this handles "stir fry" and "bump off", but there are
      // other cases that need refinement
      val lemmas = seq.map(_.firstChild.lemma)
      // meh
      if (lemmas.contains(LEMMA_BE) || lemmas.contains(LEMMA_TO)) {
        false
      } else {
        val spaced = lemmas.mkString(" ")
        ShlurdWordnet.isPotentialVerb(spaced)
      }
    }
  }

  private def makePreTerminals(
    word : String, token : String,
    tokenPrefix : String, tokenSuffix : String,
    indexWord : IndexWord,
    forceProper : Boolean, alternatives : Set[IndexWord])
      : Set[SprSyntaxTree] =
  {
    val lemma = indexWord.getLemma
    val label = indexWord.getPOS match {
      case POS.ADJECTIVE => LABEL_JJ
      case POS.ADVERB => LABEL_RB
      case POS.NOUN => {
        if ((tokenSuffix != lemma) || ShlurdWordnet.isPlural(indexWord)) {
          LABEL_NNS
        } else {
          if (forceProper) {
            LABEL_NNP
          } else {
            LABEL_NN
          }
        }
      }
      case POS.VERB => {
        if (tokenSuffix != lemma) {
          if (tokenSuffix.endsWith(SUFFIX_ING)) {
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
              (tokenSuffix.take(2) != lemma.take(2)))
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
    }
    val labels = Set(label) ++ {
      if ((label == LABEL_VBD) && (lemma != LEMMA_BE) && (lemma != LEMMA_DO)) {
        Set(LABEL_VBN)
      } else {
        Set.empty
      }
     }
    labels.map(label => {
      // try to match the way CoreNLP lemmatizes gerunds and participles
      val conformedLemma = {
        label match {
          case LABEL_VBN => {
            if (ShlurdWordnet.isPotentialNoun(tokenSuffix)) {
              tokenSuffix
            } else {
              lemma
            }
          }
          case LABEL_JJ => {
            if (ShlurdWordnet.isPotentialNoun(tokenSuffix)) {
              lemma
            } else {
              alternatives.find(v => (v.getPOS == POS.VERB)).
                map(_.getLemma).getOrElse(lemma)
            }
          }
          case _ => lemma
        }
      }
      val leaf = {
        if (label == LABEL_NNP) {
          makeLeaf(word, word, word)
        } else {
          makeLeaf(word, token, tokenPrefix + conformedLemma)
        }
      }
      SprSyntaxRewriter.recompose(label, Seq(leaf))
    })
  }
}
