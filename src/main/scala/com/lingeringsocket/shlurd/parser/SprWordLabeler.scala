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

import net.sf.extjwnl.data._

import scala.collection._
import scala.collection.JavaConverters._

object SprContext
{
  def apply(tongue : SprTongue) = {
    new SprContext(
      new SprWordnetLabeler(tongue),
      new SprWordnetScorer(tongue))
  }
}

case class SprContext(
  wordLabeler : SprWordLabeler,
  scorer : SilPhraseScorer,
  annotator : SilAnnotator = SilBasicAnnotator(),
  genderAnalyzer : SilGenderAnalyzer = SilGenderPreserver
)
{
  def newParser(input : String) = SprParser(input, this)

  def getTongue = wordLabeler.getTongue
}

case class SprWordRule(
  phrase : Seq[String],
  labels : Seq[String],
  isClosed : Boolean
)
{
}

trait SprWordLabeler
{
  def getTongue : SprTongue

  def getWordnet = getTongue.getWordnet

  def labelWords(
    // (token, word, iToken)
    entries : Seq[(String, String, Int)],
    foldEphemeralLabels : Boolean = true
  ) : Seq[Set[SprSyntaxTree]]

  def isCompoundNoun(seq : Seq[SprSyntaxTree]) : Boolean = false

  def isCompoundAdverb(seq : Seq[SprSyntaxTree]) : Boolean = false

  def isCompoundVerb(seq : Seq[SprSyntaxTree]) : Boolean = false
}

object SprWordnetLabeler
{
  private val partsOfSpeech = POS.getAllPOS.asScala.toSet

  private val quote = DQUOTE
}

class SprWordnetLabeler(
  val tongue : SprTongue,
  var maxPrefix : Int = 0,
  val rules : mutable.HashMap[Seq[String], SprWordRule] =
    new mutable.HashMap[Seq[String], SprWordRule]
) extends SprWordLabeler with SprSynthesizer
{
  import SprWordnetLabeler._

  private val wordnet = getWordnet

  override def getTongue = tongue

  def addRule(rule : SprWordRule)
  {
    rules.put(rule.phrase.map(_.toLowerCase), rule)
    if (rule.phrase.size > maxPrefix) {
      maxPrefix = rule.phrase.size
    }
  }

  override def labelWords(
    entries : Seq[(String, String, Int)],
    foldEphemeralLabels : Boolean = true) : Seq[Set[SprSyntaxTree]] =
  {
    val indexedEntries = entries.toIndexedSeq
    val results = indexedEntries.map(
      entry => new mutable.LinkedHashSet[SprSyntaxTree])
    var index = 0
    while (index < entries.size) {
      index = labelWordsAt(indexedEntries, results, index, foldEphemeralLabels)
    }
    results
  }

  private def labelWordsAt(
    entries : IndexedSeq[(String, String, Int)],
    results : IndexedSeq[mutable.Set[SprSyntaxTree]],
    iStart : Int,
    foldEphemeralLabels : Boolean) : Int =
  {
    // ought to reimplement this using a trie
    val limit = Math.min(maxPrefix, entries.size - iStart)
    range(1 to limit).foreach(length => {
      val slice = entries.slice(iStart, iStart + length)
      rules.get(slice.map(_._1.toLowerCase)).foreach(rule => {
        val labels = rule.labels
        range(iStart until (iStart + length)).foreach(iComponent => {
          val (token, word, iToken) = entries(iComponent)
          results(iComponent) ++= labelWordFromRule(
            token, word, labels, foldEphemeralLabels)
        })
        if (rule.isClosed) {
          return (iStart + length)
        }
      })
    })
    val (token, word, iToken) = entries(iStart)
    val set = {
      if (word.startsWith(quote) && word.endsWith(quote)
        && (word.size > 1))
      {
        val tree : SprSyntaxTree = SptNNQ(makeLeaf(
          word.stripPrefix(quote).stripSuffix(quote)))
        Set(tree)
      } else {
        labelWordFromDict(token, word, iToken, foldEphemeralLabels)
      }
    }
    results(iStart) ++= set
    iStart + 1
  }

  private def foldEphemeral(
    label : String, foldEphemeralLabels : Boolean) : String =
  {
    if (foldEphemeralLabels &&
      ((label == LABEL_PRP_OBJ) || (label == LABEL_PRP_REFLEXIVE))
    ) {
      LABEL_PRP
    } else {
      label
    }
  }

  private def labelWordFromRule(
    token : String,
    word : String,
    labels : Seq[String],
    foldEphemeralLabels : Boolean) : Set[SprSyntaxTree] =
  {
    SprUtils.orderedSet(labels.map(label => {
      if (label == LABEL_NNP) {
        val leaf = makeLeaf(word, word, word)
        SptNNP(leaf)
      } else {
        val lemma = {
          if (label == LABEL_NNS) {
            getSingular(token)
          } else {
            token
          }
        }
        val leaf = makeLeaf(word, token, lemma)
        val foldedLabel = foldEphemeral(label, foldEphemeralLabels)
        SprSyntaxRewriter.recompose(foldedLabel, Seq(leaf))
      }
    }))
  }

  private def getSingular(token : String) : String =
  {
    // FIXME cache the mapping, and deal with compound+proper nouns
    rules.values.
      filter(r => (r.phrase.size == 1) && r.labels.contains(LABEL_NN)).
      filter(r => tongue.pluralizeNoun(r.phrase.last) == token).
      map(_.phrase.last).headOption.getOrElse(token)
  }

  private def labelWordFromDict(
    token : String, word : String, iToken : Int,
    foldEphemeralLabels : Boolean) : Set[SprSyntaxTree] =
  {
    val stopList = tongue.getStopList
    val (tokenPrefix, tokenSuffix) = {
      val iHyphen = token.lastIndexOf('-')
      if ((iHyphen < 1) || (iHyphen == (token.size - 1))) {
        tupleN(("", token))
      } else {
        tupleN((token.take(iHyphen + 1), token.drop(iHyphen + 1)))
      }
    }
    val indexWords : Set[SprSyntaxTree] = {
      if (token.contains('_')) {
        Set(SptNN(makeLeaf(word, word, word)))
      } else if (stopList.contains(tokenSuffix) ||
        tongue.maybeDeterminerFor(token).nonEmpty)
      {
        // FIXME some determiners may have other POS roles, e.g.
        // in English, "no" can be a noun or interjection
        Set.empty
      } else if (((token != word) && (iToken > 0)) ||
        (tongue.isProper(token) && (iToken == 0)))
      {
        Set(SptNNP(makeLeaf(word, word, word)))
      } else {
        val pairs = partsOfSpeech.flatMap(pos => {
          wordnet.getMorphology.lookupAllBaseForms(pos, tokenSuffix).
            asScala.map(
              lemma => tupleN((pos, lemma))).toSet
        })
        val rawWords = pairs.flatMap {
          case (pos, lemma) => {
            Option(wordnet.getDictionary.getIndexWord(pos, lemma))
          }
        } ++ {
          wordnet.getDictionary.lookupAllIndexWords(tokenSuffix).
            getIndexWordArray.toSet
        }
        val filteredWords = tongue.filterIndexWords(
          token, tokenSuffix, rawWords)
        filteredWords.filterNot(wordnet.isAcronym).flatMap(
          indexWord => makePreTerminals(
            word, token, tokenPrefix, tokenSuffix,
            indexWord, (iToken == 0), filteredWords))
      }
    }
    def leaf = makeLeaf(word, token)
    val combined = {
      if (tongue.isCoordinatingConjunction(token)) {
        Set(SptCC(leaf))
      } else {
        tongue.maybeDeterminerFor(token).filter(_ != DETERMINER_VARIABLE).map(
          determiner => (SptDT(leaf))).toSet
      }
    } ++ {
      tongue.labelPronoun(word, token, foldEphemeralLabels)
    } ++ {
      tongue.labelSpecial(word, token)
    } ++ {
      if ((tongue.isAdposition(token) ||
        tongue.isSubordinatingConjunction(token)) &&
        !tongue.isSpecialAdposition(token))
      {
        Set(SptIN(leaf))
      } else {
        Set.empty
      }
    } ++ {
      indexWords
    }
    if (combined.nonEmpty) {
      combined
    } else {
      val set : Set[SprSyntaxTree] = token match {
        case LABEL_COMMA => Set(SptCOMMA(leaf))
        case LABEL_SEMICOLON => Set(SptSEMICOLON(leaf))
        case LABEL_LPAREN => Set(SptLRB(leaf))
        case LABEL_RPAREN => Set(SptRRB(leaf))
        case LABEL_LCURLY => Set(SptLCB(leaf))
        case LABEL_RCURLY => Set(SptRCB(leaf))
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
      if (!tongue.possibleCompoundNoun(seq)) {
        false
      } else if (!seq.last.isNoun) {
        false
      } else if (seq.forall(isProperNoun)) {
        true
      } else {
        val folded = (seq.dropRight(1).map(_.firstChild.foldedToken) :+
          seq.last.firstChild.lemma)
        rules.get(folded) match {
          case Some(rule) if (
            rule.labels.exists(_.startsWith(LABEL_NN))
          ) => true
          case _ => {
            val spaced = folded.mkString(" ")
            wordnet.isPotentialNoun(spaced)
          }
        }
      }
    }
  }

  override def isCompoundAdverb(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    if (seq.size < 2 || !seq.forall(_.isAdverb)) {
      false
    } else {
      val folded = seq.map(_.firstChild.foldedToken)
      rules.get(folded) match {
        case Some(rule) if (
          rule.labels.exists(_.startsWith(LABEL_RB))
        ) => true
        case _ => {
          val spaced = folded.mkString(" ")
          wordnet.isPotentialAdverb(spaced)
        }
      }
    }
  }

  override def isCompoundVerb(seq : Seq[SprSyntaxTree]) : Boolean =
  {
    if (seq.size < 2 || !seq.forall(_.isPreTerminal)) {
      false
    } else {
      // this handles "stir fry" and "bump off", but there are
      // other cases that need refinement
      val folded = seq.map(_.firstChild.lemma)
      if (!tongue.possibleCompoundVerb(folded)) {
        false
      } else {
        rules.get(folded) match {
          case Some(rule) if (
            rule.labels.exists(_.startsWith(LABEL_VB))
          ) => true
          case _ => {
            val spaced = folded.mkString(" ")
            wordnet.isPotentialVerb(spaced)
          }
        }
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
    val labels = indexWord.getPOS match {
      case POS.ADJECTIVE => Set(LABEL_JJ)
      case POS.ADVERB => Set(LABEL_RB)
      case POS.NOUN => {
        if ((tokenSuffix != lemma) || wordnet.isPlural(indexWord)) {
          Set(LABEL_NNS)
        } else {
          if (forceProper) {
            Set(LABEL_NNP)
          } else {
            Set(LABEL_NN)
          }
        }
      }
      case POS.VERB => {
        tongue.labelVerb(tokenSuffix, lemma)
      }
    }
    labels.map(label => {
      // try to match the way CoreNLP lemmatizes gerunds and participles
      val conformedLemma = {
        label match {
          case LABEL_VBN => {
            if (wordnet.isPotentialNoun(tokenSuffix)) {
              tokenSuffix
            } else {
              lemma
            }
          }
          case LABEL_JJ => {
            if (wordnet.isPotentialNoun(tokenSuffix)) {
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
