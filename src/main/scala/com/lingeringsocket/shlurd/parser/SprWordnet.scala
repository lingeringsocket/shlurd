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

import net.sf.extjwnl.data._
import net.sf.extjwnl.dictionary._

import scala.collection._
import scala.jdk.CollectionConverters._

import java.util.regex._

trait SprWordnet
{
  def getDictionary : Dictionary

  def getMorphology : MorphologicalProcessor

  private val plainPattern = Pattern.compile("[\\p{IsAlphabetic}&&[^\\p{Lu}]]+")

  def getWordSenses(pos : POS, lemma : String) : Seq[Synset] =
  {
    Option(getDictionary.getIndexWord(pos, lemma)) match {
      case Some(indexWord) => {
        indexWord.getSenses.asScala
      }
      case _ => Seq.empty
    }
  }

  def getVerbSenses(lemma : String) : Seq[Synset] =
  {
    getWordSenses(POS.VERB, lemma)
  }

  def getNounSenses(lemma : String) : Seq[Synset] =
  {
    getWordSenses(POS.NOUN, lemma)
  }

  def allNounSenses =
  {
    getDictionary.getSynsetIterator(POS.NOUN).asScala
  }

  def getVerbFrames(lemma : String) : Seq[String] =
  {
    getVerbSenses(lemma).flatMap(_.getVerbFrames).distinct
  }

  def getVerbFrameFlags(lemma : String) : BitSet =
  {
    getVerbSenses(lemma).map(
      sense => BitSet(sense.getVerbFrameIndices.toIndexedSeq:_*)
    ).reduceLeftOption(_ union _).getOrElse(BitSet.empty)
  }

  def getUsageScore(lemma : String, pos : POS) : Int =
  {
    Option(getDictionary.getIndexWord(pos, lemma)) match {
      case Some(indexWord) => {
        val senseIter = indexWord.getSenses.iterator
        if (senseIter.hasNext) {
          val wordIter = senseIter.next.getWords.iterator
          if (wordIter.hasNext) {
            wordIter.next.getUseCount / 5
          } else {
            -1
          }
        } else {
          -1
        }
      }
      case _ => -1
    }
  }

  def isPotentialAdjective(inflected : String) : Boolean =
  {
    Option(getDictionary.getIndexWord(POS.ADJECTIVE, inflected)) match {
      case Some(indexWord) => true
      case _ => false
    }
  }

  def isPotentialAdverb(inflected : String) : Boolean =
  {
    Option(getDictionary.getIndexWord(POS.ADVERB, inflected)) match {
      case Some(indexWord) => true
      case _ => false
    }
  }

  def isPotentialNoun(inflected : String) : Boolean =
  {
    Option(getDictionary.getIndexWord(POS.NOUN, inflected)) match {
      case Some(indexWord) => true
      case _ => false
    }
  }

  def isPotentialVerb(inflected : String) : Boolean =
  {
    Option(getDictionary.getIndexWord(POS.VERB, inflected)) match {
      case Some(indexWord) => true
      case _ => false
    }
  }

  def isPlainWord(word : String) : Boolean =
  {
    plainPattern.matcher(word).matches
  }

  def isAcronym(indexWord : IndexWord) : Boolean =
  {
    indexWord.getSenses.asScala.forall(sense => {
      sense.getWords.asScala.exists(word => {
        (word.getLemma.forall(_.isUpper)) &&
          (word.getLemma.toLowerCase == indexWord.getLemma)
      })
    })
  }

  def getSenseId(synset : Synset) : String =
  {
    s"${synset.getPOS.getKey}:${synset.getOffset}"
  }

  def getSenseId(synsets : Seq[Synset]) : String =
  {
    synsets.map(getSenseId).mkString("|")
  }

  def findSense(senseId : String) : Synset =
  {
    val components = senseId.split(':')
    val pos = POS.getPOSForKey(components.head)
    getDictionary.getSynsetAt(pos, components.last.toLong)
  }

  def findSenses(senseId : String) : Seq[Synset] =
  {
    if (senseId.isEmpty) {
      Seq.empty
    } else {
      senseId.split('|').map(findSense)
    }
  }

  def getGlossDefinitions(synset : Synset) : Seq[String] =
  {
    val quote = DQUOTE
    val gloss = synset.getGloss
    gloss.split(';').map(_.trim).filterNot(_.startsWith(quote))
  }

  def getGlossExamples(synset : Synset) : Seq[String] =
  {
    val quote = DQUOTE
    val gloss = synset.getGloss
    gloss.split(';').map(_.trim).filter(_.startsWith(quote)).map(
      _.stripPrefix(quote).stripSuffix(quote))
  }

  def getAdjectiveLemma(
    token : String,
    lemma : String,
    alternatives : Set[IndexWord]
  ) : String =
  {
    lemma
  }
}
