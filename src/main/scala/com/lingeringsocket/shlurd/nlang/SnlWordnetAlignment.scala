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
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.dictionary._
import net.sf.extjwnl.data._

import scala.util._
import scala.collection._

class SnlWordnetAlignment(
  resourceDir : String,
  firstTongue : SprTongue,
  secondTongue : SprTongue)
{
  private val firstToSecond = loadMapping

  private val secondToFirst = firstToSecond.map {
    case ((pos, offset1), offset2) => {
      tupleN(tupleN(pos, offset2), offset1)
    }
  }

  private def loadPOS(resourceName : String) : IndexedSeq[Long] =
  {
    Using.resource(
      ResourceUtils.getResourceSource(resourceDir + resourceName)
    ) {
      var pos = 0
      source => source.getLines().flatMap(line => {
        val linePos = pos
        pos += (line.getBytes("UTF-8").length + 1)
        if (line.startsWith("#")) {
          None
        } else {
          val offset = line.split(" ").head.toLong
          assert(offset == linePos, tupleN(offset, linePos, line))
          Some(offset)
        }
      }).toIndexedSeq
    }
  }

  private def loadMapping : Map[(POS, Long), Long] =
  {
    val index = Map(
      POS.NOUN -> loadPOS("data.noun"),
      POS.VERB -> loadPOS("data.verb"),
      POS.ADJECTIVE -> loadPOS("data.adj"),
      POS.ADVERB -> loadPOS("data.adv")
    )
    Using.resource(
      ResourceUtils.getResourceSource(resourceDir + "translation.ssv")
    ) {
      source => source.getLines().flatMap(line => {
        val cols = line.split(" ")
        assert(cols.size == 3, cols)
        val src = cols.head
        val target = cols(1)
        val posChar = src.head
        if (posChar != target.head) {
          // due to a few pos mismatches in mapping_wordnet.json
          None
        } else {
          assert(src(1) == '#', src(1))
          val srcIndex = src.drop(2).toInt
          val targetOffset = target.drop(1).toLong
          val pos = posChar match {
            case 'n' => POS.NOUN
            case 'v' => POS.VERB
            case 'a' => POS.ADJECTIVE
            case 'r' => POS.ADVERB
          }
          Some(tupleN(tupleN(pos, index(pos)(srcIndex)), targetOffset))
        }
      }).toMap
    }
  }

  private def applyMapping(
    synset : Synset, map : Map[(POS, Long), Long],
    target : Dictionary) : Option[Synset] =
  {
    val pos = synset.getPOS
    map.get(tupleN(pos, synset.getOffset)).map(offset =>
      target.getSynsetAt(pos, offset))
  }

  def getFirstTongue = firstTongue

  def getSecondTongue = secondTongue

  def getSourceTongue(direction : SnlTranslationDirection) : SprTongue =
  {
    direction match {
      case TRANSLATE_FIRST_TO_SECOND => getFirstTongue
      case TRANSLATE_SECOND_TO_FIRST => getSecondTongue
    }
  }

  def getTargetTongue(direction : SnlTranslationDirection) : SprTongue =
  {
    direction match {
      case TRANSLATE_FIRST_TO_SECOND => getSecondTongue
      case TRANSLATE_SECOND_TO_FIRST => getFirstTongue
    }
  }

  def getFirstWordnet = firstTongue.getWordnet

  def getSecondWordnet = secondTongue.getWordnet

  def mapSense(
    synset : Synset,
    direction : SnlTranslationDirection) : Option[Synset] =
  {
    val (mapping, dict) = direction match {
      case TRANSLATE_FIRST_TO_SECOND => tupleN(
        firstToSecond, getSecondWordnet.getDictionary
      )
      case TRANSLATE_SECOND_TO_FIRST => tupleN(
        secondToFirst, getFirstWordnet.getDictionary
      )
    }
    applyMapping(
      synset, mapping,
      dict)
  }
}
