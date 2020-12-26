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

import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.data._
import net.sf.extjwnl.data.mcr30.alignment._

class SnlWordnetAlignment(
  resourceDir : String,
  firstTongue : SprTongue,
  secondTongue : SprTongue)
{
  private val firstToSecond = InterLingualIndex.loadMapper(
    getFirstWordnet.getDictionary,
    getSecondWordnet.getDictionary)

  private val secondToFirst = InterLingualIndex.loadMapper(
    getSecondWordnet.getDictionary,
    getFirstWordnet.getDictionary)

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
    val mapper = direction match {
      case TRANSLATE_FIRST_TO_SECOND => firstToSecond
      case TRANSLATE_SECOND_TO_FIRST => secondToFirst
    }
    Option(mapper.mapSynset(synset))
  }
}
