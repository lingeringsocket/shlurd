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

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.data._

object SnlWordnetGlossPrep
{
  private val wordnet = SnlPrincetonWordnet

  private implicit val tongue = SnlUtils.defaultTongue

  def parseNounGlosses(lemma : String) : Seq[Option[SilReference]] =
  {
    wordnet.getNounSenses(lemma).flatMap(sense => {
      getNounSenseDefinitions(sense).map(parseNounDefinition)
    })
  }

  def getNounSenseDefinitions(sense : Synset)
      : Seq[String] =
  {
    wordnet.getGlossDefinitions(sense).map(
      definition => s"$definition exists")
  }

  def parseSentence(input : String) : SilSentence =
  {
    SprParser(input, SnlUtils.defaultContext).parseOne.sentence
  }

  def parseNounExample(input : String) : SilPhrase =
  {
    val attempt = parseSentence(input)
    if (attempt.hasUnknown) {
      parseSentence(s"$input exists")
    } else {
      attempt
    }
  }

  def parseNounDefinition(
    definition : String) : Option[SilReference] =
  {
    val sentence = parseSentence(definition)
    sentence match {
      case SilPredicateSentence(
        SilStatePredicate(
          subject,
          SprStatePredefVerb(STATE_PREDEF_BE),
          SilExistenceState(_),
          Seq()
        ),
        tam,
        SilFormality.DEFAULT
      ) if (tam == SilTam.indicative) => {
        Some(subject)
      }
      case _ => {
        None
      }
    }
  }
}
