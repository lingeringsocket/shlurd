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

import SnlEnglishLemmas._

import scala.collection._

class SnlEnglishSyntaxAnalyzer(
  context : SprContext,
  guessedQuestion : Boolean,
  strictness : SprStrictness,
  enforceTransitive : Boolean
) extends SnlCorenlpSyntaxAnalyzer(
  context, guessedQuestion, strictness, enforceTransitive)
{
  override protected def isImperative(children : Seq[SprSyntaxTree]) =
  {
    (children.size == 1) && children.head.isVerbPhrase
  }

  override protected def isNegative(tree : SprSyntaxTree) : Boolean =
  {
    // FIXME I just can't even
    if (tree.unwrapPhrase.hasTerminalLemma(LEMMA_NOT)) {
      true
    } else if (tree.isAdverbPhrase && (tree.children.size > 1)) {
      tree.children.exists(c =>
        c.hasTerminalLemma(LEMMA_NOT) || c.hasTerminalLemma(LEMMA_NO))
    } else {
      false
    }
  }

  override protected def getVerbInflection(verb : SprSyntaxTree)
      : SilVerbInflection =
  {
    // this is bogus but works correctly for the few special cases
    // where it matters
    verb match {
      case vbp : SptVBP => {
        if (vbp.child.token == "am") {
          SilVerbInflection(PERSON_FIRST, GENDER_NEUTER, COUNT_SINGULAR)
        } else {
          SilVerbInflection(PERSON_THIRD, GENDER_NEUTER, COUNT_PLURAL)
        }
      }
      case _ => SilVerbInflection(PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR)
    }
  }

  override protected def combineNegatives(
    n1 : Boolean, n2 : Boolean) : Boolean =
  {
    // FIXME:  representation for double negatives?
    n1 ^ n2
  }
}
