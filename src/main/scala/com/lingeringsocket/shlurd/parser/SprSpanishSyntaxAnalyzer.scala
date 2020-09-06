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

import com.lingeringsocket.shlurd.ilang._

class SprSpanishSyntaxAnalyzer(
  context : SprContext,
  strictness : SprStrictness,
  enforceTransitive : Boolean
) extends SprSvoSyntaxAnalyzer(
  context, false, strictness, enforceTransitive)
{
  override protected def allowElidedSubject() : Boolean = true

  override protected def isImperative(children : Seq[SprSyntaxTree]) =
  {
    // FIXME negatives etc
    val seq = {
      if (children.head.isVerbPhrase) {
        children.head.children
      } else {
        children
      }
    }
    seq.head match {
      case verb : SprSyntaxSimpleVerb => {
        val leaf = verb.child
        val (person, count, gender, tam) =
          context.getTongue.analyzeVerbConjugation(
            SilWord(leaf.token, leaf.lemma))
        tam.isImperative
      }
      case _ => {
        false
      }
    }
  }

  override protected def applyInterrogative(tam : SilTam) : SilTam =
  {
    // FIXME not correct for some sentence patterns
    tam
  }
}
