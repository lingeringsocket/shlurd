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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

class SmcInputRewriter[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType <:SmcCosmos[EntityType, PropertyType]
](
  mind : SmcMind[EntityType, PropertyType, CosmosType],
  annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]]
) extends SilPhraseRewriter(annotator)
{
  private implicit val tongue = mind.getTongue

  def normalizeInput(
    sentence : SilSentence) : SilSentence =
  {
    rewrite(
      combineRules(convertGenitiveOf, deparenthesize),
      sentence,
      SilRewriteOptions(topDown = true)
    )
  }

  // FIXME this should be context-sensitive, and should be able to
  // handle more complicated constructs such as
  // "the first cousin of Elizabeth"
  private def convertGenitiveOf = replacementMatcher(
    "convertGenitiveOf", {
      case SilOptionallyDeterminedReference(
        SilStateSpecifiedReference(
          SilCountedNounReference(noun, count),
          SilAdpositionalState(
            SilAdposition.OF,
            possessor
          )
        ),
        determiner
      ) if (
        (noun.toNounLemma != MW_KIND.toLemma) &&
          (determiner match {
            case DETERMINER_ABSENT | DETERMINER_DEFINITE |
                (_ : SilUnlimitedDeterminer) => true
            case _ => false
          })
      ) => {
        annotator.genitiveRef(
          possessor,
          annotator.nounRef(noun, count))
      }
    }
  )

  private def deparenthesize = replacementMatcher(
    "deparenthesize", {
      case SilParenthesizedReference(
        reference, _
      ) => {
        reference
      }
    }
  )

  def bindPredicateWildcard(predicate : SilPredicate, objRef : SilReference)
      : SilPredicate =
  {
    rewrite(
      replacePredicateWildcard(objRef),
      predicate,
      SilRewriteOptions(topDown = true))
  }

  private def replacePredicateWildcard(
    objRef : SilReference) = replacementMatcher(
    "replacePredicateWildcard", {
      case ref @ SilDeterminedReference(
        _ : SilNounReference, _
      ) if SmcPhraseQuerier.containsWildcard(ref) => {
        objRef
      }
    }
  )
}
