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

import scala.collection._
import scala.util._

case class SmcResolutionOptions(
  failOnUnknown : Boolean = true,
  resolveUniqueDeterminers : Boolean = false,
  resolveConjunctions : Boolean = false
)

class SmcReferenceRewriter[
  EntityType<:SmcEntity, PropertyType<:SmcProperty
]
  (
  cosmos : SmcCosmos[EntityType, PropertyType],
  sentencePrinter : SilSentencePrinter,
  resultCollector : SmcResultCollector[EntityType],
  options : SmcResolutionOptions = SmcResolutionOptions())
    extends SilPhraseRewriter
{
  private val reverseMap = new mutable.HashMap[SilReference, SilReference]

  private def newResolved(
    entities : Set[EntityType],
    noun : SilWord,
    determiner : SilDeterminer,
    unresolved : SilReference,
    intermediate : Option[SilReference] = None) =
  {
    val rr = SilResolvedReference(entities, noun, determiner)
    reverseMap.put(rr, unresolved)
    resultCollector.referenceMap.put(rr, entities)
    resultCollector.referenceMap.put(unresolved, entities)
    intermediate.foreach(resultCollector.referenceMap.put(_, entities))
    rr
  }

  def rewriteReferences = replacementMatcher {
    case nr @ SilNounReference(
      noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
    ) if (noun.isProper) => {
      cosmos.resolveQualifiedNoun(
        noun.lemma, REF_SUBJECT, Set.empty) match
      {
        case Success(entities) => {
          newResolved(entities, noun, nr.determiner, nr)
        }
        case Failure(e) => {
          if (options.failOnUnknown) {
            throw cosmos.fail(
              sentencePrinter.sb.respondUnknown(noun)).exception
          } else {
            nr
          }
        }
      }
    }
    case nr @ SilNounReference(
      noun, DETERMINER_UNIQUE, COUNT_SINGULAR
    ) if (options.resolveUniqueDeterminers) => {
      cosmos.resolveQualifiedNoun(
        noun.lemma, REF_SUBJECT, Set.empty) match
      {
        case Success(entities) => {
          newResolved(entities, noun, nr.determiner, nr)
        }
        case Failure(e) => {
          nr
        }
      }
    }
    case cr @ SilConjunctiveReference(
      DETERMINER_ALL, references, separator
    ) if (options.resolveConjunctions) => {
      val resolved = references.flatMap(_ match {
        case rr : SilResolvedReference[EntityType] => {
          Some(rr)
        }
        case _ => {
          None
        }
      })
      if (resolved.size != references.size) {
        cr
      } else {
        val entities = resolved.flatMap(_.entities).toSet
        val reconstructed = SilConjunctiveReference(
          DETERMINER_ALL, references.map(reverseMap), separator)
        // FIXME is this correct for noun and determiner??
        newResolved(
          entities, resolved.head.noun, resolved.head.determiner,
          reconstructed, Some(cr))
      }
    }
    case gr @ SilGenitiveReference(
      grr : SilResolvedReference[EntityType], SilNounReference(noun, _, _)
    ) => {
      val roleName = noun.lemma
      grr.entities.foreach(entity => cosmos.reifyRole(entity, roleName, true))
      val attempts = grr.entities.map(
        entity => cosmos.resolveEntityAssoc(entity, roleName))
      if (attempts.exists(_.isFailure)) {
        gr
      } else {
        val entities = attempts.map(_.get).flatMap(_.toSet)
        val reconstructed = SilGenitiveReference(reverseMap(grr), gr.possessee)
        // FIXME is this correct for noun and determiner??
        newResolved(entities, noun, grr.determiner, reconstructed, Some(gr))
      }
    }
  }
}
