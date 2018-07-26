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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import scala.collection._
import scala.util._

case class ShlurdResolutionOptions(
  failOnUnknown : Boolean = true,
  resolveUniqueDeterminers : Boolean = false,
  resolveConjunctions : Boolean = false
)

class ShlurdReferenceRewriter[E<:ShlurdEntity, P<:ShlurdProperty](
  cosmos : ShlurdCosmos[E, P],
  sentencePrinter : SilSentencePrinter,
  resultCollector : ResultCollector[E],
  options : ShlurdResolutionOptions = ShlurdResolutionOptions())
    extends SilPhraseRewriter
{
  def rewriteReferences = replacementMatcher {
    case nr @ SilNounReference(
      noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
    ) if (noun.isProper) => {
      cosmos.resolveQualifiedNoun(
        noun.lemma, REF_SUBJECT, Set.empty) match
      {
        case Success(entities) => {
          val rr = SilResolvedReference(entities, noun, nr.determiner)
          resultCollector.referenceMap.put(rr, entities)
          resultCollector.referenceMap.put(nr, entities)
          rr
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
          val rr = SilResolvedReference(entities, noun, nr.determiner)
          resultCollector.referenceMap.put(rr, entities)
          resultCollector.referenceMap.put(nr, entities)
          rr
        }
        case Failure(e) => {
          nr
        }
      }
    }
    case cr @ SilConjunctiveReference(
      DETERMINER_ALL, references, _
    ) if (options.resolveConjunctions) => {
      val resolved = references.flatMap(_ match {
        case rr : SilResolvedReference[E] => {
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
        // FIXME is this correct for noun and determiner??
        val rr = SilResolvedReference(
          entities, resolved.head.noun, resolved.head.determiner)
        resultCollector.referenceMap.put(rr, entities)
        resultCollector.referenceMap.put(cr, entities)
        rr
      }
    }
    case gr @ SilGenitiveReference(
      grr : SilResolvedReference[E], SilNounReference(noun, _, _)
    ) => {
      val roleName = noun.lemma
      grr.entities.foreach(entity => cosmos.reifyRole(entity, roleName, true))
      val attempts = grr.entities.map(
        entity => cosmos.resolveEntityAssoc(entity, roleName))
      if (attempts.exists(_.isFailure)) {
        gr
      } else {
        // FIXME is this correct for noun and determiner??
        val entities = attempts.map(_.get).flatMap(_.toSet)
        val rr = SilResolvedReference(
          entities, noun, grr.determiner)
        resultCollector.referenceMap.put(rr, entities)
        resultCollector.referenceMap.put(gr, entities)
        rr
      }
    }
  }
}
