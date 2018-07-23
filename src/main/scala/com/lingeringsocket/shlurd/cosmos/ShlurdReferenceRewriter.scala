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
        case SilResolvedReference(entities, noun, determiner) => {
          Some((entities, noun, determiner))
        }
        case _ => {
          None
        }
      })
      if (resolved.size != references.size) {
        cr
      } else {
        // FIXME do we need to some resultCollector.referenceMap.put?
        SilResolvedReference(
          resolved.flatMap(_._1).toSet, resolved.head._2, resolved.head._3)
      }
    }
    case gr @ SilGenitiveReference(
      rr : SilResolvedReference[E], SilNounReference(noun, _, _)
    ) => {
      val roleName = noun.lemma
      rr.entities.foreach(entity => cosmos.reifyRole(entity, roleName, true))
      val attempts = rr.entities.map(
        entity => cosmos.resolveEntityAssoc(entity, roleName))
      if (attempts.exists(_.isFailure)) {
        gr
      } else {
        // FIXME do we need to some resultCollector.referenceMap.put?
        // and is this correct for noun and determiner??
        SilResolvedReference[E](
          attempts.map(_.get).flatMap(_.toSet), noun, rr.determiner)
      }
    }
  }
}
