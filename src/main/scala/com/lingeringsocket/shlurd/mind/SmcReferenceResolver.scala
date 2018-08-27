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
  resolveConjunctions : Boolean = false,
  resolveGenitives : Boolean = true,
  reifyRoles : Boolean = true
)

class SmcReferenceResolver[
  EntityType<:SilEntity, PropertyType<:SmcProperty
]
  (
  cosmos : SmcCosmos[EntityType, PropertyType],
  sentencePrinter : SilSentencePrinter,
  resultCollector : SmcResultCollector[EntityType],
  options : SmcResolutionOptions = SmcResolutionOptions())
    extends SilPhraseRewriter
{
  def resolve(phrase : SilPhrase) {
    query(resolveReference, phrase)
  }

  private def storeResolved(
    ref : SilReference,
    entities : Set[EntityType])
  {
    resultCollector.referenceMap.put(ref, entities)
  }

  private def resolveReference = queryMatcher {
    case nr @ SilNounReference(
      noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
    ) if (noun.isProper) => {
      cosmos.resolveQualifiedNoun(
        noun.lemma, REF_SUBJECT, Set.empty) match
      {
        case Success(entities) => {
          storeResolved(nr, entities)
        }
        case Failure(e) => {
          if (options.failOnUnknown) {
            throw cosmos.fail(
              sentencePrinter.sb.respondUnknown(noun)).exception
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
          storeResolved(nr, entities)
        }
        case Failure(e) =>
      }
    }
    case cr @ SilConjunctiveReference(
      DETERMINER_ALL, references, separator
    ) if (options.resolveConjunctions) => {
      val resolved = references.flatMap(r =>
        resultCollector.referenceMap.get(r)
      )
      if (resolved.size == references.size) {
        val entities = resolved.flatten.toSet
        storeResolved(cr, entities)
      }
    }
    case gr @ SilGenitiveReference(
      grr,
      SilNounReference(noun, _, _)
    ) if (options.resolveGenitives) => {
      resultCollector.referenceMap.get(grr).foreach(entities => {
        val roleName = noun.lemma
        if (options.reifyRoles) {
          entities.foreach(entity => cosmos.reifyRole(entity, roleName, true))
        }
        val attempts = entities.map(
          entity => cosmos.resolveEntityAssoc(entity, roleName))
        if (!attempts.exists(_.isFailure)) {
          val entities = attempts.map(_.get).flatMap(_.toSet)
          storeResolved(gr, entities)
        }
      })
    }
  }
}
