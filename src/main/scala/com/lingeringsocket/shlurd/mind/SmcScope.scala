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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

import SprEnglishLemmas._

case class SmcScopeOutput[
  EntityType<:SmcEntity
](
  prior : Option[SilReference],
  entities : Set[EntityType]
)

trait SmcScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
]{
  def getMind : MindType

  def getSentencePrinter : SilSentencePrinter

  def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[SmcScopeOutput[EntityType]]

  def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]]

  protected def findMatchingPronounReference(
    refMap : SmcRefMap[EntityType],
    reference : SilPronounReference) : Seq[SmcScopeOutput[EntityType]] =
  {
    var skip = false
    refMap.filter {
      case (prior, set) => {
        if (skip) {
          false
        } else {
          // maybe this should be eq instead of ==
          if (prior == reference) {
            skip = true
            false
          } else {
            getMind.thirdPersonReference(set) == Some(reference)
          }
        }
      }
    }.toSeq.map {
      case (prior, set) => SmcScopeOutput(Some(prior), set)
    }
  }

  protected def resolveOutput(
    reference : SilReference,
    outputs : Seq[SmcScopeOutput[EntityType]])
      : Try[SmcScopeOutput[EntityType]] =
  {
    val sentencePrinter = getSentencePrinter
    outputs match {
      case Seq() => {
        getMind.getCosmos.fail(
          ShlurdExceptionCode.UnresolvedPronoun,
          sentencePrinter.sb.respondUnresolvedPronoun(
            sentencePrinter.print(
              reference, INFLECT_NOMINATIVE, SilConjoining.NONE)))
      }
      case Seq(output) => {
        Success(output)
      }
      case _ => {
        if (outputs.map(_.entities).distinct.size == 1) {
          // no ambiguity since they are all coreferences; pick one
          // arbitrarily
          Success(outputs.head)
        } else {
          // FIXME report ambiguous possibilities in error
          getMind.getCosmos.fail(
            ShlurdExceptionCode.AmbiguousPronoun,
            sentencePrinter.sb.respondAmbiguousPronoun(
              sentencePrinter.print(
                reference, INFLECT_NOMINATIVE, SilConjoining.NONE)))
        }
      }
    }
  }
}

class SmcMindScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  mind : MindType,
  sentencePrinter : SilSentencePrinter
) extends SmcScope[EntityType, PropertyType, CosmosType, MindType]
{
  override def getMind = mind

  override def getSentencePrinter = sentencePrinter

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) =
  {
    mind.resolveQualifiedNoun(noun, context, qualifiers).map(entities => {
      SmcScopeOutput(None, entities)
    })
  }

  override def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    reference : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    val entityOpt = {
      if (reference.count == COUNT_SINGULAR) {
        reference.person match {
          case PERSON_FIRST => communicationContext.speakerEntity
          case PERSON_SECOND => communicationContext.listenerEntity
          case _ => None
        }
      } else {
        None
      }
    }
    val outputs = entityOpt match {
      case Some(entity) => {
        Seq(SmcScopeOutput(None, Set(entity)))
      }
      case _ => {
        if (reference.distance != DISTANCE_UNSPECIFIED) {
          // FIXME proper resolution for this/that
          Seq(SmcScopeOutput(None, Set.empty[EntityType]))
        } else {
          // FIXME heavy-duty coreference resolution, including current
          // sentence; also, there should probably be some limit on how
          // far back to search.
          if (mind.isConversing) {
            mind.getConversation.getUtterances.reverseIterator.drop(1).map(
              utterance => {
                findMatchingPronounReference(
                  utterance.refMap, reference
                )
              }
            ).find(_.nonEmpty).getOrElse(Seq.empty)
          } else {
            Seq.empty
          }
        }
      }
    }
    resolveOutput(reference, outputs)
  }
}

class SmcPhraseScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  refMap : SmcRefMap[EntityType],
  parent : SmcScope[EntityType, PropertyType, CosmosType, MindType]
) extends SmcScope[EntityType, PropertyType, CosmosType, MindType]
{
  override def getMind = parent.getMind

  override def getSentencePrinter = parent.getSentencePrinter

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[SmcScopeOutput[EntityType]] =
  {
    val outputs = {
      if (!noun.isProper) {
        val nounLemma = noun.toNounLemma
        var nextOrdinal = 0
        def produceOrdinal(ordinal : Int) : Int =
        {
          nounLemma match {
            case LEMMA_FORMER | LEMMA_LATTER => {
              nextOrdinal += 1
              nextOrdinal
            }
            case _ => ordinal
          }
        }
        val ordered = refMap.toSeq.flatMap {
          case (prior, set) => {
            val (primary, aliasOpt) = prior match {
              case SilAppositionalReference(
                primary,
                SilDeterminedReference(
                  SilMandatorySingular(SilNounReference(alias, _)),
                  DETERMINER_UNIQUE
                )
              ) => {
                tupleN((primary, Some(alias.toNounLemma)))
              }
              case _ => tupleN((prior, None))
            }
            def matchLemma(lemma : String) : Boolean =
            {
              nounLemma match {
                case LEMMA_FORMER | LEMMA_LATTER => true
                case _ => {
                  aliasOpt.map(_ == nounLemma).getOrElse(
                    (lemma == nounLemma)
                  )
                }
              }
            }
            primary match {
              case SilDeterminedReference(
                SilNounReference(SilWordLemma(lemma), _),
                DETERMINER_NONSPECIFIC
              ) if (matchLemma(lemma)) => {
                Some(tupleN((prior, set, produceOrdinal(1))))
              }
              case SilStateSpecifiedReference(
                SilMandatorySingular(
                  SilNounReference(
                    SilWordLemma(lemma),
                    _
                  )
                ),
                SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
              ) if (matchLemma(lemma)) => {
                Some(tupleN((prior, set, produceOrdinal(2))))
              }
              case SilDeterminedReference(
                SilStateSpecifiedReference(
                  SilMandatorySingular(
                    SilNounReference(
                      SilWordLemma(lemma),
                      _
                    )
                  ),
                  SilPropertyState(SilWordLemma(qualifier))
                ),
                DETERMINER_NONSPECIFIC
              ) if (matchLemma(lemma)) => {
                getSentencePrinter.sb.ordinalValue(qualifier) match {
                  case Success(ordinal) => {
                    Some(tupleN((prior, set, produceOrdinal(ordinal))))
                  }
                  case _ => None
                }
              }
              case _ => None
            }
          }
        }
        val selected = {
          if (qualifiers.isEmpty && (ordered.size <= 1)) {
            ordered
          } else {
            val (ordinalOpt, misqualified) = {
              if (qualifiers.isEmpty) {
                nounLemma match {
                  case LEMMA_FORMER => {
                    tupleN((Some(1), ordered.size < 1))
                  }
                  case LEMMA_LATTER => {
                    tupleN((Some(2), ordered.size < 2))
                  }
                  case _ => {
//                     tupleN((None, true))
                     tupleN((Some(1), false))
                  }
                }
              } else if (qualifiers.size == 1) {
                val qualifier = qualifiers.head
                val qualifierOrdinalOpt = qualifier match {
                  case LEMMA_FORMER => Some(1)
                  case LEMMA_LATTER | LEMMA_OTHER => Some(2)
                  case _ => {
                    getSentencePrinter.sb.ordinalValue(qualifier) match {
                      case Success(o) => Some(o)
                      case _ => None
                    }
                  }
                }
                tupleN((qualifierOrdinalOpt, false))
              } else {
                tupleN((None, false))
              }
            }
            def failMisqualified() = {
              getMind.getCosmos.fail(
                ShlurdExceptionCode.MisqualifiedNoun,
                getSentencePrinter.sb.respondMisqualifiedNoun(
                  noun, qualifiers.toSeq))
            }
            if (misqualified) {
              return failMisqualified
            }
            ordinalOpt match {
              case Some(ordinal) => {
                val filtered = {
                  if (ordered.size > 1) {
                    ordered.filter(_._3 == ordinal)
                  } else {
                    Seq.empty
                  }
                }
                if (filtered.isEmpty) {
                  return failMisqualified
                }
                filtered
              }
              case _ => Seq.empty
            }
          }
        }
        selected.map {
          case ((prior, set, _)) => SmcScopeOutput(Some(prior), set)
        }
      } else {
        Seq.empty
      }
    }
    if (outputs.isEmpty) {
      parent.resolveQualifiedNoun(noun, context, qualifiers)
    } else {
      assert(outputs.size == 1)
      Success(outputs.head)
    }
  }

  override def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    val outputs = ref match {
      case SilPronounReference(PERSON_THIRD, _, _, DISTANCE_UNSPECIFIED) => {
        findMatchingPronounReference(refMap, ref)
      }
      case _ => Seq.empty
    }
    if (outputs.isEmpty) {
      parent.resolvePronoun(communicationContext, ref)
    } else {
      resolveOutput(ref, outputs)
    }
  }
}
