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
  type AnnotatorType = SmcAnnotator[EntityType, SmcRefNote[EntityType]]

  def getMind : MindType

  def getSentencePrinter : SilSentencePrinter

  def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[SmcScopeOutput[EntityType]]

  def resolvePronoun(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]]

  def resolveSpatialDeictic(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[EntityType],
    word : SilWord,
    proximity : SilProximity
  ) : Try[SmcScopeOutput[EntityType]]

  // FIXME this is English-specific
  private def foldThem(pr : SilPronounReference) : SilPronounReference =
  {
    pr match {
      case SilPronounReference(
        PERSON_THIRD, GENDER_SOMEONE, COUNT_PLURAL,
        PROXIMITY_UNSPECIFIED | PROXIMITY_REFLEXIVE
      ) => {
        pr.copy(gender = GENDER_NEUTER)
      }
      case _ => pr
    }
  }

  // FIXME context
  private def foldReflexives(pr : SilPronounReference) : SilPronounReference =
  {
    if (pr.isReflexive) {
      pr.copy(proximity = PROXIMITY_UNSPECIFIED)
    } else {
      pr
    }
  }

  private def foldSpecialCases(pr : SilPronounReference) : SilPronounReference =
  {
    foldThem(foldReflexives(pr))
  }

  private def getPredicate(ref : SilReference) : Option[SilPredicate] =
  {
    ref match {
      case ar : SilAnnotatedReference => {
        ar.getAnnotator.getBasicNote(ar).getPredicate
      }
      case _ => None
    }
  }

  private def getContext(ref : SilReference) : SilReferenceContext =
  {
    val contextOpt = ref match {
      case ar : SilAnnotatedReference => {
        ar.getAnnotator.getBasicNote(ar).getContext
      }
      case _ => None
    }
    contextOpt.getOrElse(REF_SUBJECT)
  }

  private def isReflexiveMatch(
    p1 : SilPronounReference, p2 : SilPronounReference) : Boolean =
  {
    (getContext(p1) == REF_SUBJECT) && p2.isReflexive
  }

  private def isNonReflexiveMatch(
    p1 : SilPronounReference, p2 : SilPronounReference,
    flipped : Boolean = false) : Boolean =
  {
    // FIXME what about "he kicked him and his dog out of the house"?
    val c1 = getContext(p1)
    val c2 = getContext(p2)
    if (c1 == REF_SUBJECT) {
      c2 match {
        case REF_DIRECT_OBJECT | REF_ADPOSITION_OBJ => false
        case _ => true
      }
    } else if (flipped) {
      true
    } else {
      isNonReflexiveMatch(p2, p1, true)
    }
  }

  private def isPronounMatch(
    p1 : SilPronounReference, p2 : SilPronounReference,
    samePhrase : Boolean) : Boolean =
  {
    if (foldSpecialCases(p1) != foldSpecialCases(p2)) {
      false
    } else {
      val reflexiveMatch = {
        if (p1.isReflexive || p2.isReflexive) {
          if (!samePhrase) {
            false
          } else if (p1.isReflexive && p2.isReflexive) {
            true
          } else {
            isReflexiveMatch(p1, p2) || isReflexiveMatch(p2, p1)
          }
        } else if (samePhrase) {
          isNonReflexiveMatch(p1, p2)
        } else {
          true
        }
      }
      val pronounMatch = {
        if (p1.word == p2.word) {
          true
        } else if (p1.word.nonEmpty) {
          p2.pronounMap.values.exists(_ == p1.word.get)
        } else if (p2.word.nonEmpty) {
          p1.pronounMap.values.exists(_ == p2.word.get)
        } else {
          (p1.gender == GENDER_SOMEWHERE) && (p2.gender == GENDER_SOMEWHERE)
        }
      }
      reflexiveMatch && pronounMatch
    }
  }

  protected def findMatchingPronounReference(
    annotator : AnnotatorType,
    refMap : SmcRefMap[EntityType],
    reference : SilPronounReference,
    samePhrase : Boolean) : Seq[SmcScopeOutput[EntityType]] =
  {
    val refPredicate = getPredicate(reference)
    var skip = false
    refMap.filter {
      case (prior, set) => {
        if (skip) {
          false
        } else {
          if (prior eq reference) {
            skip = true
            false
          } else {
            val axis = reference.gender match {
              case GENDER_SOMEWHERE => DEICTIC_SPATIAL
              case _ => DEICTIC_PERSONAL
            }
            getMind.thirdPersonDeictic(
              annotator, set, axis
            ) match {
              case Some(pr : SilPronounReference) => {
                val note = annotator.getNote(pr)
                note.setContext(getContext(prior))
                val priorPredicate = getPredicate(prior)
                priorPredicate.foreach(
                  predicate => note.setPredicate(predicate))
                val reallySamePhrase = {
                  if (samePhrase) {
                    priorPredicate == refPredicate
                  } else {
                    false
                  }
                }
                isPronounMatch(pr, reference, reallySamePhrase)
              }
              case _ => false
            }
          }
        }
      }
    }.toSeq.map {
      case (prior, set) => {
        val reannotated = annotator.copy(
          prior,
          SilPhraseCopyOptions(preserveNotes = true))
        SmcScopeOutput(Some(reannotated), set)
      }
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

  override def resolveSpatialDeictic(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[EntityType],
    word : SilWord,
    proximity : SilProximity
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    if (proximity.isInstanceOf[SilThereLimitedProximity]) {
      if (mind.isConversing) {
        val reference = annotator.pronounRef(
          PERSON_THIRD,
          GENDER_SOMEWHERE,
          COUNT_SINGULAR,
          mind,
          proximity)
        val outputs = {
          mind.getConversation.getUtterances.reverseIterator.drop(1).map(
            utterance => {
              findMatchingPronounReference(
                annotator,
                utterance.refMap, reference, false
              )
            }
          ).find(_.nonEmpty).getOrElse(Seq.empty)
        }
        val resolved = resolveOutput(reference, outputs)
        if (resolved.isSuccess) {
          return resolved
        }
      }
    }

    val entityOpt = proximity match {
      case _ : SilHereProximity =>
        communicationContext.speakerEntity
      case _ : SilThereLimitedProximity =>
        communicationContext.listenerEntity
      case _ => None
    }
    entityOpt match {
      case Some(entity) => {
        Success(SmcScopeOutput(None, Set(entity)))
      }
      case None => {
        mind.getCosmos.fail(
          ShlurdExceptionCode.UnknownModifier,
          sentencePrinter.sb.respondUnknownModifier(word))
      }
    }
  }

  override def resolvePronoun(
    annotator : AnnotatorType,
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
        if (reference.isDemonstrative) {
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
                  annotator,
                  utterance.refMap, reference, false
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
                  SilMandatorySingular(alias),
                  DETERMINER_DEFINITE
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
                SilNounLemmaReference(lemma),
                _ : SilIndefiniteDeterminer
              ) if (matchLemma(lemma)) => {
                Some(tupleN((prior, set, produceOrdinal(1))))
              }
              case SilStateSpecifiedReference(
                SilMandatorySingular(
                  SilWordLemma(lemma)
                ),
                SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
              ) if (matchLemma(lemma)) => {
                Some(tupleN((prior, set, produceOrdinal(2))))
              }
              case SilDeterminedReference(
                SilStateSpecifiedReference(
                  SilNounReference(
                    SilWordLemma(lemma)
                  ),
                  SilPropertyState(SilWordLemma(qualifier))
                ),
                _ : SilIndefiniteDeterminer
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

  override def resolveSpatialDeictic(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[EntityType],
    word : SilWord,
    proximity : SilProximity
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    // FIXME handle cases such as "While I was at Starbucks, I saw my
    // friend there"
    parent.resolveSpatialDeictic(
      annotator, communicationContext, word, proximity)
  }

  override def resolvePronoun(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    val outputs = ref match {
      case SilPronounReference(
        PERSON_THIRD, _, _, PROXIMITY_UNSPECIFIED | PROXIMITY_REFLEXIVE
      ) => {
        findMatchingPronounReference(annotator, refMap, ref, true)
      }
      case _ => Seq.empty
    }
    if (outputs.isEmpty) {
      parent.resolvePronoun(annotator, communicationContext, ref)
    } else {
      resolveOutput(ref, outputs)
    }
  }
}
