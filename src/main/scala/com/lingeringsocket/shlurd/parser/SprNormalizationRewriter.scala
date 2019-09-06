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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import SprEnglishLemmas._

private[parser] object SprNormalizationRewriter
{
  private val compassRose = Set("north", "south", "east", "west")
}

private[parser] class SprNormalizationRewriter(context : SprContext)
  extends SilPhraseRewriter(context.annotator) with SprEnglishWordAnalyzer
{
  import SprNormalizationRewriter._

  private def annotator = context.annotator

  def normalize(sentence : SilSentence) : SilSentence =
  {
    rewrite(normalizeAllPhrases, sentence, SilRewriteOptions(repeat = true))
  }

  private def normalizeAllPhrases = combineRules(
    normalizeCompass,
    normalizeGenitives,
    normalizeCoordinatingDeterminers,
    normalizeDanglingAdpositions,
    normalizeCompoundAdpositions,
    normalizeAdpositionalPhrases,
    normalizeDeterminedSpecifiers,
    normalizeCommands
  )

  private def normalizeGenitives = replacementMatcher(
    "normalizeGenitives", {
      case SilGenitiveReference(r1, SilGenitiveReference(r2, r3)) => {
        annotator.genitiveRef(
          annotator.genitiveRef(r1, r2), r3)
      }
    }
  )

  private def normalizeCoordinatingDeterminers = replacementMatcher(
    "normalizeCoordinatingDeterminers", {
      case SilStatePredicate(
        subject,
        verb,
        SilConjunctiveState(
          DETERMINER_ANY, states, separator),
        verbModifiers
      ) if (findCoordinatingDeterminer(verbModifiers).nonEmpty) => {
        val (modifier, lemma) = findCoordinatingDeterminer(verbModifiers).get
        val determiner = maybeDeterminerFor(lemma).get
        SilStatePredicate(
          subject,
          verb,
          SilConjunctiveState(
            determiner, states, separator),
          verbModifiers.filterNot(_ == modifier)
        )
      }
      case SilRelationshipPredicate(
        subject,
        verb,
        SilConjunctiveReference(
          DETERMINER_ANY, references, separator),
        verbModifiers
      ) if (findCoordinatingDeterminer(verbModifiers).nonEmpty) => {
        val (modifier, lemma) = findCoordinatingDeterminer(verbModifiers).get
        val determiner = maybeDeterminerFor(lemma).get
        SilRelationshipPredicate(
          subject,
          verb,
          annotator.conjunctiveRef(
            determiner, references, separator),
          verbModifiers.filterNot(_ == modifier)
        )
      }
    }
  )

  private def normalizeDanglingAdpositions = replacementMatcher(
    "normalizeDanglingAdposition", {
      case SilPredicateQuery(
        SilActionPredicate(subject, verb, directObject, modifiers),
        question,
        INFLECT_ACCUSATIVE,
        tam,
        formality
      ) if (modifiers.exists(
        m => !SilUtils.getDanglingAdposition(m).isEmpty)
      ) => {
        SilPredicateQuery(
          SilActionPredicate(
            subject, verb, directObject,
            modifiers.flatMap(modifier => {
              SilUtils.getDanglingAdposition(modifier) match {
                case Some(adposition) => {
                  Seq.empty
                }
                case _ => {
                  Seq(modifier)
                }
              }
            })
          ),
          question,
          INFLECT_ADPOSITIONED,
          tam,
          formality)
      }
    }
  )

  // FIXME generalize this
  private def normalizeCompoundAdpositions = replacementMatcher(
    "normalizeCompoundAdpositions", {
      case SilAdpositionalState(
        adp1 : SilAdposition,
        SilDeterminedReference(
          SilStateSpecifiedReference(
            SilMandatorySingular(
              word : SilSimpleWord
            ),
            SilAdpositionalState(
              adp2 : SilAdposition,
              objRef
            )
          ),
          DETERMINER_UNIQUE
        )
      ) if (word.lemma == LEMMA_LEFT) || (word.lemma == LEMMA_RIGHT) => {
        SilAdpositionalState(
          SilAdposition(
            adp1.word.decomposed ++ Seq(SilWord(LEMMA_THE), word) ++
              adp2.word.decomposed),
          objRef
        )
      }
    }
  )

  private def normalizeCompass = replacementMatcher(
    "normalizeCompass", {
      case SilRelationshipPredicate(
        subject,
        SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
        SilStateSpecifiedReference(
          SilNounReference(
            direction : SilSimpleWord
          ),
          SilAdpositionalState(
            adp,
            landmark)
        ),
        modifiers
      ) if (
        (adp == SilAdposition.OF) && compassRose.contains(direction.lemma)
      ) => {
        SilStatePredicate(
          subject,
          STATE_PREDEF_BE.toVerb,
          SilAdpositionalState(
            SilAdposition(direction +: adp.word.decomposed),
            landmark
          ),
          modifiers
        )
      }
      case SilStatePredicate(
        subject,
        verb,
        SilPropertyState(direction : SilSimpleWord),
        Seq(SilAdpositionalVerbModifier(adp, landmark))
      ) if (
        (adp == SilAdposition.OF) && compassRose.contains(direction.lemma)
      ) => {
        SilStatePredicate(
          subject,
          verb,
          SilAdpositionalState(
            SilAdposition(direction +: adp.word.decomposed),
            landmark),
          Seq.empty)
      }
    }
  )

  private def normalizeAdpositionalPhrases = replacementMatcher(
    "normalizeAdpositionalPhrases", {
      case SilStatePredicate(
        subject,
        verb,
        state,
        modifiers
      ) => {
        val (subjectExtracted, subjectModifiers) =
          extractVerbModifier(subject)
        SilStatePredicate(
          subjectExtracted,
          verb,
          state,
          subjectModifiers ++ modifiers
        )
      }
      case SilRelationshipPredicate(
        subject,
        verb,
        complement,
        modifiers
      ) => {
        val (subjectExtracted, subjectModifiers) =
          extractVerbModifier(subject)
        val (complementExtracted, complementModifiers) =
          extractVerbModifier(complement)
        SilRelationshipPredicate(
          subjectExtracted,
          verb,
          complementExtracted,
          subjectModifiers ++ complementModifiers ++ modifiers)
      }
      case SilActionPredicate(
        subject,
        verb,
        directObject,
        modifiers
      ) => {
        val (subjectExtracted, subjectModifiers) =
          extractVerbModifier(subject)
        val (directObjectExtracted, directObjectModifiers) =
          directObject match {
            case Some(obj) => {
              val (r, m) = extractVerbModifier(obj)
              tupleN((Some(r), m))
            }
            case _ => (None, Seq.empty)
          }
        SilActionPredicate(
          subjectExtracted,
          verb,
          directObjectExtracted,
          subjectModifiers ++ directObjectModifiers ++ modifiers)
      }
    }
  )

  private def normalizeDeterminedSpecifiers = replacementMatcher(
    "normalizeDeterminedSpecifiers", {
      case SilStateSpecifiedReference(
        SilDeterminedReference(ref, determiner),
        state
      ) => {
        annotator.determinedRef(
          annotator.stateSpecifiedRef(ref, state),
          determiner)
      }
    }
  )

  private def normalizeCommands = replacementMatcher(
    "normalizeCommands", {
      case SilPredicateSentence(
        actionPredicate : SilActionPredicate, tam, formality
      ) if (actionPredicate.directObject.nonEmpty && tam.isImperative) => {
        val (stateSpecifiers, verbModifiers) =
          actionPredicate.modifiers.partition(
            _ match {
              case SilAdpositionalVerbModifier(adposition, objRef) => {
                !isAdverbialAdposition(None, adposition, objRef)
              }
              case _ => {
                false
              }
            }
          )
        val directObject = annotator.stateQualifiedRef(
          actionPredicate.directObject.get,
          stateSpecifiers.map(_.asInstanceOf[SilAdpositionalVerbModifier]).map(
            vm => SilAdpositionalState(vm.adposition, vm.objRef)))
        SilPredicateSentence(
          SilActionPredicate(
            actionPredicate.subject,
            actionPredicate.verb,
            Some(directObject),
            verbModifiers
          ),
          tam,
          formality
        )
      }
    }
  )

  private def extractVerbModifier(ref : SilReference)
      : (SilReference, Seq[SilVerbModifier]) =
  {
    ref match {
      case SilOptionallyDeterminedReference(
        SilStateSpecifiedReference(
          sub,
          SilAdpositionalState(adposition, objRef)
        ),
        determiner
      ) if (isAdverbialAdposition(Some(sub), adposition, objRef)) => {
        tupleN((
          annotator.determinedRef(sub, determiner),
          Seq(SilAdpositionalVerbModifier(adposition, objRef))))
      }
      case _ => (ref, Seq.empty)
    }
  }

  private def isAdverbialAdposition(
    ref : Option[SilReference], adposition : SilAdposition,
    objRef : SilReference) : Boolean =
  {
    // FIXME the real thing has to be sophisticated enough to understand
    // that "what happened the day before the fight" involves a state specifier
    // whereas "where was the football before the kitchen" and
    // "bow before the throne" involve adverbial phrases.  And in some cases,
    // we should leave it ambiguous and try it both ways.
    adposition.word.toLemma match {
      case LEMMA_BEFORE | LEMMA_AFTER | LEMMA_TO => true
      case LEMMA_AT => {
        objRef match {
          case SilMandatorySingular(
            _
          ) => {
            false
          }
          case _ => {
            true
          }
        }
      }
      case _ => false
    }
  }

  private def findCoordinatingDeterminer(
    verbModifiers : Seq[SilVerbModifier]) :
      Option[(SilVerbModifier, String)] =
  {
    verbModifiers.toStream.flatMap(modifier => modifier match {
      case SilBasicVerbModifier(word) => {
        val lemma = word.toLemma
        if (isCoordinatingDeterminer(lemma)) {
          Some(tupleN((modifier, lemma)))
        } else {
          None
        }
      }
      case _ => None
    }).headOption
  }
}

