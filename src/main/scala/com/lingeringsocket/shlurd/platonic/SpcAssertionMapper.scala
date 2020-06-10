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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

import SprEnglishLemmas._

import org.slf4j._

class SpcAssertionBinding(
  val annotator : SpcAnnotator,
  val refMapIn : SpcRefMap,
  val refMapOut : Option[SpcMutableRefMap],
  val placeholderMap : Option[SpcRefMap] = None,
  var verbMatched : Boolean = false
)
{
  def unifyReferences(
    ref1 : SilReference,
    ref2 : SilReference)
  {
    tupleN((ref1, ref2)) match {
      case (ar1 : SilAnnotatedReference, ar2 : SilAnnotatedReference) => {
        SmcAnnotator.unifyReferences[SpcEntity, SpcRefNote](annotator, ar1, ar2)
      }
      case _ => {
      }
    }
  }
}

object SpcAssertionMapper
{
  private val logger =
    LoggerFactory.getLogger(classOf[SpcAssertionMapper])
}

class SpcAssertionMapper(
  mind : SpcMind,
  communicationContext : SmcCommunicationContext[SpcEntity],
  inputRewriter : SmcInputRewriter[SpcEntity, SpcProperty, SpcCosmos],
  sentencePrinter : SilSentencePrinter)
    extends SmcDebuggable(new SmcDebugger(SpcAssertionMapper.logger))
{
  import SpcImplicationMapper._

  type MindScopeType = SmcMindScope[
    SpcEntity, SpcProperty, SpcCosmos, SpcMind
  ]

  private[platonic] def matchSubsumption(
    annotator : SpcAnnotator,
    cosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    refMap : SpcRefMap
  ) : Boolean =
  {
    val operator = "SUBSUMES"
    trace(s"ATTEMPT MATCH $general $operator $specific")
    val matched = matchGeneralization(
      cosmos, general, specific,
      new SpcAssertionBinding(
        annotator,
        refMap,
        None),
      0
    )._1
    if (matched && !isTraceEnabled) {
      debug(s"MATCH SUCCESSFUL $general $operator $specific}")
    }
    matched
  }

  private def reannotate[PhraseType <: SilPhrase](
    annotator : SpcAnnotator,
    phrase : PhraseType) : PhraseType =
  {
    annotator.copy(phrase, SilPhraseCopyOptions(
      preserveNotes = true))
  }

  def matchImplication(
    operator : String,
    cosmos : SpcCosmos,
    implication : SilConditionalSentence,
    predicate : SilPredicate,
    binding : SpcAssertionBinding
  ) : Option[SilPredicate] =
  {
    matchImplicationPlusAlternative(
      operator,
      cosmos, implication,
      predicate, Seq.empty, None,
      binding, 0)._1
  }

  private[platonic] def matchImplicationPlusAlternative(
    operator : String,
    cosmos : SpcCosmos,
    conditional : SilConditionalSentence,
    predicate : SilPredicate,
    additionalConsequents : Seq[SilPredicateSentence],
    alternative : Option[SilPredicateSentence],
    binding : SpcAssertionBinding,
    triggerDepth : Int
  ) : (
    Option[SilPredicate],
    Seq[SilPredicateSentence],
    Option[SilPredicateSentence]) =
  {
    val antecedent = conditional.antecedent
    val consequent = conditional.consequent
    trace(s"ATTEMPT MATCH $antecedent $operator $consequent")

    val (matched, replacements) = matchGeneralization(
      cosmos,
      antecedent,
      predicate,
      binding,
      triggerDepth)
    if (matched) {
      if (!isTraceEnabled) {
        debug(s"MATCH SUCCESSFUL $antecedent $operator $consequent}")
      }
      val rewriter = new SilPhraseRewriter(binding.annotator)
      def replaceReferences = rewriter.replacementMatcher(
        "replaceReferences", {
          case ref : SilReference => {
            replacements.get(ref) match {
              case Some(replacement) => {
                // avoid aliased references within tree
                val copied = binding.annotator.copy(
                  replacement,
                  SilPhraseCopyOptions(
                    preserveNotes = true))
                // FIXME should do this for entire subtree (not just root)
                binding.refMapOut.foreach(refMap => {
                  refMap.get(replacement).foreach(entities => {
                    refMap.put(copied, entities)
                  })
                })
                copied
              }
              case _ => ref
            }
          }
        }
      )
      val newPredicate = rewriter.rewrite(
        replaceReferences,
        reannotate(binding.annotator, consequent))
      debug(s"MATCH FROM ${predicate}\nPRODUCES $newPredicate")
      val newAdditional = additionalConsequents.map(sentence => {
        rewriter.rewrite(
          replaceReferences,
          reannotate(binding.annotator, sentence))
      })
      newAdditional.foreach(sentence => {
        debug(s"WITH ADDITIONAL CONSEQUENT $sentence")
      })
      val newAlternative = alternative.map(sentence => {
        rewriter.rewrite(
          replaceReferences,
          reannotate(binding.annotator, sentence))
      })
      newAlternative.foreach(sentence => {
        debug(s"WITH ALTERNATIVE $sentence")
      })
      tupleN((Some(newPredicate), newAdditional, newAlternative))
    } else {
      tupleN((None, Seq.empty, None))
    }
  }

  private def isEquivalentVerb(
    verb1 : SilWord, verb2 : SilWord, triggerDepth : Int) : Boolean =
  {
    if (triggerDepth > 0) {
      verb1.toLemma == verb2.toLemma
    } else {
      mind.isEquivalentVerb(verb1, verb2)
    }
  }

  private def prepareReplacement(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    binding : SpcAssertionBinding
  ) : Boolean =
  {
    val patternMatched = prepareReplacementImpl(
      cosmos,
      replacements,
      ref,
      actualRef,
      binding)
    if (patternMatched) {
      true
    } else {
      // FIXME we should prefer entity comparisons instead, but for that
      // we need two refMaps simultaneously
      if (ref != actualRef) {
        trace(s"PHRASE $ref DOES NOT MATCH $actualRef")
        false
      } else {
        true
      }
    }
  }

  private def expandPatterns(
    set : Set[SilReference],
    binding : SpcAssertionBinding) : Set[SilReference] =
  {
    binding.placeholderMap match {
      case Some(placeholderMap) => {
        set ++ set.flatMap(
          ref => findPlaceholderCorrespondence(
            binding.annotator, ref, binding.placeholderMap
          )._2.map(r => {
            flipVariable(binding.annotator, sentencePrinter, r, r)
          }))
      }
      case _ => set
    }
  }

  private def prepareReplacementImpl(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    binding : SpcAssertionBinding
  ) : Boolean =
  {
    tupleN((
      findPlaceholderCorrespondence(
        binding.annotator, ref, binding.placeholderMap),
      extractNoun(ref)
    )) match {
      case ((true, patternRefSetUnexpanded), Some(noun)) => {
        val patternRefSet = expandPatterns(
          patternRefSetUnexpanded, binding)
        val resolvedForm = mind.resolveForm(noun)
        if (SmcPhraseQuerier.containsWildcard(actualRef, false, false, false)) {
          val wildcardRef = actualRef match {
            // FIXME need the same treatment for other variables, but
            // with intersectionality
            case SilDeterminedReference(
              SilCountedNounReference(SilWordLemma(LEMMA_WHAT), count),
              _ : SilUnlimitedDeterminer
            ) => {
              resolvedForm match {
                case Some(restrictedForm) => {
                  val typedRef = binding.annotator.determinedNounRef(
                    SilWord(restrictedForm.name),
                    DETERMINER_VARIABLE,
                    count
                  )
                  binding.unifyReferences(actualRef, typedRef)
                  typedRef
                }
                case _ => actualRef
              }
            }
            case _ => actualRef
          }
          patternRefSet.foreach(patternRef => {
            replacements.put(patternRef, wildcardRef)
          })
          return true
        }
        val (candidateRef, entities) = actualRef match {
          case pr : SilPronounReference => {
            // FIXME make use of prior reference too
            val mindScope = new MindScopeType(mind, sentencePrinter)
            val scope = new SmcPhraseScope(binding.refMapIn, mindScope)
            scope.resolvePronoun(
              binding.annotator, communicationContext, pr
            ) match {
              case Success(SmcScopeOutput(_, entities)) if (
                !entities.isEmpty
              ) => {
                tupleN((
                  mind.specificReferences(binding.annotator, entities),
                  entities))
              }
              case _ =>  {
                trace(s"PRONOUN $pr UNRESOLVED")
                return false
              }
            }
          }
          case SilDeterminedReference(
            _ : SilNounReference,
            DETERMINER_NONSPECIFIC
          ) => {
            trace(s"NONSPECIFIC REFERENCE $actualRef")
            return false
          }
          case SilQuotationReference(_, _) => {
            tupleN((actualRef, Set.empty[SpcEntity]))
          }
          case _ => {
            binding.refMapIn.get(actualRef) match {
              case Some(entities) => {
                tupleN((actualRef, entities))
              }
              case _ => {
                if (resolvedForm.isEmpty) {
                  tupleN((actualRef, Set.empty[SpcEntity]))
                } else {
                  trace(s"UNRESOLVED REFERENCE $actualRef")
                  return false
                }
              }
            }
          }
        }
        val replacementRef = resolvedForm match {
          case Some(form) => {
            val filtered = entities.filter(entity => {
              cosmos.isHyponym(entity.form, form)
            })
            if (filtered.isEmpty) {
              trace(s"NO FORM ${entities.map(_.form)} MATCHES $form")
              return false
            }
            val conjunction = {
              if (filtered.size == 1) {
                mind.specificReference(
                  binding.annotator, filtered.head, DETERMINER_DEFINITE)
              } else {
                binding.annotator.conjunctiveRef(
                  DETERMINER_ALL,
                  filtered.toSeq.map(entity => {
                    val entityRef = mind.specificReference(
                      binding.annotator,
                      entity, DETERMINER_DEFINITE)
                    binding.refMapOut.foreach(
                      _.put(entityRef, Set(entity)))
                    entityRef
                  })
                )
              }
            }
            binding.refMapOut.foreach(_.put(conjunction, filtered))
            if (filtered.size == entities.size) {
              binding.unifyReferences(actualRef, conjunction)
            }
            conjunction
          }
          case _ => candidateRef
        }
        patternRefSet.foreach(patternRef => {
          replacements.put(patternRef, replacementRef)
        })
        true
      }
      case _ => {
        trace(s"REFERENCE PATTERN $ref UNSUPPORTED")
        false
      }
    }
  }

  private[platonic] def matchGeneralization(
    cosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    binding : SpcAssertionBinding,
    triggerDepth : Int
  ) : (Boolean, Map[SilReference, SilReference]) =
  {
    val replacements = new mutable.LinkedHashMap[SilReference, SilReference]
    def unmatched() = tupleN((false, replacements))
    general match {
      // FIXME match on verb; e.g. "is" implies "is" and "becomes" implies "is",
      // but "is" may not imply "becomes"
      case SilStatePredicate(
        subject, _, state, modifiers
      ) => {
        val statePredicate = specific match {
          case sp : SilStatePredicate => sp
          case _ => {
            trace(s"PREDICATE $specific IS NOT A STATE")
            return unmatched
          }
        }
        tupleN((state, statePredicate.state)) match {
          // FIXME allow other variable patterns
          case (
            SilAdpositionalState(adp1, obj1),
            SilAdpositionalState(adp2, obj2)
          ) if (adp1 == adp2) => {
            if (!prepareReplacement(
              cosmos, replacements, obj1,
              obj2, binding))
            {
              return unmatched
            }
          }
          case _ => {
            if (state != statePredicate.state) {
              trace(s"STATE $state " +
                s"DOES NOT MATCH ${statePredicate.state}")
              return unmatched
            }
          }
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, statePredicate.subject,
          binding))
        {
          return unmatched
        }
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        val relPredicate = specific match {
          case rp : SilRelationshipPredicate => rp
          case _ => {
            trace(s"PREDICATE $specific IS NOT A RELATIONSHIP")
            return unmatched
          }
        }
        if (verb != relPredicate.verb) {
          trace(s"RELATIONSHIP ${relPredicate.verb} DOES NOT MATCH")
          return unmatched
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, relPredicate.subject,
          binding))
        {
          return unmatched
        }
        if (!prepareReplacement(
          cosmos, replacements, complement,
          relPredicate.complement,
          binding))
        {
          return unmatched
        }
      }
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        val actionPredicate = specific match {
          case ap : SilActionPredicate => ap
          case _ => {
            trace(s"PREDICATE $specific IS NOT AN ACTION")
            return unmatched
          }
        }
        if (!isEquivalentVerb(verb, actionPredicate.verb, triggerDepth)) {
          def lemma = actionPredicate.verb.toLemma
          trace(s"ACTION $lemma DOES NOT MATCH")
          return unmatched
        }
        binding.verbMatched = true
        if (!prepareReplacement(
          cosmos, replacements, subject, actionPredicate.subject,
          binding))
        {
          return unmatched
        }
        directObject.foreach(obj => {
          actionPredicate.directObject match {
            case Some(actualObj) => {
              if (!prepareReplacement(
                cosmos, replacements, obj, actualObj,
                binding))
              {
                return unmatched
              }
            }
            case _ => {
              trace(s"DIRECT OBJECT MISSING")
              return unmatched
            }
          }
        })
        // FIXME support multiple modifiers and other patterns
        val filteredModifiers = modifiers.filterNot(_ match {
          case staticModifier @ (
            _ : SilBasicVerbModifier |
              // FIXME should be using reference resolution for matching
              // instead
              SilAdpositionalVerbModifier(
                _,
                SilDeterminedReference(
                  _ : SilNounReference, DETERMINER_DEFINITE
                )
              )
          ) => {
            if (actionPredicate.modifiers.contains(staticModifier)) {
              // matched:  discard
              true
            } else {
              trace(s"VERB MODIFIER $staticModifier MISSING")
              return unmatched
            }
          }
          // keep
          case _ => false
        })
        filteredModifiers match {
          case Seq(SilAdpositionalVerbModifier(
            adposition, objRef
          )) if (
            findPlaceholderCorrespondence(
              binding.annotator, objRef, binding.placeholderMap)._1
          ) => {
            // FIXME some modifiers (e.g. "never") might
            // negate the match
            val actualRefs = actionPredicate.modifiers.flatMap(_ match {
              case SilAdpositionalVerbModifier(
                SilAdposition(adposition.word), actualRef
              ) => {
                Some(actualRef)
              }
              case _ => None
            })
            if (actualRefs.size != 1) {
              trace("VERB MODIFIER PATTERN DOES NOT MATCH")
              return unmatched
            } else {
              if (!prepareReplacement(
                cosmos, replacements, objRef, actualRefs.head,
                binding))
              {
                return unmatched
              }
            }
          }
          case Seq() => {
          }
          case _ => {
            trace("VERB MODIFIER PATTERN UNSUPPORTED")
            return unmatched
          }
        }
      }
      case _ => {
        trace("ANTECEDENT PATTERN UNSUPPORTED")
        return unmatched
      }
    }
    if (isTraceEnabled) {
      debug("MATCH SUCCESSFUL")
    }
    tupleN((true, replacements))
  }
}
