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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.cli._

import scala.collection._

class PhlebClock
{
  private var timestamp = SpcTimestamp.ZERO

  def getTimestamp() = timestamp

  def startNewTurn()
  {
    timestamp = timestamp.successor
  }
}

class PhlebMind(
  cosmos : SpcCosmos,
  val perception : Option[SpcPerception],
  val preferredSynonyms : mutable.Map[SpcIdeal, String],
  val clock : PhlebClock
) extends ShlurdCliMind(cosmos, preferredSynonyms)
{
  private lazy val mapLocationForm =
    cosmos.resolveForm(PhlebShell.MAP_PLACE_WORD).get

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new PhlebMind(
      newCosmos, perception, preferredSynonyms, clock)
    mind.initFrom(this)
    mind
  }

  override def responseReference(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[SpcEntity],
    entity : SpcEntity,
    determiner : SilDeterminer) : SilReference =
  {
    val earliestTimestamp =
      perception.flatMap(
        _.getEntityEarliestTimestamp(entity)
      ).getOrElse(
        clock.getTimestamp
      )
    val perceivedDeterminer = {
      if (earliestTimestamp == clock.getTimestamp) {
        DETERMINER_NONSPECIFIC
      } else {
        determiner
      }
    }
    val ref =
      super.responseReference(
        annotator, communicationContext, entity, determiner)
    val perceivedRef =
      super.responseReference(
        annotator, communicationContext, entity, perceivedDeterminer)
    val composed = composeReference(
      annotator, communicationContext, entity, ref)

    val rewriter = new SilPhraseRewriter(annotator)
    def replaceReferences = rewriter.replacementMatcher(
      "replaceReferences", {
        case r : SilReference => {
          if (r == ref) {
            perceivedRef
          } else {
            r
          }
        }
      }
    )
    rewriter.rewrite(
      replaceReferences,
      composed)
  }

  def composeReference(
    annotator : SpcAnnotator,
    communicationContext : SmcCommunicationContext[SpcEntity],
    entity : SpcEntity,
    ref : SilReference
  ) : SilReference =
  {
    val mind = this
    val annotatorMap = SmcResultCollector.newAnnotationRefMap(annotator)
    val refMap = SmcResultCollector.modifiableRefMap(mind, annotatorMap)
    refMap.put(ref, Set(entity))
    val speakerRef = annotator.determinedNounRef(
      SilWord("game-speaker"),
      DETERMINER_DEFINITE)
    communicationContext.speakerEntity.foreach(personEntity => {
      refMap.put(speakerRef, Set(personEntity))
    })

    val predicate = SilActionPredicate(
      speakerRef,
      SilWord("reference"),
      Some(ref)
    )
    val responder = new PhlebResponder(
      mind,
      SpcBeliefParams(IGNORE_BELIEFS, false, false, false, false),
      SmcResponseParams(),
      new SmcExecutor,
      communicationContext)
    def newAssertionMapper = new SpcAssertionMapper(
      mind, communicationContext,
      new SmcInputRewriter(mind, annotator),
      new SilSentencePrinter)
    def replacements(p : SilPredicate) = {
      val resultCollector = SpcResultCollector(annotator, refMap)
      responder.resolveReferences(p, resultCollector)
      responder.getTriggers.flatMap(
        responder.getTriggerImplications(annotator, _)
      ).flatMap {
        case (conditionalSentence, placeholderMap) => {
          newAssertionMapper.matchImplication(
            "IMPLIES",
            getCosmos,
            conditionalSentence,
            p,
            SpcAssertionBinding(
              annotator,
              refMap,
              Some(refMap),
              Some(placeholderMap)
            )
          )
        }
      }
    }
    def recurse(p : SilPredicate) : SilReference = {
      // FIXME choose best match instead of last
      replacements(p).lastOption match {
        case Some(SilActionPredicate(
          _, SilWordLemma("compose"), Some(obj), _
        )) => {
          obj
        }
        case Some(SilActionPredicate(
          _, SilWordLemma("recite"), Some(SilQuotationReference(q, _)), _
        )) => {
          annotator.quotationRef(q, BRACKET_NONE)
        }
        case Some(ap : SilActionPredicate) => {
          recurse(ap)
        }
        case _ => {
          ref
        }
      }
    }
    recurse(predicate)
  }

  override def equivalentReferences(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[SpcEntity],
    entity : SpcEntity,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    val references = super.equivalentReferences(
      annotator, communicationContext, entity, determiner)
    if (entity.form.name == PhlebShell.INVENTORY_WORD) {
      val (worse, better) =
        references.partition(_ match
        {
          case SilOptionallyDeterminedReference(
            SilNounReference(_), _) => true
          case SilGenitiveReference(
            _, SilNounLemmaReference("container")) => true
          case _ => false
        })
      // prefer "the player's stuff" over "the player-inventory"
      // and over "the widget's container"
      better ++ worse
    } else {
      references
    }
  }

  override def isSpatialLocation(entity : SpcEntity) : Boolean =
  {
    cosmos.isHyponym(entity.form, mapLocationForm) ||
      super.isSpatialLocation(entity)
  }

  override def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    val seq = super.resolveFormCandidates(noun)
    considerPreferredSynonym(seq, noun)
    seq
  }

  override def resolveForm(noun : SilWord) : Option[SpcForm] =
  {
    val opt = super.resolveForm(noun)
    considerPreferredSynonym(opt, noun)
    opt
  }

  override def resolveRole(
    form : SpcForm,
    noun : SilWord,
    includeHypernyms : Boolean = true) : Option[SpcRole] =
  {
    val opt = super.resolveRole(form, noun, includeHypernyms)
    considerPreferredSynonym(opt, noun)
    opt
  }

  private def considerPreferredSynonym(
    ideals : Iterable[SpcIdeal], noun : SilWord)
  {
    ideals.foreach(ideal => {
      if (!preferredSynonyms.contains(ideal)) {
        preferredSynonyms.put(ideal, noun.toNounLemma)
      }
    })
  }

  override protected def getFormName(form : SpcForm) : String =
  {
    synonymize(form, super.getFormName(form))
  }

  override protected def getPossesseeName(role : SpcRole) : String =
  {
    synonymize(role, super.getPossesseeName(role))
  }

  private def synonymize(ideal : SpcIdeal, name : String) : String =
  {
    def isHyphenized(s : String) = s.contains('-')
    if (isHyphenized(name)) {
      val synonyms = cosmos.getSynonymsForIdeal(ideal)
      synonyms.map(_.name.split(":").last).
        filterNot(isHyphenized).headOption.getOrElse(name)
    } else {
      name
    }
  }
}

