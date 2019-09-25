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
import scala.collection.JavaConverters._
import scala.io._
import scala.util._

import spire.math._

import org.jgrapht._

import SprEnglishLemmas._

class SpcMind(cosmos : SpcCosmos)
    extends SmcMind[SpcEntity, SpcProperty, SpcCosmos](cosmos)
{
  override def getCosmos = cosmos

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcMind(newCosmos)
    mind.initFrom(this)
    mind
  }

  def importBeliefs(
    resourceName : String,
    responder : SpcResponder = new SpcResponder(
      this,
      SpcBeliefParams(ACCEPT_NEW_BELIEFS)))
  {
    if (!cosmos.isDuplicateBeliefResource(resourceName)) {
      loadBeliefs(ResourceUtils.getResourceSource(resourceName), responder)
    }
  }

  def loadBeliefs(
    source : Source,
    responder : SpcResponder = new SpcResponder(
      this, SpcBeliefParams(ACCEPT_NEW_BELIEFS)))
  {
    val beliefs = source.getLines.
      filterNot(SprParser.isIgnorableLine).mkString("\n")
    val parseResults = responder.newParser(beliefs).parseAll
    val ok = responder.sentencePrinter.sb.respondCompliance
    parseResults.foreach(parseResult => {
      val annotator = SpcAnnotator(parseResult.annotator)
      val inputRewriter = new SmcInputRewriter(this, annotator)
      val analyzed = inputRewriter.normalizeInput(
        analyzeSense(annotator, parseResult.sentence))
      val accepter = SpcBeliefAccepter(
        responder,
        SpcBeliefParams(),
        SpcResultCollector(annotator))
      accepter.recognizeBeliefs(analyzed) match {
        case Seq(ib : IndirectBelief) => {
          accepter.applyBelief(ib)
        }
        case _ => {
          val output = responder.process(
            SprParseResult(analyzed, annotator))
          assert(output == ok, tupleN((parseResult.sentence, output)))
        }
      }
    })
    cosmos.validateBeliefs
  }

  override def equivalentReferences(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[SpcEntity],
    entity : SpcEntity,
    determiner : SilDeterminer) : Seq[SilReference] =
  {
    entity matchPartial {
      case SpcTransientEntity(form, value, inflected) => {
        if (entity.form.name == PROPERTY_TYPE_STRING.name) {
          return Seq(annotator.quotationRef(value))
        } else {
          return Seq(annotator.nounRef(SilWord(inflected, value)))
        }
      }
    }
    val assocGraph = cosmos.getEntityAssocGraph
    val edges = assocGraph.incomingEdgesOf(entity).asScala.toSeq
    val rankedGenitives = edges.flatMap(
      edge => {
        val graph = cosmos.getGraph
        val possessor = graph.getPossessorEntity(edge)
        val role = cosmos.getPossesseeRole(edge)
        val specializedRole = graph.specializeRoleForForm(
          role,
          entity.form)
        // we prefer more specific associations over less specific ones
        // e.g. "Larry's father" is better than "one of Pete's uncles"
        val cardinality = assocGraph.outgoingEdgesOf(possessor).asScala.
          count(edge2 => {
            (edge2.getRoleName == edge.getRoleName) &&
            (role == specializedRole) || cosmos.isHyponym(
              specializedRole,
              graph.getPossesseeEntity(edge2).form)
          })
        val equivs = super.equivalentReferences(
          annotator, communicationContext, possessor, determiner)
        val genitives = equivs.map(
          possessorEquiv => {
            if (cardinality > 1) {
              // "one of Pete's uncles"
              annotator.stateSpecifiedRef(
                annotator.nounRef(SilWord(LEMMA_ONE)),
                SilAdpositionalState(
                  SilAdposition.OF,
                  annotator.genitiveRef(
                    possessorEquiv,
                    annotator.nounRef(SilWord.uninflected(
                      getPossesseeName(specializedRole)),
                      COUNT_PLURAL))))
            } else {
              // "Larry's father"
              annotator.genitiveRef(
                possessorEquiv,
                annotator.nounRef(SilWord(getPossesseeName(specializedRole))))
            }
          }
        )
        if (SpcMeta.isMetaEntity(possessor)) {
          None
        } else {
          genitives.map(g => tupleN((g, cardinality)))
        }
      }
    )
    val qualifiedSeq = {
      if (!entity.properName.isEmpty) {
        Seq(qualifiedReference(annotator, entity, DETERMINER_NONSPECIFIC))
      } else {
        Seq.empty
      }
    }
    super.equivalentReferences(
      annotator, communicationContext, entity, determiner
    ) ++ rankedGenitives.sortBy(_._2).map(_._1) ++ qualifiedSeq
  }

  protected def getFormName(form : SpcForm) : String =
  {
    cosmos.decodeName(form.name)
  }

  protected def getPossesseeName(role : SpcRole) : String =
  {
    cosmos.decodeName(role.name)
  }

  override def thirdPersonReference(
    annotator : AnnotatorType, entities : Set[SpcEntity])
      : Option[SilReference] =
  {
    val gender = {
      if (entities.size == 1) {
        val entity = entities.head
        cosmos.evaluateEntityProperty(entity, LEMMA_GENDER) match {
          case Success((_, Some(LEMMA_FEMININE))) => GENDER_FEMININE
          case Success((_, Some(LEMMA_MASCULINE))) => GENDER_MASCULINE
          case _ => guessGender(entity)
        }
      } else {
        // FIXME:  for languages like Spanish, need to be macho
        GENDER_NEUTER
      }
    }
    val count = {
      if (entities.size == 1) {
        COUNT_SINGULAR
      } else {
        COUNT_PLURAL
      }
    }
    Some(annotator.pronounRef(PERSON_THIRD, gender, count))
  }

  protected def guessGender(entity : SpcEntity) : SilGender =
  {
    cosmos.resolveForm(SmcLemmas.LEMMA_SOMEONE) match {
      case Some(someoneForm) => {
        if (cosmos.isHyponym(entity.form, someoneForm)) {
          GENDER_SOMEONE
        } else {
          GENDER_NEUTER
        }
      }
      case _ => GENDER_NEUTER
    }
  }

  def properReference(
    annotator : AnnotatorType,
    entity : SpcEntity) =
  {
    annotator.nounRef(SilWord(entity.properName))
  }

  def qualifiedReference(
    annotator : AnnotatorType,
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    val formName = getFormName(entity.form)
    val nounRef = annotator.determinedNounRef(
      SilWord(formName), determiner)
    if (entity.properName.isEmpty) {
      annotator.qualifiedRef(
        nounRef, entity.qualifiers.map(q => SilWord(q)).toSeq)
    } else {
      nounRef
    }
  }

  override def specificReference(
    annotator : AnnotatorType,
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    if (entity.properName.nonEmpty) {
      properReference(annotator, entity)
    } else {
      qualifiedReference(annotator, entity, determiner)
    }
  }

  def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    cosmos.resolveForm(cosmos.encodeName(noun)).toSeq
  }

  def resolveForm(noun : SilWord) : Option[SpcForm] =
  {
    resolveFormCandidates(noun).headOption
  }

  def resolveRole(
    form : SpcForm, noun : SilWord,
    includeHypernyms : Boolean = true) : Option[SpcRole] =
  {
    cosmos.resolveRole(form, cosmos.encodeName(noun), includeHypernyms)
  }

  override def resolvePropertyValueEntity(
    property : SpcProperty,
    value : String) : Try[SpcEntity] =
  {
    // FIXME for enums, use the correct meta entity
    val domainName = property.domain.name
    val annotator = SpcAnnotator(this)
    val analyzedNoun =
      analyzeSense(annotator, annotator.nounRef(SilWord(domainName))).noun
    val form = resolveForm(analyzedNoun).getOrElse(SpcForm(domainName))
    val inflected = cosmos.getPropertyStateMap(property).
      get(value).getOrElse(value)
    Success(
      SpcTransientEntity(
        form,
        value,
        inflected
      )
    )
  }

  def instantiateForm(noun : SilWord) : SpcForm =
  {
    resolveForm(noun).getOrElse {
      cosmos.instantiateForm(noun)
    }
  }

  def instantiateRole(
    possessor : SpcForm, noun : SilWord) : SpcRole =
  {
    cosmos.instantiateRole(possessor, noun)
  }

  override def resolveGenitive(
    possessor : SpcEntity, roleName : SilWord) : Try[Set[SpcEntity]] =
  {
    resolveRole(possessor.form, roleName) match {
      case Some(role) => {
        Success(cosmos.resolveGenitive(possessor, role))
      }
      case _ => Success(Set.empty)
    }
  }

  override def reifyRole(
    possessor : SpcEntity,
    roleName : SilWord,
    onlyIfProven : Boolean) =
  {
    resolveRole(possessor.form, roleName) match {
      case Some(role) => {
        cosmos.reifyRole(possessor, role, onlyIfProven)
      }
      // FIXME assert instead?
      case _ => Set.empty
    }
  }

  override def evaluateEntityAdpositionPredicate(
    entity : SpcEntity,
    objEntity : SpcEntity,
    adposition : SilAdposition,
    qualifiers : Set[SilWord]) : Try[Trilean] =
  {
    val roleName = adposition match {
      case SilAdposition.GENITIVE_OF => {
        if (qualifiers.size != 1) {
          return Success(Trilean.Unknown)
        } else {
          qualifiers.head
        }
      }
      case SilAdposition.IN => {
        SilWord(SmcLemmas.LEMMA_CONTAINEE)
      }
      case SilAdposition.AMONG => {
        return Success(Trilean(entity == objEntity))
      }
      case _ => {
        return Success(Trilean.Unknown)
      }
    }
    resolveRole(objEntity.form, roleName) match {
      case Some(role) => {
        Success(Trilean(cosmos.isEntityAssoc(objEntity, entity, role)))
      }
      case _ => {
        Success(Trilean.Unknown)
      }
    }
  }

  override def canonicalGender(gender : SilGender) : SilGender =
  {
    def lookupGender(lemma : String) =
    {
      cosmos.resolveForm(lemma).map(
        SpcGender(_, gender.maybeBasic)
      ).getOrElse(gender)
    }

    gender match {
      case GENDER_MASCULINE => lookupGender(LEMMA_MASCULINE)
      case GENDER_FEMININE => lookupGender(LEMMA_FEMININE)
      case GENDER_NEUTER => lookupGender(LEMMA_NEUTER)
      case _ => super.canonicalGender(gender)
    }
  }

  override def evaluateEntityCategoryPredicate(
    entity : SpcEntity,
    noun : SilWord,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    val categoryName = cosmos.encodeName(noun)
    val (formSeq, roleOpt) = cosmos.resolveIdeal(
      categoryName
    ) match {
      case (None, None) => {
        resolveFormCandidates(noun) match {
          case Seq() => {
            val graph = cosmos.getGraph
            val roles = cosmos.getFormHypernyms(entity.form).flatMap(
              hypernym => {
                val immediateRoles = Graphs.predecessorListOf(
                  graph.idealTaxonomy,
                  hypernym).asScala.filter(_.isRole)
                immediateRoles.flatMap(role =>
                  graph.getIdealHyponyms(role).map(_.asInstanceOf[SpcRole]))
              }
            )
            roles.find(role =>
              cosmos.getSynonymsForIdeal(role).contains(
                cosmos.synthesizeRoleSynonym(role.possessor, categoryName)
              )
            ).map(role =>
              tupleN((Seq.empty, Some(role)))
            ).getOrElse {
              tupleN((Seq.empty, None))
            }
          }
          case seq => tupleN((seq, None))
        }
      }
      case (Some(form), roleOpt) => {
        tupleN((Seq(form), roleOpt))
      }
      case (None, roleOpt) => {
        tupleN((Seq.empty, roleOpt))
      }
    }
    roleOpt match {
      case Some(role) => {
        Success(Trilean(
          cosmos.isFormCompatibleWithRole(entity.form, role) &&
            cosmos.getEntityAssocGraph.incomingEdgesOf(entity).asScala.
            exists(edge => {
              cosmos.isHyponym(
                role,
                cosmos.getPossesseeRole(edge))
            })
        ))
      }
      case _ => {
        formSeq match {
          case Seq() => {
            cosmos.fail(s"unknown ideal ${cosmos.encodeName(noun)}")
          }
          case _ => {
            if (formSeq.exists(form => cosmos.isHyponym(entity.form, form))) {
              Success(Trilean.True)
            } else {
              if (entity.form.isTentative) {
                Success(Trilean.Unknown)
              } else {
                Success(Trilean.False)
              }
            }
          }
        }
      }
    }
  }
}
