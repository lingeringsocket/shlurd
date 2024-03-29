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
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.jgrapht._

import spire.math._

import scala.util._
import scala.collection._
import scala.jdk.CollectionConverters._

import java.util.concurrent.atomic._

import org.jgrapht._

import SprPennTreebankLabels._

trait SpcContainmentVertex
{
}

class SpcPropertyState(val lemma : String, val inflected : String)
    extends SpcContainmentVertex
{
  override def toString = s"SpcPropertyState($lemma -> $inflected)"
}

sealed trait SpcPropertyDomain extends SmcNamedObject
case object PROPERTY_OPEN_ENUM extends SpcPropertyDomain
{
  override def name = "spc-open-enum"
}
case object PROPERTY_CLOSED_ENUM extends SpcPropertyDomain
{
  override def name = "spc-closed-enum"
}
case object PROPERTY_TYPE_STRING extends SpcPropertyDomain
{
  override def name = "spc-string"
}
object SpcPropertyDomain
{
  val all = Seq(
    PROPERTY_OPEN_ENUM,
    PROPERTY_CLOSED_ENUM,
    PROPERTY_TYPE_STRING)

  def apply(name : String) : Option[SpcPropertyDomain] =
  {
    all.find(_.name == name)
  }
}

case class SpcProperty(
  form : SpcForm, name : String, domain : SpcPropertyDomain)
    extends SmcProperty with SmcNamedObject with SpcContainmentVertex
{
  def isSynthetic = name.contains('_')

  override def toString = s"SpcProperty(${form.name}, $name, $domain)"
}

class SpcEntityPropertyState(val propertyName : String, val lemma : String)
    extends SpcContainmentVertex
{
  override def toString =
    s"SpcEntityPropertyState($propertyName -> $lemma)"
}

sealed trait SpcNym
    extends SmcNamedObject
{
  def isIdeal : Boolean = false

  def isRole : Boolean = false

  def isForm : Boolean = false

  def isSynonym : Boolean = false
}

case class SpcIdealSynonym(name : String)
    extends SpcNym
{
  override def isSynonym = true
}

sealed abstract class SpcIdeal(name : String)
    extends SpcNym
{
  override def isIdeal : Boolean = true
}

object SpcForm
{
  val TENTATIVE_INFIX = "__"

  val POSSESSEE_INFIX = "--"

  val TENTATIVE_SUFFIX = s"${TENTATIVE_INFIX}form"

  def tentativeName(word : SilWord) =
  {
    SilWord(word.decomposed.map(_.lemma).mkString("_") + TENTATIVE_SUFFIX)
  }
}

case class SpcForm(name : String)
    extends SpcIdeal(name) with SpcContainmentVertex
{
  import SpcForm._

  def isTentative = name.endsWith(TENTATIVE_SUFFIX)

  override def isForm = true

  override def toString = s"SpcForm($name)"
}

case class SpcRole(possessor : SpcForm, name : String)
    extends SpcIdeal(name)
{
  override def isRole = true

  override def toString = s"SpcRole(${possessor.name}:$name)"
}

trait SpcEntityVertex extends SmcNamedObject
{
}

trait SpcSentential
{
  def getAssertion : SilSentence

  def getAdditional : Seq[SilSentence]

  def getAlternative : Option[SilSentence]

  def getPlaceholderMap : SpcRefMap

  def toSentence : SilSentence =
  {
    if (getAlternative.nonEmpty || getAdditional.nonEmpty) {
      SilConjunctiveSentence(
        DETERMINER_ABSENT,
        Seq(getAssertion) ++ getAdditional ++ getAlternative.toSeq,
        SEPARATOR_SEMICOLON)
    } else {
      getAssertion
    }
  }
}

case class SpcAssertion(
  sentence : SilSentence,
  additionalConsequents : Seq[SilPredicateSentence],
  alternative : Option[SilPredicateSentence],
  placeholderMap : SpcRefMap
) extends SpcSentential
{
  override def getAssertion = sentence

  override def getAdditional = additionalConsequents

  override def getAlternative = alternative

  override def getPlaceholderMap = placeholderMap

  def asTrigger : Option[SpcTrigger] =
  {
    sentence match {
      case cs : SilConditionalSentence => {
        Some(SpcTrigger(
          cs, additionalConsequents, alternative, placeholderMap))
      }
      case _ => None
    }
  }
}

case class SpcTrigger(
  conditionalSentence : SilConditionalSentence,
  additionalConsequents : Seq[SilPredicateSentence],
  alternative : Option[SilPredicateSentence],
  placeholderMap : SpcRefMap
) extends SpcSentential
{
  override def getAssertion = conditionalSentence

  override def getAdditional = additionalConsequents

  override def getAlternative = alternative

  override def getPlaceholderMap = placeholderMap
}

trait SpcEntity extends SmcEntity with SpcEntityVertex with SpcContainmentVertex
{
  def name : String

  def form : SpcForm

  def qualifiers : Set[String]

  def properName : String
}

case class SpcPersistentEntity(
  name : String,
  form : SpcForm,
  qualifiers : Set[String],
  properName : String = "")
    extends SpcEntity
{
  override def isTentative =
    properName.contains(SpcForm.TENTATIVE_INFIX) && !SpcMeta.isMetaEntity(this)

  override def getUniqueIdentifier = name
}

case class SpcTransientEntity(
  form : SpcForm,
  value : String,
  inflected : String
) extends SpcEntity
{
  override def name = getUniqueIdentifier

  override def qualifiers = Set(value)

  override def properName = value

  override def isTentative = false

  override def getUniqueIdentifier = form.name + ":" + value
}

case class SpcEntitySynonym(val name : String)
    extends SpcEntityVertex
{
}

case class SpcGender(
  form : SpcForm,
  basic : Option[SilBasicGender]) extends SilGender
{
  override def maybeBasic = basic
}

class SpcCosmicPool
{
  val idGenerator = new AtomicLong

  @transient private var bulkLoad = false

  @transient var taxonomyTimestamp = 1

  @transient var entityTimestamp = 1

  @transient val hypernymCache =
    new mutable.HashMap[SpcIdeal, (Int, Seq[SpcForm])]

  @transient val hyponymCache =
    new mutable.HashMap[SpcIdeal, (Int, Seq[SpcForm])]

  @transient val roleCache =
    new mutable.HashMap[(SpcForm, SilWord, Boolean), (Int, Option[SpcRole])]

  @transient val roleCompatibilityCache =
    new mutable.HashMap[(SpcRole, SpcForm), (Int, Boolean)]

  @transient val nounCache =
    new mutable.HashMap[
      (String, SilReferenceContext, Set[String]),
      (Int, Try[Set[SpcEntity]])
    ]

  @transient val genderCache =
    new mutable.HashMap[SpcEntity, (Int, SilGender)]

  @transient val pronounCache =
    new mutable.HashMap[SpcEntity, (Int, SilPronounMap)]

  def invalidateCache() : Unit =
  {
    taxonomyTimestamp += 1
    entityTimestamp += 1
  }

  def accessCache[K, V](
    cache : mutable.Map[K, (Int, V)],
    key : K,
    newTimestamp : Int,
    op : => V) =
  {
    val oldValueOpt = cache.get(key).flatMap {
      case (oldTimestamp, value) => {
        if (oldTimestamp == newTimestamp) {
          Some(value)
        } else {
          None
        }
      }
    }
    oldValueOpt.getOrElse {
      val newValue = op
      cache.put(key, tupleN(newTimestamp, newValue))
      newValue
    }
  }

  def isBulkLoad : Boolean =
  {
    bulkLoad
  }

  def enableBulkLoad() : Unit =
  {
    bulkLoad = true
  }
}

class SpcCosmos(
  graph : SpcGraph = SpcGraph(),
  forkLevel : Int = 0,
  pool : SpcCosmicPool = new SpcCosmicPool
) extends SmcCosmos[SpcEntity, SpcProperty] with DeltaModification
{
  @transient private lazy val unmodifiableGraph = graph.asUnmodifiable

  // FIXME
  @transient private var wordLabeler : SprWordnetLabeler = null

  // FIXME we use Map instead of Set due to kryo limitation
  private val importedBeliefResources =
    new mutable.LinkedHashMap[String, Boolean]

  private var parent : Option[SpcCosmos] = None

  private[platonic] val meta = new SpcMeta(this)

  def getForms = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isForm).map(_.asInstanceOf[SpcForm])

  def getRoles = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isRole).map(_.asInstanceOf[SpcRole])

  def getEntities =
    graph.entitySynonyms.vertexSet.asScala.toSeq.
      filter(_.isInstanceOf[SpcEntity]).map(_.asInstanceOf[SpcEntity])

  def getAssertions =
    graph.assertions.vertexSet.asScala.toSeq

  def getTriggers =
    graph.assertions.vertexSet.asScala.toSeq.flatMap(_.asTrigger)

  def getGraph = unmodifiableGraph

  protected[platonic] def getModifiableGraph = graph

  def getPool = pool

  private[platonic] def getIdGenerator = pool.idGenerator

  private def generateId = getIdGenerator.getAndIncrement

  protected[platonic] def annotateFormAssoc(
    edge : SpcFormAssocEdge, constraint : SpcCardinalityConstraint) : Unit =
  {
    edge.constraint = constraint
  }

  def fork(detached : Boolean = false) : SpcCosmos =
  {
    // we don't currently support true nested forks
    val forkedGraph = {
      if (forkLevel > 0) {
        assert(!detached)
        graph
      } else {
        SpcGraph.fork(graph)
      }
    }
    val (forkPool, newForkLevel) = {
      if (detached) {
        val newPool = new SpcCosmicPool
        newPool.idGenerator.set(pool.idGenerator.get)
        tupleN(newPool, 0)
      } else {
        tupleN(pool, forkLevel + 1)
      }
    }
    val forked = new SpcCosmos(forkedGraph, newForkLevel, forkPool)
    forked.meta.afterFork(meta)
    forked.inheritBeliefResources(this)
    if (!detached) {
      forked.parent = Some(this)
    }
    forked
  }

  def asUnmodifiable : SpcCosmos =
  {
    val frozen = new SpcCosmos(unmodifiableGraph, forkLevel, pool)
    frozen.meta.afterFork(meta)
    frozen.inheritBeliefResources(this)
    frozen.parent = Some(this)
    frozen
  }

  def copyFrom(src : SpcCosmos) : Unit =
  {
    assert(getIdGenerator.get == 0)
    val dstGraphs = graph.getGraphs
    dstGraphs.foreach(graph => assert(graph.vertexSet.isEmpty))
    syncGenerator(src)
    dstGraphs.zip(src.getGraph.getGraphs).foreach({
      case (dstGraph, srcGraph) => {
        // FIXME find a way to do this without ugly casts
        val dstGraphUp = dstGraph.asInstanceOf[Graph[Any, Any]]
        val srcGraphUp = srcGraph.asInstanceOf[Graph[Any, Any]]
        Graphs.addGraph(dstGraphUp, srcGraphUp)
      }
    })
    this.inheritBeliefResources(src)
    meta.enable()
  }

  def newClone(flattenDeltas : Boolean = false) : SpcCosmos =
  {
    val newCosmos = new SpcCosmos(graph.newClone(flattenDeltas))
    newCosmos.syncGenerator(this)
    newCosmos.meta.afterFork(meta)
    newCosmos.inheritBeliefResources(this)
    newCosmos
  }

  def getWordLabeler(tongue : SprTongue) : SprWordnetLabeler =
  {
    // FIXME what if a different wordnet is used?
    if (Option(wordLabeler).isEmpty) {
      val newLabeler = new SprWordnetLabeler(tongue)
      getAssertions.flatMap(assertion =>
        SpcBeliefRecognizer.recognizeWordRule(tongue, assertion.sentence)
      ).foreach(rule => {
        newLabeler.addRule(rule)
      })
      wordLabeler = newLabeler
    }
    wordLabeler
  }

  private[platonic] def clearWordLabeler() : Unit =
  {
    wordLabeler = null
  }

  private def syncGenerator(src : SpcCosmos) : Unit =
  {
    getIdGenerator.set(src.getIdGenerator.get)
  }

  private def inheritBeliefResources(src : SpcCosmos) : Unit =
  {
    importedBeliefResources ++= src.importedBeliefResources
    Option(src.wordLabeler).foreach(labeler => {
      wordLabeler = new SprWordnetLabeler(
        labeler.tongue, labeler.maxPrefix, labeler.rules.clone)
    })
  }

  def isDuplicateBeliefResource(resourceName : String) : Boolean =
  {
    if (importedBeliefResources.contains(resourceName)) {
      true
    } else {
      importedBeliefResources.put(resourceName, true)
      false
    }
  }

  def isBulkLoad : Boolean =
  {
    pool.isBulkLoad
  }

  private[platonic] def synthesizeRoleSynonym(
    possessor : SpcForm,
    name : String) : SpcIdealSynonym =
  {
    SpcIdealSynonym(encodeName(possessor.name + ":" + name))
  }

  private def synthesizeSynonym(ideal : SpcIdeal) : SpcIdealSynonym =
  {
    ideal match {
      case form : SpcForm => {
        SpcIdealSynonym(encodeName(form.name))
      }
      case role : SpcRole => {
        synthesizeRoleSynonym(role.possessor, role.name)
      }
    }
  }

  def synthesizeEntitySynonym(name : String) : SpcEntitySynonym =
  {
    SpcEntitySynonym(encodeName(name.toLowerCase))
  }

  private def registerIdeal(ideal : SpcIdeal) =
  {
    // FIXME we should validate the form name to make sure it doesn't
    // intrude on system conventions, e.g. spc- prefix.  Likewise
    // for entity names, role names, etc.
    val synonym = synthesizeSynonym(ideal)
    assert(!graph.idealSynonyms.containsVertex(synonym), synonym)
    graph.idealSynonyms.addVertex(synonym)
    graph.idealSynonyms.addVertex(ideal)
    addIdealSynonymEdge(synonym, ideal)
    graph.idealTaxonomy.addVertex(ideal)
    graph.formAssocs.addVertex(ideal)
    getIdealBySynonym(SpcMeta.ENTITY_METAFORM_NAME) matchPartial {
      case Some(entityForm) => {
        addIdealTaxonomy(ideal, entityForm)
      }
    }
    ideal
  }

  private def addIdealSynonymEdge(
    synonym : SpcIdealSynonym, ideal : SpcNym) : Unit =
  {
    graph.idealSynonyms.addEdge(synonym, ideal, SpcSynonymEdge(generateId))
  }

  private def addEntitySynonymEdge(
    synonym : SpcEntitySynonym, entity : SpcEntity) : Unit =
  {
    graph.entitySynonyms.addEdge(synonym, entity, SpcSynonymEdge(generateId))
  }

  private def forgetIdeal(ideal : SpcIdeal) : Unit =
  {
    assert(graph.idealSynonyms.degreeOf(ideal) == 0)
    assert(graph.idealTaxonomy.inDegreeOf(ideal) == 0)
    assert(graph.formAssocs.degreeOf(ideal) == 0)
    graph.idealSynonyms.removeVertex(ideal)
    graph.idealTaxonomy.removeVertex(ideal)
    graph.formAssocs.removeVertex(ideal)
  }

  private def registerForm(form : SpcForm) =
  {
    meta.formExistence(form, true)
    registerIdeal(form)
    graph.components.addVertex(form)
    form
  }

  private def registerRole(role : SpcRole) =
  {
    meta.roleExistence(role)
    registerIdeal(role)
    role
  }

  def getSynonymsForIdeal(ideal : SpcIdeal) : Seq[SpcIdealSynonym] =
  {
    Graphs.predecessorListOf(
      getGraph.idealSynonyms,
      ideal).asScala.map(_.asInstanceOf[SpcIdealSynonym])
  }

  private[platonic] def getIdealBySynonym(name : String) : Option[SpcIdeal] =
  {
    getIdealBySynonym(SpcIdealSynonym(encodeName(name)))
  }

  private[platonic] def getIdealBySynonym(
    synonym : SpcIdealSynonym) : Option[SpcIdeal] =
  {
    if (graph.idealSynonyms.containsVertex(synonym)) {
      Some(graph.getIdealBySynonym(synonym))
    } else {
      None
    }
  }

  def getEntityBySynonym(name : String)
      : Option[SpcEntity] =
  {
    val synonym = synthesizeEntitySynonym(name)
    getEntitiesBySynonym(synonym).find(
      e => (synthesizeEntitySynonym(e.name) == synonym))
  }

  protected[platonic] def getEntitiesBySynonym(synonym : SpcEntitySynonym)
      : Seq[SpcEntity] =
  {
    if (graph.entitySynonyms.containsVertex(synonym)) {
      Graphs.successorListOf(
        graph.entitySynonyms, synonym
      ).asScala.toSeq.map(_.asInstanceOf[SpcEntity])
    } else {
      Seq.empty
    }
  }

  def getFormHypernyms(
    form : SpcForm) : Seq[SpcForm] =
  {
    pool.accessCache(
      pool.hypernymCache,
      form,
      pool.taxonomyTimestamp,
      graph.getFormHypernyms(form).toSeq
    )
  }

  def getFormHyponyms(form : SpcForm) : Seq[SpcForm] =
  {
    pool.accessCache(
      pool.hyponymCache,
      form,
      pool.taxonomyTimestamp,
      graph.getFormHyponyms(form).toSeq
    )
  }

  def isHyponym(
    hyponymIdeal : SpcIdeal,
    hypernymIdealOpt : Option[SpcIdeal]) : Boolean =
  {
    graph.isHyponym(hyponymIdeal, hypernymIdealOpt)
  }

  def isHyponym(
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal) : Boolean =
  {
    graph.isHyponym(hyponymIdeal, hypernymIdeal)
  }

  def getFormHyponymRealizations(form : SpcForm) : Seq[SpcEntity] =
  {
    if (meta.isFresh) {
      getFormHyponyms(form).flatMap(getFormRealizations)
    } else {
      getEntities.filter(entity => isHyponym(entity.form, form))
    }
  }

  def getFormHypernymRealizations(form : SpcForm) : Seq[SpcEntity] =
  {
    if (meta.isFresh) {
      getFormHypernyms(form).flatMap(getFormRealizations)
    } else {
      getEntities.filter(entity => isHyponym(form, entity.form))
    }
  }

  def getFormRealizations(form : SpcForm) : Seq[SpcEntity] =
  {
    val formEntityName = SpcMeta.formMetaEntityName(form)
    val formEntity = getEntityBySynonym(formEntityName).get
    graph.entityAssocs.outgoingEdgesOf(formEntity).asScala.toSeq.filter(
      _.getRoleName == SpcMeta.REALIZATION_METAROLE_NAME).map(
      graph.getPossesseeEntity)
  }

  def resolveIdealSynonym(name : String) : String =
  {
    getIdealBySynonym(encodeName(name)) match {
      case Some(ideal) => ideal.name
      case _ => name
    }
  }

  def resolveForm(lemma : String) = resolveIdeal(lemma)._1

  def resolveRole(
    form : SpcForm,
    lemma : String,
    includeHypernyms : Boolean = true) : Option[SpcRole] =
  {
    val hypernyms = {
      if (includeHypernyms) {
        try {
          getFormHypernyms(form)
        } catch {
          case ex : Throwable => {
            // SpcGraphVisualizer.displayEntities(getGraph)
            throw ex
          }
        }
      } else {
        Seq(form)
      }
    }
    val name = hypernyms.flatMap(hypernym => {
      getIdealBySynonym(
        synthesizeRoleSynonym(hypernym, lemma)).map(_.name)
    }).find(_ => true).getOrElse {
      getIdealBySynonym(SpcIdealSynonym(encodeName(lemma))).
        map(_.name).getOrElse(lemma)
    }
    hypernyms.foreach(hypernym => {
      graph.formAssocs.outgoingEdgesOf(hypernym).asScala.foreach(edge => {
        val role = graph.getPossesseeRole(edge)
        graph.getIdealHyponyms(role).foreach(hyponym => {
          if (hyponym.name == name) {
            return Some(hyponym.asInstanceOf[SpcRole])
          }
        })
      })
    })
    None
  }

  def getPossesseeRole(edge : SpcEntityAssocEdge) : SpcRole =
  {
    resolveRole(graph.getPossessorEntity(edge).form, edge.getRoleName).get
  }

  def instantiateForm(word : SilWord) =
  {
    val name = encodeName(word)
    val ideal = getIdealBySynonym(name).getOrElse(
      registerForm(new SpcForm(name)))
    assert(ideal.isForm, ideal)
    ideal.asInstanceOf[SpcForm]
  }

  def instantiateRole(possessor : SpcForm, word : SilWord) =
  {
    val name = encodeName(word)
    val ideal = getIdealBySynonym(
      synthesizeRoleSynonym(possessor, name)
    ).getOrElse(
      registerRole(new SpcRole(possessor, name))
    )
    assert(ideal.isRole, ideal)
    ideal.asInstanceOf[SpcRole]
  }

  private[platonic] def forgetForm(form : SpcForm) : Unit =
  {
    assert(getFormHyponymRealizations(form).isEmpty)
    assert(graph.idealTaxonomy.incomingEdgesOf(form).isEmpty)
    graph.idealTaxonomy.outgoingEdgesOf(form).asScala.foreach(edge => {
      val superclass = graph.getSuperclassIdeal(edge)
      meta.idealSuperclass(form, superclass, false)
    })
    meta.formExistence(form, false)
    val synonymEdges = graph.idealSynonyms.incomingEdgesOf(form).asScala.toSeq
    synonymEdges.foreach(edge => graph.idealSynonyms.removeVertex(
      graph.idealSynonyms.getEdgeSource(edge)))
    graph.removeContainer(form)
    forgetIdeal(form)
  }

  private[platonic] def forgetEntity(entity : SpcEntity) : Unit =
  {
    pool.entityTimestamp += 1
    meta.entityExistence(entity, false)
    assert(graph.entityAssocs.degreeOf(entity) == 0)
    val entitySynonyms = graph.entitySynonyms
    if (getEntityBySynonym(entity.name) == Some(entity)) {
      val synonymEdges = entitySynonyms.
        incomingEdgesOf(entity).asScala.toSeq
      synonymEdges.foreach(edge => {
        val synonym = entitySynonyms.getEdgeSource(edge)
        entitySynonyms.removeEdge(edge)
        if (entitySynonyms.outDegreeOf(synonym) == 0) {
          entitySynonyms.removeVertex(synonym)
        }
      })
    }
    assert(entitySynonyms.degreeOf(entity) == 0)
    entitySynonyms.removeVertex(entity)
    graph.entityAssocs.removeVertex(entity)
    graph.removeContainer(entity)
  }

  def matchAssocs(oldForm : SpcForm, newForm : SpcForm)
      : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)]=
  {
    val formAssocs = graph.formAssocs
    formAssocs.outgoingEdgesOf(oldForm).asScala.toSeq.flatMap(
      oldEdge => {
        assert(!graph.inverseAssocs.containsVertex(oldEdge))
        graph.getFormAssocEdge(newForm, graph.getPossesseeRole(oldEdge)).map(
          newEdge => (oldEdge, newEdge))
      }
    )
  }

  private[platonic] def isValidMergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge) : Boolean =
  {
    val entityAssocs = graph.entityAssocs
    val constraint = newEdge.constraint
    if (constraint.upper < Int.MaxValue) {
      val oldEntityEdges =
        entityAssocs.edgeSet.asScala.filter(
          _.getRoleName == oldEdge.getRoleName)
      oldEntityEdges.groupBy(graph.getPossessorEntity).
        filter(_._2.size > constraint.upper).isEmpty
    } else {
      true
    }
  }

  private[platonic] def mergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge) : Unit =
  {
    assert(!graph.inverseAssocs.containsVertex(oldEdge))
    // FIXME we should be able to support this
    assert(!graph.inverseAssocs.containsVertex(newEdge))
    val formAssocs = graph.formAssocs
    val entityAssocs = graph.entityAssocs
    val oldEntityEdges =
      entityAssocs.edgeSet.asScala.filter(
        _.getRoleName == oldEdge.getRoleName)
    oldEntityEdges.foreach(
      entityEdge => {
        entityAssocs.addEdge(
          graph.getPossessorEntity(entityEdge),
          graph.getPossesseeEntity(entityEdge),
          SpcEntityAssocEdge(generateId, newEdge.getRoleName))
        entityAssocs.removeEdge(entityEdge)
      }
    )
    formAssocs.removeEdge(oldEdge)
  }

  private[platonic] def replaceForm(
    oldForm : SpcForm, newForm : SpcForm) : Unit =
  {
    pool.taxonomyTimestamp += 1
    assert(oldForm.isTentative)
    assert(graph.components.degreeOf(oldForm) == 0)
    graph.replaceVertex(graph.formAssocs, oldForm, newForm)
    forgetForm(oldForm)
  }

  private[platonic] def replaceEntity(
    oldEntity : SpcEntity, newEntity : SpcEntity) : Unit =
  {
    // FIXME verify that entities are role-compatible across all
    // relevant form associations, etc
    if (oldEntity != newEntity) {
      // this happens again later, but we have to do it now
      // to make sure that newEntity doesn't end up with
      // multiple types
      meta.entityExistence(oldEntity, false)
      graph.replaceVertex(graph.entityAssocs, oldEntity, newEntity)
      graph.replaceVertex(graph.components, oldEntity, newEntity)
      forgetEntity(oldEntity)
      assert {
        val outgoingAssocs =
          graph.getOutgoingEntityAssocEdges(newEntity).
            map(_.getRoleName).distinct
        outgoingAssocs.forall(roleName => {
          sanityCheckConstraint(newEntity, roleName)
        })
      }
      assert {
        val incomingEdges = graph.getIncomingEntityAssocEdges(newEntity)
        incomingEdges.forall(entityEdge => {
          sanityCheckConstraint(
            graph.getPossessorEntity(entityEdge),
            entityEdge.getRoleName)
        })
      }
      // FIXME we currently leave garbage lying around
      /*
      if (oldEntity.form.isTentative) {
        forgetForm(oldEntity.form)
      }
       */
    }
  }

  def addIdealSynonym(synonymName : String, fundamentalName : String) : Unit =
  {
    val ideal = getIdealBySynonym(fundamentalName).get
    addIdealSynonym(synonymName, ideal)
  }

  def addIdealSynonym(synonymName : String, ideal : SpcIdeal) : Unit =
  {
    val synonym = SpcIdealSynonym(encodeName(synonymName))
    addIdealSynonym(synonym, ideal)
  }

  def addIdealSynonym(synonym : SpcIdealSynonym, ideal : SpcIdeal) : Unit =
  {
    assert(!graph.idealSynonyms.containsVertex(synonym), synonym)
    graph.idealSynonyms.addVertex(synonym)
    addIdealSynonymEdge(synonym, ideal)
  }

  protected[platonic] def getIdealSynonyms =
    graph.idealSynonyms.edgeSet.asScala.toSeq.map(edge =>
      tupleN(graph.idealSynonyms.getEdgeSource(edge).name,
        graph.idealSynonyms.getEdgeTarget(edge).name))

  def getIdealTaxonomyGraph =
    unmodifiableGraph.idealTaxonomy

  def getFormAssocGraph =
    unmodifiableGraph.formAssocs

  def getEntityAssocGraph =
    unmodifiableGraph.entityAssocs

  def getInverseAssocEdges : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)] =
    graph.inverseAssocs.vertexSet.asScala.toSeq.map(vertex =>
      tupleN(vertex, getInverseAssocEdge(vertex).get))

  def getRolesForForm(
    form : SpcForm) : Iterable[SpcRole] =
  {
    graph.getIdealHyponyms(form).toSeq.filter(_.isRole).
      map(_.asInstanceOf[SpcRole]).filter(
        role => isFormCompatibleWithRole(form, role))
  }

  def isFormCompatibleWithIdeal(
    form : SpcForm, possessorIdeal : SpcIdeal) : Boolean =
  {
    possessorIdeal match {
      case possessorForm : SpcForm => {
        isHyponym(form, possessorForm)
      }
      case role : SpcRole => {
        isFormCompatibleWithRole(form, role)
      }
    }
  }

  def isFormCompatibleWithRole(form : SpcForm, role : SpcRole) : Boolean =
  {
    pool.accessCache(
      pool.roleCompatibilityCache,
      tupleN(role, form),
      pool.taxonomyTimestamp,
      graph.isFormCompatibleWithRole(form, role)
    )
  }

  private def hasQualifiers(
    existing : SpcEntity,
    form : SpcForm,
    qualifiers : Set[String],
    overlap : Boolean) : Boolean =
  {
    if (overlap) {
      (form == existing.form) &&
        (qualifiers.subsetOf(existing.qualifiers) ||
          existing.qualifiers.subsetOf(qualifiers))
    } else {
      isHyponym(existing.form, form) &&
        qualifiers.subsetOf(existing.qualifiers)
    }
  }

  def instantiateEntity(
    form : SpcForm,
    qualifierString : Seq[SilWord],
    properName : String = "") : (SpcEntity, Boolean) =
  {
    val qualifiers = qualifierSet(qualifierString)
    if (properName.isEmpty) {
      getFormHypernymRealizations(form).find(hasQualifiers(
        _, form, qualifiers, true)
      ).foreach(entity => {
        return tupleN(entity, false)
      })
    } else {
      getEntityBySynonym(properName).foreach(entity => {
        return tupleN(entity, false)
      })
    }
    val formId = generateId.toString
    val name = {
      if (properName.isEmpty) {
        "E_" ++ (qualifierString.flatMap(_.decomposed).map(_.lemma) ++
          Seq(form.name, formId)).mkString("_")
      } else {
        // FIXME should enforce that properName is capitalized,
        // or else handle uncapitalized names properly
        encodeName(properName)
      }
    }
    val entity = SpcPersistentEntity(name, form, qualifiers, properName)
    createOrReplaceEntity(entity)
    tupleN(entity, true)
  }

  private def addPartialEntitySynonyms(entity : SpcEntity) : Unit =
  {
    val components = entity.properName.split(" ")
    if (components.size > 1) {
      val anyUpper = components.exists(_.head.isUpper)
      components.foreach(component => {
        // FIXME language-dependent stopword filtering and name/title
        // handling like "Conan the Barbarian", "Ponce de Leon"
        val ignore = (component.size == 1) ||
          (component.head.isLower && anyUpper)
        if (!ignore) {
          val synonym = SpcEntitySynonym(component.toLowerCase)
          graph.entitySynonyms.addVertex(synonym)
          addEntitySynonymEdge(synonym, entity)
        }
      })
    }
  }

  def createOrReplaceEntity(entity : SpcEntity) : Unit =
  {
    pool.entityTimestamp += 1
    graph.entityAssocs.addVertex(entity)
    graph.entitySynonyms.addVertex(entity)
    graph.components.addVertex(entity)
    val synonym = SpcEntitySynonym(entity.name.toLowerCase)
    getEntityBySynonym(entity.name) match {
      case Some(old) => {
        assert(old != entity)
        graph.entitySynonyms.removeEdge(synonym, old)
        addEntitySynonymEdge(synonym, entity)
        replaceEntity(old, entity)
      }
      case _ => {
        graph.entitySynonyms.addVertex(synonym)
        addEntitySynonymEdge(synonym, entity)
      }
    }
    addPartialEntitySynonyms(entity)
    meta.entityExistence(entity, true)
  }

  def getInverseAssocEdge(edge : SpcFormAssocEdge)
      : Option[SpcFormAssocEdge] =
  {
    graph.getInverseAssocEdge(edge)
  }

  protected[platonic] def connectInverseAssocEdges(
    edge1 : SpcFormAssocEdge,
    edge2 : SpcFormAssocEdge) : Unit =
  {
    val inverseAssocs = graph.inverseAssocs
    getInverseAssocEdge(edge1).foreach(existing => {
      if (existing.getRoleName == edge2.getRoleName) {
        return
      }
    })
    assert(!inverseAssocs.containsVertex(edge1), edge1)
    assert(!inverseAssocs.containsVertex(edge2), edge2)
    inverseAssocs.addVertex(edge1)
    inverseAssocs.addVertex(edge2)
    inverseAssocs.addEdge(edge1, edge2, SpcInverseAssocEdge(generateId))
  }

  def addIdealTaxonomy(
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal) : Unit =
  {
    if (!isHyponym(hyponymIdeal, hypernymIdeal)) {
      pool.taxonomyTimestamp += 1
      val idealTaxonomy = graph.idealTaxonomy
      val newHypernyms = graph.getIdealHypernyms(hypernymIdeal)
      val redundantEdges = idealTaxonomy.outgoingEdgesOf(hyponymIdeal).
        asScala.filter(
          edge => newHypernyms.contains(graph.getSuperclassIdeal(edge)))
      val newEdge = SpcTaxonomyEdge(generateId)
      val added = idealTaxonomy.addEdge(hyponymIdeal, hypernymIdeal, newEdge)
      // since we already checked for an existing relationship and there
      // was none, the new edge should not have been redundant
      assert(added)
      // defer this until after addEdge since addEdge may throw a
      // cycle exception
      redundantEdges.foreach(edge => meta.idealSuperclass(
        graph.getSubclassIdeal(edge), graph.getSuperclassIdeal(edge), false
      ))
      idealTaxonomy.removeAllEdges(redundantEdges.asJava)
      meta.idealSuperclass(hyponymIdeal, hypernymIdeal, true)
    }
  }

  def addFormAssoc(
    possessor : SpcForm,
    role : SpcRole) : SpcFormAssocEdge =
  {
    graph.getFormAssocEdge(possessor, role) match {
      case Some(edge) => edge
      case _ => {
        val edge = SpcFormAssocEdge(possessor, role.name)
        graph.formAssocs.addEdge(possessor, role, edge)
        edge
      }
    }
  }

  def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : SpcEntityAssocEdge =
  {
    assert(isFormCompatibleWithRole(possessee.form, role))
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formAssocEdge) => {
        val edge = addEntityAssocEdge(
          possessor, possessee, role)
        getInverseAssocEdge(formAssocEdge).foreach(inverseAssocEdge => {
          addEntityAssocEdge(
            possessee, possessor, inverseAssocEdge)
        })
        edge
      }
      case _ => {
        throw new IllegalArgumentException("addEntityAssoc")
      }
    }
  }

  protected[platonic] def addEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    formAssocEdge : SpcFormAssocEdge) : SpcEntityAssocEdge =
  {
    val role = graph.getPossesseeRole(formAssocEdge)
    addEntityAssocEdge(possessor, possessee, role)
  }

  protected[platonic] def addEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : SpcEntityAssocEdge =
  {
    getEntityAssocEdge(possessor, possessee, role) match {
      case Some(edge) => {
        assert(edge.getRoleName == role.name)
        edge
      }
      case _ => {
        val edge = SpcEntityAssocEdge(
          generateId, role.name)
        graph.entityAssocs.addEdge(
          possessor, possessee, edge)
        edge
      }
    }
  }

  protected[platonic] def removeEntityAssociation(
    possessor : SpcEntity,
    possessee : SpcEntity,
    formAssocEdge : SpcFormAssocEdge) : Unit =
  {
    getEntityAssocEdge(
      possessor, possessee,
      graph.getPossesseeRole(formAssocEdge)
    ).foreach(
      removeEntityAssocEdge
    )
  }

  protected[platonic] def removeEntityAssocEdge(
    edge : SpcEntityAssocEdge) : Unit =
  {
    graph.entityAssocs.removeEdge(edge)
  }

  def isEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : Boolean =
  {
    !getEntityAssocEdge(possessor, possessee, role).isEmpty &&
      isFormCompatibleWithRole(possessee.form, role)
  }

  def getEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole
  ) : Option[SpcEntityAssocEdge] =
  {
    val edges = graph.entityAssocs.getAllEdges(possessor, possessee)
    edges.asScala.find(edge => {
      val possesseeRole = getPossesseeRole(edge)
      isHyponym(possesseeRole, role) || isHyponym(role, possesseeRole)
    })
  }

  def validateBeliefs() : Unit =
  {
    if (!isBulkLoad && (getIdGenerator.get < 10000)) {
      assert(sanityCheck)
    }
  }

  private def safeSet[T](set : java.util.Set[T]) : Set[T] =
  {
    val a = set.asScala.toSeq
    Set(a:_*)
  }

  def sanityCheck : Boolean =
  {
    assert(graph.sanityCheck)
    val idealSet = safeSet(graph.idealSynonyms.vertexSet).filter(_.isIdeal).
      map(_.asInstanceOf[SpcIdeal])
    assert(idealSet == safeSet(graph.idealTaxonomy.vertexSet))
    assert(idealSet == safeSet(graph.formAssocs.vertexSet))
    val formAssocs = graph.formAssocs
    idealSet.foreach(ideal => {
      assert((formAssocs.outDegreeOf(ideal) ==
        formAssocs.outgoingEdgesOf(ideal).
        asScala.map(_.getRoleName).toSet.size),
        ideal.toString)
      assert(getIdealBySynonym(synthesizeSynonym(ideal)) == Some(ideal))
      ideal matchPartial {
        case form : SpcForm => {
          assert(graph.components.inDegreeOf(form) == 0)
          assert(
            getFormPropertyMap(form).size ==
              graph.components.outDegreeOf(form))
        }
      }
    })
    val entitySet = getEntities.toSet
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = formEdge.constraint
      if (constraint.upper < Int.MaxValue) {
        val possessorForm = graph.getPossessorForm(formEdge)
        entitySet.filter(
          e => isHyponym(e.form, possessorForm)).foreach(entity =>
          {
            sanityCheckConstraint(entity, formEdge.getRoleName)
          }
        )
      }
    })
    val formSet = getForms.toSet
    entitySet.foreach(entity => {
      assert(getEntityBySynonym(entity.name) == Some(entity))
      assert(formSet.contains(entity.form))
      val propertyMap = getEntityPropertyMap(entity)
      assert(propertyMap.keySet.size ==
        graph.components.outDegreeOf(entity))
      propertyMap.values.foreach(entityProperty => {
        val propertyOpt = findProperty(entity.form, entityProperty.propertyName)
        assert(!propertyOpt.isEmpty)
        getPropertyStateObjMap(propertyOpt.get).contains(entityProperty.lemma)
      })
    })
    assert(entitySet == safeSet(graph.entityAssocs.vertexSet))
    val componentSet = safeSet(graph.components.vertexSet)
    val propertySet = formSet.flatMap(getFormPropertyMap(_).values).toSet
    val propertyStateSet =
      propertySet.flatMap(getPropertyStateObjMap(_).values).toSet
    val entityPropertyStateSet = entitySet.flatMap(
      getEntityPropertyMap(_).values).toSet
    assert(
      (
        formSet ++ propertySet ++ propertyStateSet ++
          entitySet ++
          entityPropertyStateSet
      ) == componentSet
    )
    propertySet.foreach(property => {
      assert(getPropertyStateMap(property).keySet.size ==
        graph.components.outDegreeOf(property))
    })
    graph.entityAssocs.edgeSet.asScala.foreach(entityEdge => {
      val possesseeRole = getPossesseeRole(entityEdge)
      val formEdge = getFormAssocForRole(possesseeRole)
      val possessorForm = graph.getPossessorForm(formEdge)
      val possessorEntity = graph.getPossessorEntity(entityEdge)
      val possesseeEntity = graph.getPossesseeEntity(entityEdge)
      assert(isFormCompatibleWithIdeal(possessorEntity.form, possessorForm))
      val role = graph.getPossesseeRole(formEdge)
      assert(isFormCompatibleWithRole(possesseeEntity.form, role),
        tupleN(possesseeEntity.form, role)).toString
    })
    true
  }

  def getFormAssocForRole(role : SpcRole) : SpcFormAssocEdge =
  {
    graph.getIdealHypernyms(role).filter(_.isRole).foreach(hypernym => {
      graph.formAssocs.incomingEdgesOf(hypernym).
        asScala.toSeq.headOption.foreach(edge => {
          return edge
        })
    })
    throw new AssertionError("eek")
  }

  private def sanityCheckConstraint(
    possessor : SpcEntity,
    roleName : String) : Boolean =
  {
    val role = resolveRole(possessor.form, roleName).get
    val formEdge = getFormAssocForRole(role)
    val constraint = formEdge.constraint
    if (constraint.upper < Int.MaxValue) {
      val entityEdges = graph.getOutgoingEntityAssocEdges(possessor)
      val c = entityEdges.count(edge => {
        val possesseeRole = getPossesseeRole(edge)
        isHyponym(role, possesseeRole) || isHyponym(possesseeRole, role)
      })
      assert(c <= constraint.upper, (formEdge, possessor))
    }
    true
  }

  def resolveGenitive(
    possessor : SpcEntity,
    role : SpcRole)
      : Set[SpcEntity] =
  {
    SprUtils.orderedSet(graph.getOutgoingEntityAssocEdges(possessor).filter(
      edge => {
        val possesseeRole = getPossesseeRole(edge)
        (isHyponym(possesseeRole, role) ||
          (isHyponym(role, possesseeRole))
        ) &&
        isFormCompatibleWithRole(
          graph.getPossesseeEntity(edge).form, role)
      }
    ).map(
      graph.getPossesseeEntity
    ))
  }

  private[platonic] def resolveIdeal(
    lemma : String) : (Option[SpcForm], Option[SpcRole]) =
  {
    val name = encodeName(lemma)
    getIdealBySynonym(name) match {
      case Some(form : SpcForm) => {
        tupleN(Some(form), None)
      }
      case Some(role : SpcRole) => {
        tupleN(None, Some(role))
      }
      case _ => (None, None)
    }
  }

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    pool.accessCache(
      pool.nounCache,
      tupleN(lemma, context, qualifiers),
      pool.entityTimestamp + pool.taxonomyTimestamp,
      lookupNoun(lemma, context, qualifiers))
  }

  private def lookupNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    resolveForm(lemma).map(form => {
      Success(SprUtils.orderedSet(
        getFormHyponymRealizations(form).filter(
          hasQualifiers(_, form, qualifiers, false))))
    }).getOrElse {
      getEntitiesBySynonym(synthesizeEntitySynonym(lemma)).filter(entity =>
        hasQualifiers(
          entity, entity.form,
          qualifiers ++ lemma.split(" ").map(_.toLowerCase),
          false)
      ) match {
        case seq if (seq.nonEmpty) => {
          Success(SprUtils.orderedSet(seq))
        }
        case empty => {
          if (context == REF_GENITIVE_POSSESSEE) {
            Success(empty.toSet)
          } else {
            fail(s"unknown ideal $lemma")
          }
        }
      }
    }
  }

  def getPropertyForm(property : SpcProperty) : SpcForm =
  {
    val inEdges = getGraph.components.incomingEdgesOf(property).asScala
    assert(inEdges.size == 1)
    getGraph.getContainer(inEdges.head).asInstanceOf[SpcForm]
  }

  override def resolvePropertyState(
    entity : SpcEntity,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    resolveHypernymPropertyState(entity.form, lemma) match {
      case Some((property, stateName)) => Success((property, stateName))
      case _ => fail(s"unknown state $lemma for $entity")
    }
  }

  def resolveFormProperty(form : SpcForm, lemma : String)
      : Option[(SpcProperty, String)] =
  {
    val stateName = lemma
    getFormPropertyMap(form).values.find(
      p => getPropertyStateMap(p).contains(stateName)).map((_, stateName))
  }

  def resolveHypernymPropertyState(
    form : SpcForm,
    lemma : String) : Option[(SpcProperty, String)] =
  {
    getFormHypernyms(form).foreach(hyperForm => {
      resolveFormProperty(hyperForm, lemma) matchPartial {
        case Some((property, stateName)) => {
          return Some(
            tupleN(findProperty(form, property.name).getOrElse(property),
              stateName))
        }
      }
    })
    None
  }

  def getFormPropertyMap(form : SpcForm) : Map[String, SpcProperty] =
  {
    graph.formPropertyIndex.accessComponentMap(form)
  }

  def getPropertyStateObjMap(property : SpcProperty) =
  {
    graph.propertyStateIndex.accessComponentMap(property)
  }

  override def getPropertyStateMap(property : SpcProperty) =
  {
    getPropertyStateObjMap(property).view.mapValues(_.inflected).toMap
  }

  def getEntityPropertyMap(entity : SpcEntity)
      : Map[String, SpcEntityPropertyState] =
  {
    entity match {
      case _ : SpcPersistentEntity => {
        graph.entityPropertyIndex.accessComponentMap(entity)
      }
      case _ => {
        Map.empty
      }
    }
  }

  override def resolvePropertyName(
    entity : SpcEntity,
    propertyName : String) : Try[SpcProperty] =
  {
    findProperty(entity.form, propertyName) match {
      case Some(property) => Success(property)
      case _ => Failure(new IllegalArgumentException(propertyName))
    }
  }

  def findProperty(
    form : SpcForm, name : String) : Option[SpcProperty] =
  {
    getFormHypernyms(form).foreach(hyperForm => {
      getFormPropertyMap(hyperForm).get(name).foreach(matchingProperty => {
        return Some(matchingProperty)
      })
    })

    None
  }

  private def addComponent(
    container : SpcContainmentVertex, component : SpcContainmentVertex) : Unit =
  {
    graph.addComponent(container, component, generateId)
  }

  def instantiateProperty(
    form : SpcForm, name : SilWord,
    domain : SpcPropertyDomain = PROPERTY_OPEN_ENUM) : SpcProperty =
  {
    val propertyName = encodeName(name)
    getFormPropertyMap(form).get(propertyName) match {
      case Some(property) => property
      case _ => {
        val property = new SpcProperty(form, propertyName, domain)
        assert(!getFormPropertyMap(form).contains(property.name))
        meta.propertyExistence(form, property)
        addComponent(form, property)
        property
      }
    }
  }

  def closePropertyStates(property : SpcProperty) : Unit =
  {
    if (property.domain == PROPERTY_OPEN_ENUM) {
      val closedProperty = new SpcProperty(
        property.form, property.name, PROPERTY_CLOSED_ENUM)
      graph.components.addVertex(closedProperty)
      graph.replaceVertex(graph.components, property, closedProperty)
      assert(graph.components.degreeOf(property) == 0)
      graph.components.removeVertex(property)
    }
  }

  def instantiatePropertyState(
    property : SpcProperty, word : SilWord) : Unit =
  {
    val name = encodeName(word)
    assert(!getPropertyStateMap(property).contains(name))
    val propertyState = new SpcPropertyState(
      name, word.recompose(word.decomposed.map(_.inflected)))
    meta.propertyValueExistence(
      getPropertyForm(property), property, propertyState)
    addComponent(property, propertyState)
  }

  def updateEntityProperty(
    originalEntity : SpcEntity, originalProperty : SpcProperty,
    originalLemma : String) : Unit =
  {
    visitEntityProperty(
      originalEntity, originalProperty.name, originalLemma,
      {
        (_, _, _) => {
          Success(Trilean.Unknown)
        }
      }, {
        (entity, propertyName, lemma) => {
          getEntityPropertyMap(entity).get(propertyName).foreach(old => {
            graph.removeComponent(old)
          })
          val ps = new SpcEntityPropertyState(propertyName, lemma)
          addComponent(entity, ps)
          Success(Trilean.True)
        }
      }
    )

  }

  // FIXME:  rewrite this entirely in terms of evaluateEntityProperty
  // and move it to SmcCosmos level
  override def evaluateEntityPropertyPredicate(
    originalEntity : SpcEntity,
    originalProperty : SpcProperty,
    originalLemma : String) : Try[Trilean] =
  {
    visitEntityProperty(
      originalEntity, originalProperty.name, originalLemma,
      {
        (entity, propertyName, lemma) => {
          checkEntityProperty(entity, propertyName, lemma)
        }
      }, {
        (entity, propertyName, lemma) => {
          val result = evaluateEntityProperty(entity, propertyName, true)
          result.map(_._2 match {
            case Some(actual) => Trilean(actual == lemma)
            case _ => Trilean.Unknown
          })
        }
      }
    )
  }

  private def checkEntityProperty(
    entity : SpcEntity, propertyName : String, lemma : String) : Try[Trilean] =
  {
    findProperty(entity.form, propertyName) match {
      case Some(property) => {
        if (property.domain == PROPERTY_CLOSED_ENUM) {
          val propertyStates = getPropertyStateMap(property)
          if (propertyStates.size == 1) {
            Success(Trilean(lemma == propertyStates.keySet.head))
          } else if (!propertyStates.contains(lemma)) {
            Success(Trilean.False)
          } else {
            Success(Trilean.Unknown)
          }
        } else {
          Success(Trilean.Unknown)
        }
      }
      case _ => {
        Success(Trilean.Unknown)
      }
    }
  }

  override def evaluateEntityProperty(
    originalEntity : SpcEntity,
    originalPropertyName : String,
    specific : Boolean = false) : Try[(Option[SpcProperty], Option[String])] =
  {
    if (specific) {
      findProperty(originalEntity.form, originalPropertyName) match {
        case Some(property) => {
          return Success(
            tupleN(Some(property),
              getEntityPropertyMap(originalEntity).
                get(originalPropertyName).map(_.lemma)))
        }
        case _ => {
          return Success((None, None))
        }
      }
    }
    var resultVal : Option[String] = None
    var resultProp : Option[SpcProperty] = None
    def preVisit(entity : SpcEntity, propertyName : String, lemma : String) = {
      findProperty(entity.form, propertyName) match {
        case Some(property) => {
          resultProp = Some(property)
          if (property.domain == PROPERTY_CLOSED_ENUM) {
            val propertyStates = getPropertyStateMap(property)
            if (propertyStates.size == 1) {
              resultVal = Some(propertyStates.keySet.head)
              Success(Trilean.True)
            } else {
              Success(Trilean.Unknown)
            }
          } else {
            Success(Trilean.Unknown)
          }
        }
        case _ => {
          Success(Trilean.Unknown)
        }
      }
    }
    def postVisit(entity : SpcEntity, propertyName : String, lemma : String) = {
      findProperty(entity.form, propertyName) match {
        case Some(property) => {
          resultProp = Some(property)
          getEntityPropertyMap(entity).get(propertyName) match {
            case Some(ps) => {
              resultVal = Some(ps.lemma)
              Success(Trilean.True)
            }
            case _ => {
              Success(Trilean.Unknown)
            }
          }
        }
        case _ => {
          Success(Trilean.Unknown)
        }
      }
    }
    val result = visitEntityProperty(
      originalEntity,
      originalPropertyName,
      "",
      preVisit,
      postVisit
    )
    result.map(_ => (resultProp, resultVal))
  }

  private def visitEntityProperty(
    entity : SpcEntity,
    propertyName : String,
    lemma : String,
    preVisit : (SpcEntity, String, String) => Try[Trilean],
    postVisit : (SpcEntity, String, String) => Try[Trilean])
      : Try[Trilean] =
  {
    preVisit(entity, propertyName, lemma) match {
      case Success(Trilean.Unknown) =>
      case preResult => return preResult
    }
    postVisit(entity, propertyName, lemma)
  }

  def reifyRole(
    possessor : SpcEntity,
    role : SpcRole,
    instantiation : EntityAssocInstantiation,
    onlyIfProven : Boolean,
    assumeNew : Boolean = false) : Set[SpcEntity] =
  {
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formEdge) => {
        if (onlyIfProven) {
          val constraint = formEdge.constraint
          if (constraint.lower == 0) {
            return Set.empty
          }
        }
        val existing : Set[SpcEntity] = {
          if (assumeNew) {
            Set.empty
          } else {
            resolveGenitive(possessor, role)
          }
        }
        if (existing.nonEmpty) {
          existing
        } else {
          // make up possessee out of thin air
          val infix = instantiation match {
            case ENTITY_ASSOC_DEFINITE => SpcForm.POSSESSEE_INFIX
            case _ => SpcForm.TENTATIVE_INFIX
          }
          val name = possessor.name + "_" + role.name +
            infix + generateId
          val roleForms = graph.getFormsForRole(role)
          val form = {
            // if role has a unique form, use it, otherwise make
            // up a multiple-inheritance subform
            if (roleForms.size == 1) {
              roleForms.head
            } else {
              val roleForm = instantiateForm(
                SpcForm.tentativeName(SilWord(name)))
              graph.getFormsForRole(role).foreach(
                hypernym => {
                  addIdealTaxonomy(roleForm, hypernym)
                })
              roleForm
            }
          }
          val (possessee, success) = instantiateEntity(
            form, Seq(SilWord(name)), name)
          assert(success, tupleN(form, name))
          addEntityAssoc(possessor, possessee, role)
          Set(possessee)
        }
      }
      case _ => {
        // FIXME make up role out of thin air?
        Set.empty
      }
    }
  }

  def addAssertion(
    tongue : SprTongue,
    assertion : SpcAssertion) : Unit =
  {
    graph.assertions.addVertex(assertion)
    Option(wordLabeler).foreach(labeler => {
      SpcBeliefRecognizer.recognizeWordRule(tongue, assertion.sentence).foreach(
        rule => {
          labeler.addRule(rule)
        }
      )
    })
  }

  def getEntityPronounWord(
    tongue : SprTongue,
    pronounKey : SilPronounKey,
    entity : SpcEntity) : Option[SilWord] =
  {
    val pronouns = getEntityPronouns(tongue, entity)
    pronouns.get(pronounKey)
  }

  def getEntityPronouns(
    tongue : SprTongue,
    entity : SpcEntity) : SilPronounMap =
  {
    // FIXME tongue needs to be part of cache key
    pool.accessCache(
      pool.pronounCache,
      entity,
      pool.entityTimestamp + pool.taxonomyTimestamp,
      deriveEntityPronouns(tongue, entity)
    )
  }

  private def deriveEntityPronouns(
    tongue : SprTongue,
    entity : SpcEntity) : SilPronounMap =
  {
    entity match {
      case te : SpcTransientEntity if (
        te.value.contains(SpcMeta.PLACEHOLDER_MULTI)
      ) => {
        tongue.getPronounMap(GENDER_NEUTER, COUNT_PLURAL)
      }
      case _ => {
        val map = new mutable.HashMap[SilPronounKey, SilWord]
        assocEntityPronouns(tongue, entity, map)
        if (map.isEmpty) {
          getFormHypernyms(entity.form).foreach(form => {
            if (map.isEmpty) {
              val formEntityName = SpcMeta.formMetaEntityName(form)
              getEntityBySynonym(formEntityName).foreach(formEntity => {
                // FIXME should be reentrant=false, but that is
                // super slow
                assocEntityPronouns(tongue, formEntity, map, true)
              })
            }
          })
        }
        val result = if (map.isEmpty) {
          val gender = getEntityGender(tongue, entity)
          gender.maybeBasic match {
            case Some(g) => {
              tongue.getPronounMap(g, COUNT_SINGULAR)
            }
            case _ => SilPronounMap()
          }
        } else {
          map
        }
        result
      }
    }
  }

  private def assocEntityPronouns(
    tongue : SprTongue,
    entity : SpcEntity,
    map : mutable.Map[SilPronounKey, SilWord],
    reentrant : Boolean = false) : Unit =
  {
    if (map.isEmpty) {
      val props = getEntityPropertyMap(entity)
      val pronouns =
        props.get(SpcMeta.PRONOUN_LIST_METAPROP_NAME).toSeq.flatMap(
          _.lemma.split(',').map(_.trim))
      if (pronouns.nonEmpty) {
        pronouns.foreach(pronoun => {
          val seq = getWordLabeler(tongue).labelWords(
            Seq(tupleN(pronoun, pronoun, 0)),
            foldEphemeralLabels = false)
          assert(seq.size == 1)
          seq.head.foreach(tree => {
            if (tree.label.startsWith(LABEL_PRP)) {
              val key = SilPronounKey(tree.label, PERSON_THIRD)
              if (!map.contains(key)) {
                // FIXME compounds
                map.put(key, SilWord(pronoun))
              }
            }
          })
        })
      } else if (!reentrant) {
        getEntityGender(tongue, entity) matchPartial {
          case SpcGender(genderForm, None) => {
            val genderFormEntityName = SpcMeta.formMetaEntityName(genderForm)
            getEntityBySynonym(genderFormEntityName).foreach(
              genderFormEntity => {
                assocEntityPronouns(tongue, genderFormEntity, map, true)
              }
            )
          }
        }
      }
    }
  }

  def getEntityGender(
    tongue : SprTongue, entity : SpcEntity) : SilGender =
  {
    // FIXME tongue needs to be part of cache key
    pool.accessCache(
      pool.genderCache,
      entity,
      pool.entityTimestamp + pool.taxonomyTimestamp,
      deriveEntityGender(tongue, entity)
    )
  }

  private def deriveEntityGender(
    tongue : SprTongue, entity : SpcEntity) : SilGender =
  {
    assocEntityGender(entity).orElse {
      getFormHypernyms(entity.form).flatMap(form => {
        val formEntityName = SpcMeta.formMetaEntityName(form)
        getEntityBySynonym(formEntityName).flatMap(formEntity => {
          assocEntityGender(formEntity)
        })
      }).headOption
    }.getOrElse(guessGender(tongue, entity.form))
  }

  def getIdealGender(tongue : SprTongue, ideal : SpcIdeal) : SilGender =
  {
    // maybe we should cache this too?
    graph.getIdealHypernyms(ideal).filter(_.isForm).
      map(_.asInstanceOf[SpcForm]).flatMap(form => {
        val formEntityName = SpcMeta.formMetaEntityName(form)
        getEntityBySynonym(formEntityName).flatMap(formEntity => {
          assocEntityGender(formEntity)
        })
      }).to(Iterable).headOption.getOrElse(guessGender(tongue, ideal))
  }

  def getGenderRole(form : SpcForm) : Option[SpcRole] =
  {
    resolveRole(
      form,
      encodeName(SilWord(SpcMeta.GENDER_METAROLE_NAME)),
      true
    )
  }

  def assocEntityGender(entity : SpcEntity) : Option[SpcGender] =
  {
    getGenderRole(entity.form) match {
      case Some(genderRole) => {
        val set = resolveGenitive(entity, genderRole)
        if (set.size == 1) {
          val formEntity = set.head
          val formName = SpcMeta.formNameFromMeta(formEntity.name)
          // FIXME language-specific
          implicit val tongue = SnlUtils.defaultTongue
          val basic = SilWord(SpcWordnetOntology.getNoun(formName)) match {
            case SprPredefWord(PD_MASCULINE) => Some(GENDER_MASCULINE)
            case SprPredefWord(PD_FEMININE) => Some(GENDER_FEMININE)
            case SprPredefWord(PD_NEUTER) => Some(GENDER_NEUTER)
            case _ => None
          }
          resolveForm(formName).map(SpcGender(_, basic))
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private def guessGender(
    tongue : SprTongue,
    ideal : SpcIdeal) : SilGender =
  {
    def genderViaTongue = {
      tongue.deriveGender(SilWord(
        decodeName(SpcWordnetOntology.getNoun(ideal.name))))
    }
    resolveForm(SmcIdeals.FORM_SOMEONE) match {
      case Some(someoneForm) => {
        if (isHyponym(ideal, someoneForm)) {
          GENDER_SOMEONE
        } else {
          genderViaTongue
        }
      }
      case _ => genderViaTongue
    }
  }

  override def applyModifications() : Unit =
  {
    validateBeliefs()
    assert(forkLevel != 0)
    if (forkLevel == 1) {
      graph.applyModifications()
      parent.foreach(cosmos => cosmos.inheritBeliefResources(this))
      validateBeliefs()
    }
  }
}
