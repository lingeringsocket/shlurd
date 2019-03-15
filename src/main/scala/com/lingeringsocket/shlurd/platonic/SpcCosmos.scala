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

import spire.math._

import scala.util._
import scala.collection._
import scala.collection.JavaConverters._

import java.util.concurrent.atomic._

import org.jgrapht._

trait SpcContainmentVertex
{
}

class SpcPropertyState(val lemma : String, val inflected : String)
    extends SpcContainmentVertex
{
  override def toString = s"SpcPropertyState($lemma -> $inflected)"
}

class SpcProperty(val name : String, val isClosed : Boolean)
    extends SmcProperty with SmcNamedObject with SpcContainmentVertex
{
  def isSynthetic = name.contains('_')

  override def toString = s"SpcProperty($name)"
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

sealed abstract class SpcIdeal(val name : String)
    extends SpcNym
{
  override def isIdeal : Boolean = true
}

class SpcStateNormalization(
  val original : SilState,
  val normalized : SilState,
  val isInflected : Boolean)
    extends SpcContainmentVertex
{
}

object SpcForm
{
  private val TENTATIVE_SUFFIX = "_form"

  def tentativeName(word : SilWord) = SilWord(word.lemma + TENTATIVE_SUFFIX)
}

class SpcForm(name : String)
    extends SpcIdeal(name) with SpcContainmentVertex
{
  import SpcForm._

  def isTentative = name.endsWith(TENTATIVE_SUFFIX)

  override def isForm = true

  override def toString = s"SpcForm($name)"
}

class SpcRole(name : String)
    extends SpcIdeal(name)
{
  override def isRole = true

  override def toString = s"SpcRole($name)"
}

trait SpcEntityVertex extends SmcNamedObject
{
}

case class SpcTrigger(
  conditionalSentence : SilConditionalSentence,
  alternative : Option[SilPredicateSentence]
)
{
  def toSentence() : SilSentence =
  {
    alternative match {
      case Some(alternativeSentence) => {
        SilConjunctiveSentence(
          DETERMINER_UNSPECIFIED,
          Seq(conditionalSentence, alternativeSentence),
          SEPARATOR_SEMICOLON)
      }
      case _ => conditionalSentence
    }
  }
}

case class SpcEntity(
  val name : String,
  val form : SpcForm,
  val qualifiers : Set[String],
  val properName : String = "")
    extends SmcEntity with SpcEntityVertex with SpcContainmentVertex
{
  override def isTentative =
    properName.contains("_") && !SpcMeta.isMetaEntity(this)

  override def getUniqueIdentifier = name
}

case class SpcEntitySynonym(val name : String)
    extends SpcEntityVertex
{
}

class SpcCosmicPool
{
  val idGenerator = new AtomicLong

  @transient var taxonomyTimestamp = 1

  @transient var entityTimestamp = 1

  @transient val hypernymCache =
    new mutable.HashMap[SpcIdeal, (Int, Seq[SpcForm])]

  @transient val hyponymCache =
    new mutable.HashMap[SpcIdeal, (Int, Seq[SpcForm])]

  @transient val roleFormCache =
    new mutable.HashMap[SpcRole, (Int, Seq[SpcForm])]

  @transient val roleCompatibilityCache =
    new mutable.HashMap[(SpcRole, SpcForm), (Int, Boolean)]

  @transient val nounCache =
    new mutable.HashMap[(String, Set[String]), (Int, Try[Set[SpcEntity]])]

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
      cache.put(key, tupleN((newTimestamp, newValue)))
      newValue
    }
  }
}

class SpcCosmos(
  graph : SpcGraph = SpcGraph(),
  forkLevel : Int = 0,
  pool : SpcCosmicPool = new SpcCosmicPool
) extends SmcCosmos[SpcEntity, SpcProperty] with DeltaModification
{
  private val unmodifiableGraph = graph.asUnmodifiable

  private[platonic] val meta = new SpcMeta(this)

  def getForms = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isForm).map(_.asInstanceOf[SpcForm])

  def getRoles = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isRole).map(_.asInstanceOf[SpcRole])

  def getEntities =
    graph.entitySynonyms.vertexSet.asScala.toSeq.
      filter(_.isInstanceOf[SpcEntity]).map(_.asInstanceOf[SpcEntity])

  def getTriggers =
    graph.triggers.vertexSet.asScala.toSeq

  def getGraph = unmodifiableGraph

  protected[platonic] def getModifiableGraph = graph

  def getPool = pool

  private[platonic] def getIdGenerator = pool.idGenerator

  protected[platonic] def annotateFormAssoc(
    edge : SpcFormAssocEdge, constraint : SpcCardinalityConstraint,
    isProperty : Boolean)
  {
    edge.constraint = constraint
    edge.isProperty = isProperty
  }

  def fork() : SpcCosmos =
  {
    // we don't currently support true nested forks
    val forkedGraph = {
      if (forkLevel > 0) {
        graph
      } else {
        SpcGraph.fork(graph)
      }
    }
    val forked = new SpcCosmos(forkedGraph, forkLevel + 1, pool)
    forked.meta.afterFork(meta)
    forked
  }

  def asUnmodifiable() : SpcCosmos =
  {
    val frozen = new SpcCosmos(unmodifiableGraph, forkLevel, pool)
    frozen.meta.afterFork(meta)
    frozen
  }

  def copyFrom(src : SpcCosmos)
  {
    assert(getIdGenerator.get == 0)
    val dstGraphs = graph.getGraphs
    dstGraphs.foreach(graph => assert(graph.vertexSet.isEmpty))
    getIdGenerator.set(src.getIdGenerator.get)
    dstGraphs.zip(src.getGraph.getGraphs).foreach({
      case (dstGraph, srcGraph) => {
        // FIXME find a way to do this without ugly casts
        val dstGraphUp = dstGraph.asInstanceOf[Graph[Any, Any]]
        val srcGraphUp = srcGraph.asInstanceOf[Graph[Any, Any]]
        Graphs.addGraph(dstGraphUp, srcGraphUp)
      }
    })
  }

  private def registerIdeal(ideal : SpcIdeal) =
  {
    // FIXME we should validate the form name to make sure it doesn't
    // intrude on system conventions, e.g. spc- prefix.  Likewise
    // for entity names, role names, etc.
    val synonym = SpcIdealSynonym(ideal.name)
    assert(!graph.idealSynonyms.containsVertex(synonym))
    graph.idealSynonyms.addVertex(synonym)
    graph.idealSynonyms.addVertex(ideal)
    graph.idealSynonyms.addEdge(synonym, ideal)
    graph.idealTaxonomy.addVertex(ideal)
    graph.formAssocs.addVertex(ideal)
    getIdealBySynonym(SpcMeta.ENTITY_METAFORM_NAME) match {
      case Some(entityForm) => {
        addIdealTaxonomy(ideal, entityForm)
      }
      case _ =>
    }
    ideal
  }

  private def forgetIdeal(ideal : SpcIdeal)
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

  private def getIdealBySynonym(name : String) : Option[SpcIdeal] =
  {
    val synonym = SpcIdealSynonym(name)
    if (graph.idealSynonyms.containsVertex(synonym)) {
      Some(Graphs.successorListOf(
        graph.idealSynonyms, synonym).iterator.next.asInstanceOf[SpcIdeal])
    } else {
      None
    }
  }

  protected[platonic] def getEntityBySynonym(name : String)
      : Option[SpcEntity] =
  {
    val synonym = SpcEntitySynonym(name.toLowerCase)
    if (graph.entitySynonyms.containsVertex(synonym)) {
      Some(Graphs.successorListOf(
        graph.entitySynonyms, synonym).iterator.next.asInstanceOf[SpcEntity])
    } else {
      None
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

  def getFormHyponymRealizations(form : SpcForm) : Seq[SpcEntity] =
  {
    if (meta.isFresh) {
      getFormHyponyms(form).flatMap(getFormRealizations)
    } else {
      getEntities.filter(entity => graph.isHyponym(entity.form, form))
    }
  }

  def getFormHypernymRealizations(form : SpcForm) : Seq[SpcEntity] =
  {
    if (meta.isFresh) {
      getFormHypernyms(form).flatMap(getFormRealizations)
    } else {
      getEntities.filter(entity => graph.isHyponym(form, entity.form))
    }
  }

  def getRoleRealizations(role : SpcRole) : Seq[SpcEntity] =
  {
    if (meta.isFresh) {
      val forms = pool.accessCache(
        pool.roleFormCache,
        role,
        pool.taxonomyTimestamp,
        graph.getIdealHypernyms(role).toSeq.filter(_.isForm).
          map(_.asInstanceOf[SpcForm]).
          flatMap(getFormHyponyms).distinct.filter(form =>
            isFormCompatibleWithRole(form, role))
      )
      forms.flatMap(getFormRealizations)
    } else {
      getEntities.filter(
        entity => isFormCompatibleWithRole(entity.form, role))
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
    getIdealBySynonym(name) match {
      case Some(ideal) => ideal.name
      case _ => name
    }
  }

  def instantiateIdeal(word : SilWord, assumeRole : Boolean = false) =
  {
    getIdealBySynonym(word.lemma).getOrElse({
      if (assumeRole) {
        instantiateRole(word)
      } else {
        instantiateForm(word)
      }
    })
  }

  def resolveForm(name : String) = resolveIdeal(name)._1

  def resolveRole(name : String) = resolveIdeal(name)._2

  def instantiateForm(word : SilWord) =
  {
    val name = word.lemma
    val ideal = getIdealBySynonym(name).getOrElse(
      registerForm(new SpcForm(name)))
    assert(ideal.isForm)
    ideal.asInstanceOf[SpcForm]
  }

  def instantiateRole(word : SilWord) =
  {
    val name = word.lemma
    val ideal = getIdealBySynonym(name).getOrElse(
      registerRole(new SpcRole(name)))
    assert(ideal.isRole)
    ideal.asInstanceOf[SpcRole]
  }

  private[platonic] def forgetForm(form : SpcForm)
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

  private[platonic] def forgetEntity(entity : SpcEntity)
  {
    pool.entityTimestamp += 1
    meta.entityExistence(entity, false)
    assert(graph.entityAssocs.degreeOf(entity) == 0)
    if (getEntityBySynonym(entity.name) == Some(entity)) {
      val synonymEdges = graph.entitySynonyms.
        incomingEdgesOf(entity).asScala.toSeq
      synonymEdges.foreach(edge => graph.entitySynonyms.removeVertex(
        graph.entitySynonyms.getEdgeSource(edge)))
    }
    assert(graph.entitySynonyms.degreeOf(entity) == 0)
    graph.entitySynonyms.removeVertex(entity)
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
        entityAssocs.edgeSet.asScala.filter(_.formEdge == oldEdge)
      oldEntityEdges.groupBy(graph.getPossessorEntity).
        filter(_._2.size > constraint.upper).isEmpty
    } else {
      true
    }
  }

  private[platonic] def mergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge)
  {
    assert(!graph.inverseAssocs.containsVertex(oldEdge))
    // FIXME we should be able to support this
    assert(!graph.inverseAssocs.containsVertex(newEdge))
    val formAssocs = graph.formAssocs
    val entityAssocs = graph.entityAssocs
    val oldEntityEdges =
      entityAssocs.edgeSet.asScala.filter(_.formEdge == oldEdge)
    oldEntityEdges.foreach(
      entityEdge => {
        entityAssocs.addEdge(
          graph.getPossessorEntity(entityEdge),
          graph.getPossesseeEntity(entityEdge),
          new SpcEntityAssocEdge(newEdge))
        entityAssocs.removeEdge(entityEdge)
      }
    )
    formAssocs.removeEdge(oldEdge)
  }

  private[platonic] def replaceForm(oldForm : SpcForm, newForm : SpcForm)
  {
    pool.taxonomyTimestamp += 1
    assert(oldForm.isTentative)
    assert(graph.components.degreeOf(oldForm) == 0)
    graph.replaceVertex(graph.formAssocs, oldForm, newForm)
    forgetForm(oldForm)
  }

  private[platonic] def replaceEntity(
    oldEntity : SpcEntity, newEntity : SpcEntity)
  {
    // FIXME verify that entities are role-compatible across all
    // relevant form associations, etc
    if (oldEntity != newEntity) {
      // this happens again later, but we have to do it now
      // to make sure that newEntity doesn't end up with
      // multiple types
      meta.entityExistence(oldEntity, false)
      graph.replaceVertex(graph.entityAssocs, oldEntity, newEntity)
      forgetEntity(oldEntity)
      assert {
        val outgoingAssocs =
          getEntityAssocGraph.outgoingEdgesOf(newEntity).asScala.
            toSeq.map(_.formEdge).distinct
        outgoingAssocs.forall(formEdge => {
          sanityCheckConstraint(newEntity, formEdge)
        })
      }
      assert {
        val incomingEdges =
          getEntityAssocGraph.incomingEdgesOf(newEntity).asScala.toSeq
        incomingEdges.forall(entityEdge => {
          sanityCheckConstraint(
            graph.getPossessorEntity(entityEdge),
            entityEdge.formEdge)
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

  def addIdealSynonym(synonymName : String, fundamentalName : String)
  {
    val synonym = SpcIdealSynonym(synonymName)
    assert(!graph.idealSynonyms.containsVertex(synonym))
    graph.idealSynonyms.addVertex(synonym)
    val ideal = getIdealBySynonym(fundamentalName).get
    graph.idealSynonyms.addEdge(synonym, ideal)
  }

  protected[platonic] def getIdealSynonyms =
    graph.idealSynonyms.edgeSet.asScala.toSeq.map(edge =>
      tupleN((graph.idealSynonyms.getEdgeSource(edge).name,
        graph.idealSynonyms.getEdgeTarget(edge).name)))

  def getIdealTaxonomyGraph =
    unmodifiableGraph.idealTaxonomy

  def getFormAssocGraph =
    unmodifiableGraph.formAssocs

  def getEntityAssocGraph =
    unmodifiableGraph.entityAssocs

  def getInverseAssocEdges : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)] =
    graph.inverseAssocs.vertexSet.asScala.toSeq.map(vertex =>
      tupleN((vertex, getInverseAssocEdge(vertex).get)))

  def getRolesForForm(
    form : SpcForm) : Iterable[SpcRole] =
  {
    graph.getIdealHyponyms(form).toSeq.filter(_.isRole).
      map(_.asInstanceOf[SpcRole]).filter(
        role => isFormCompatibleWithRole(form, role))
  }

  def isFormCompatibleWithRole(form : SpcForm, role : SpcRole) : Boolean =
  {
    pool.accessCache(
      pool.roleCompatibilityCache,
      tupleN((role, form)),
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
      graph.isHyponym(form, existing.form) &&
        (qualifiers.subsetOf(existing.qualifiers) ||
          existing.qualifiers.subsetOf(qualifiers))
    } else {
      graph.isHyponym(existing.form, form) &&
        qualifiers.subsetOf(existing.qualifiers)
    }
  }

  protected[platonic] def instantiateEntity(
    form : SpcForm,
    qualifierString : Seq[SilWord],
    properName : String = "") : (SpcEntity, Boolean) =
  {
    val qualifiers = qualifierSet(qualifierString)
    if (properName.isEmpty) {
      getFormHypernymRealizations(form).find(hasQualifiers(
        _, form, qualifiers, true)) match
      {
        case Some(entity) => {
          return tupleN((entity, false))
        }
        case _ =>
      }
    } else {
      getEntityBySynonym(properName) match {
        case Some(entity) => {
          return tupleN((entity, false))
        }
        case _ =>
      }
    }
    val formId = getIdGenerator.getAndIncrement.toString
    val name = {
      if (properName.isEmpty) {
        (qualifierString.map(_.lemma) ++
          Seq(form.name, formId)).mkString("_")
      } else {
        properName
      }
    }
    val entity = new SpcEntity(name, form, qualifiers, properName)
    createOrReplaceEntity(entity)
    tupleN((entity, true))
  }

  protected[platonic] def createOrReplaceEntity(entity : SpcEntity)
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
        graph.entitySynonyms.addEdge(synonym, entity)
        replaceEntity(old, entity)
      }
      case _ => {
        graph.entitySynonyms.addVertex(synonym)
        graph.entitySynonyms.addEdge(synonym, entity)
      }
    }
    meta.entityExistence(entity, true)
  }

  def getInverseAssocEdge(edge : SpcFormAssocEdge)
      : Option[SpcFormAssocEdge] =
  {
    val inverseAssocs = graph.inverseAssocs
    if (!inverseAssocs.containsVertex(edge)) {
      None
    } else {
      Some(Graphs.neighborListOf(inverseAssocs, edge).get(0))
    }
  }

  protected[platonic] def connectInverseAssocEdges(
    edge1 : SpcFormAssocEdge,
    edge2 : SpcFormAssocEdge)
  {
    val inverseAssocs = graph.inverseAssocs
    getInverseAssocEdge(edge1) match {
      case Some(existing) => {
        if (existing == edge2) {
          return
        }
      }
      case _ =>
    }
    inverseAssocs.removeVertex(edge1)
    inverseAssocs.removeVertex(edge2)
    inverseAssocs.addVertex(edge1)
    inverseAssocs.addVertex(edge2)
    inverseAssocs.addEdge(edge1, edge2)
  }

  protected[platonic] def addIdealTaxonomy(
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal)
  {
    if (!graph.isHyponym(hyponymIdeal, hypernymIdeal)) {
      pool.taxonomyTimestamp += 1
      val idealTaxonomy = graph.idealTaxonomy
      val newHypernyms = graph.getIdealHypernyms(hypernymIdeal)
      val redundantEdges = idealTaxonomy.outgoingEdgesOf(hyponymIdeal).
        asScala.filter(
          edge => newHypernyms.contains(graph.getSuperclassIdeal(edge)))
      val newEdge = new SpcTaxonomyEdge()
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

  protected[platonic] def addFormAssoc(
    possessor : SpcIdeal,
    role : SpcRole) : SpcFormAssocEdge =
  {
    graph.getFormAssocEdge(possessor, role) match {
      case Some(edge) => edge
      case _ => {
        val edge = new SpcFormAssocEdge(role.name)
        graph.formAssocs.addEdge(possessor, role, edge)
        edge
      }
    }
  }

  protected[platonic] def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : SpcEntityAssocEdge =
  {
    assert(isFormCompatibleWithRole(possessee.form, role))
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formAssocEdge) => {
        val edge = addEntityAssocEdge(
          possessor, possessee, formAssocEdge)
        getInverseAssocEdge(formAssocEdge) match {
          case Some(inverseAssocEdge) => {
            addEntityAssocEdge(
              possessee, possessor, inverseAssocEdge)
          }
          case _ =>
        }
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
    getEntityAssocEdge(possessor, possessee, role) match {
      case Some(edge) => {
        assert(edge.formEdge == formAssocEdge)
        edge
      }
      case _ => {
        val edge = new SpcEntityAssocEdge(formAssocEdge)
        graph.entityAssocs.addEdge(
          possessor, possessee, edge)
        edge
      }
    }
  }

  protected[platonic] def removeEntityAssociation(
    possessor : SpcEntity,
    possessee : SpcEntity,
    formAssocEdge : SpcFormAssocEdge)
  {
    getEntityAssocEdge(
      possessor, possessee,
      graph.getPossesseeRole(formAssocEdge)
    ).foreach(
      removeEntityAssocEdge
    )
  }

  protected[platonic] def removeEntityAssocEdge(edge : SpcEntityAssocEdge)
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
    graph.entityAssocs.getAllEdges(
      possessor, possessee).asScala.find(edge => {
        graph.isHyponym(role, graph.getPossesseeRole(edge.formEdge))
      }
    )
  }

  def validateBeliefs()
  {
    if (graph.idealTaxonomy.vertexSet.size < 10000) {
      assert(sanityCheck)
    }
  }

  def sanityCheck() : Boolean =
  {
    assert(graph.sanityCheck)
    val idealSet = graph.idealSynonyms.vertexSet.asScala.filter(_.isIdeal).
      map(_.asInstanceOf[SpcIdeal])
    assert(idealSet == graph.idealTaxonomy.vertexSet.asScala)
    assert(idealSet == graph.formAssocs.vertexSet.asScala)
    val formAssocs = graph.formAssocs
    idealSet.foreach(ideal => {
      assert((formAssocs.outDegreeOf(ideal) ==
        formAssocs.outgoingEdgesOf(ideal).
        asScala.map(_.getRoleName).toSet.size),
        ideal.toString)
      assert(getIdealBySynonym(ideal.name) == Some(ideal))
      ideal match {
        case form : SpcForm => {
          assert(graph.components.inDegreeOf(form) == 0)
          assert(
            getFormPropertyMap(form).size +
              getStateNormalizationMap(form).size ==
              graph.components.outDegreeOf(form))
        }
        case _ =>
      }
    })
    val entitySet = getEntities.toSet
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = formEdge.constraint
      if (constraint.upper < Int.MaxValue) {
        val ideal = graph.getPossessorIdeal(formEdge)
        entitySet.filter(
          e => graph.isHyponym(e.form, ideal)).foreach(entity =>
          {
            sanityCheckConstraint(entity, formEdge)
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
    assert(entitySet == graph.entityAssocs.vertexSet.asScala)
    val componentSet = graph.components.vertexSet.asScala
    val propertySet = formSet.flatMap(getFormPropertyMap(_).values).toSet
    val propertyStateSet =
      propertySet.flatMap(getPropertyStateObjMap(_).values).toSet
    val stateNormalizationSet =
      formSet.flatMap(getStateNormalizationMap(_).values).toSet
    val entityPropertyStateSet = entitySet.flatMap(
      getEntityPropertyMap(_).values).toSet
    assert(
      (
        formSet ++ propertySet ++ propertyStateSet ++
          stateNormalizationSet ++ entitySet ++
          entityPropertyStateSet
      ) == componentSet
    )
    propertySet.foreach(property => {
      assert(getPropertyStateMap(property).keySet.size ==
        graph.components.outDegreeOf(property))
    })
    true
  }

  private def sanityCheckConstraint(
    possessor : SpcEntity,
    formEdge : SpcFormAssocEdge) : Boolean =
  {
    val constraint = formEdge.constraint
    if (constraint.upper < Int.MaxValue) {
      val entityEdges = getEntityAssocGraph.
        outgoingEdgesOf(possessor).asScala
      val c = entityEdges.count(_.formEdge == formEdge)
      assert(c <= constraint.upper, (formEdge, possessor))
    }
    true
  }

  def resolveGenitive(
    possessor : SpcEntity,
    role : SpcRole)
      : Set[SpcEntity] =
  {
    SprUtils.orderedSet(getEntityAssocGraph.outgoingEdgesOf(possessor).
      asScala.toSeq.filter(
        edge => {
          graph.isHyponym(role, graph.getPossesseeRole(edge.formEdge)) &&
            isFormCompatibleWithRole(
              graph.getPossesseeEntity(edge).form, role)
        }
      ).map(
        graph.getPossesseeEntity))
  }

  private[platonic] def resolveIdeal(
    lemma : String) : (Option[SpcForm], Option[SpcRole]) =
  {
    getIdealBySynonym(lemma) match {
      case Some(form : SpcForm) => {
        tupleN((Some(form), None))
      }
      case Some(role : SpcRole) => {
        tupleN((None, Some(role)))
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
      tupleN((lemma, qualifiers)),
      pool.entityTimestamp + pool.taxonomyTimestamp,
      lookupNoun(lemma, qualifiers))
  }

  private def lookupNoun(lemma : String, qualifiers : Set[String]) =
  {
    val (formOpt, roleOpt) = resolveIdeal(lemma)
    roleOpt match {
      case Some(role) => {
        Success(SprUtils.orderedSet(
          getRoleRealizations(role).filter(entity =>
              hasQualifiers(entity, entity.form, qualifiers, false))))
      }
      case _ => {
        formOpt match {
          case Some(form) => {
            Success(SprUtils.orderedSet(
              getFormHyponymRealizations(form).filter(
                hasQualifiers(_, form, qualifiers, false))))
          }
          case _ => {
            getEntityBySynonym(lemma) match {
              case Some(entity) if (hasQualifiers(
                entity, entity.form, qualifiers + lemma, false)
              ) => {
                Success(Set(entity))
              }
              case _ => {
                fail(s"unknown ideal $lemma")
              }
            }
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
      case _ => fail(s"unknown property $lemma")
    }
  }

  def resolveFormProperty(form : SpcForm, lemma : String)
      : Option[(SpcProperty, String)] =
  {
    val stateName = resolveStateSynonym(form, lemma)
    getFormPropertyMap(form).values.find(
      p => getPropertyStateMap(p).contains(stateName)).map((_, stateName))
  }

  def resolveHypernymPropertyState(
    form : SpcForm,
    lemma : String) : Option[(SpcProperty, String)] =
  {
    getFormHypernyms(form).foreach(hyperForm => {
      resolveFormProperty(hyperForm, lemma) match {
        case Some((property, stateName)) => {
          return Some(
            tupleN((findProperty(form, property.name).getOrElse(property),
              stateName)))
        }
        case _ =>
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
    getPropertyStateObjMap(property).mapValues(_.inflected)
  }

  def getEntityPropertyMap(entity : SpcEntity)
      : Map[String, SpcEntityPropertyState] =
  {
    graph.entityPropertyIndex.accessComponentMap(entity)
  }

  override def resolvePropertyName(
    entity : SpcEntity,
    propertyName : String) : Try[SpcProperty] =
  {
    findProperty(entity.form, propertyName) match {
      case Some(property) => Success(property)
      case _ => Failure(new IllegalArgumentException)
    }
  }

  def findProperty(
    form : SpcForm, name : String) : Option[SpcProperty] =
  {
    getFormHypernyms(form).foreach(hyperForm => {
      getFormPropertyMap(hyperForm).get(name) match {
        case Some(matchingProperty) => {
          return Some(matchingProperty)
        }
        case _ =>
      }
    })

    None
  }

  def formHasProperty(form : SpcForm, name : String) : Boolean =
  {
    if (!findProperty(form, name).isEmpty) {
      return true
    }
    val hypernymSet = getFormHypernyms(form).toSet
    val outgoingPropertyEdges = hypernymSet.flatMap { hypernym =>
      getFormAssocGraph.outgoingEdgesOf(hypernym).asScala.
        filter(_.isProperty).toSet
    }
    outgoingPropertyEdges.map(_.getRoleName).contains(name)
  }

  def instantiateProperty(form : SpcForm, name : SilWord) : SpcProperty =
  {
    val propertyName = name.lemma
    getFormPropertyMap(form).get(propertyName) match {
      case Some(property) => property
      case _ => {
        val property = new SpcProperty(propertyName, false)
        assert(!getFormPropertyMap(form).contains(property.name))
        meta.propertyExistence(form, property)
        graph.addComponent(form, property)
        property
      }
    }
  }

  def closePropertyStates(property : SpcProperty)
  {
    if (!property.isClosed) {
      val closedProperty = new SpcProperty(property.name, true)
      graph.components.addVertex(closedProperty)
      graph.replaceVertex(graph.components, property, closedProperty)
      assert(graph.components.degreeOf(property) == 0)
      graph.components.removeVertex(property)
    }
  }

  def instantiatePropertyState(
    property : SpcProperty, word : SilWord)
  {
    val propertyState = new SpcPropertyState(word.lemma, word.inflected)
    assert(!getPropertyStateMap(property).contains(propertyState.lemma))
    meta.propertyValueExistence(
      getPropertyForm(property), property, propertyState)
    graph.addComponent(property, propertyState)
  }

  def updateEntityProperty(
    originalEntity : SpcEntity, originalProperty : SpcProperty,
    originalLemma : String)
  {
    visitEntityProperty(
      originalEntity, originalProperty.name, originalLemma,
      {
        (form, _, lemma) => resolveHypernymPropertyState(form, lemma)
      }, {
        (_, _, _) => {
          Success(Trilean.Unknown)
        }
      }, {
        (entity, propertyName, lemma) => {
          getEntityPropertyMap(entity).get(propertyName) match {
            case Some(old) => {
              graph.removeComponent(old)
            }
            case _ =>
          }
          val ps = new SpcEntityPropertyState(propertyName, lemma)
          graph.addComponent(entity, ps)
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
        (form, _, lemma) => resolveHypernymPropertyState(form, lemma)
      }, {
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
        if (property.isClosed) {
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
            tupleN((Some(property),
              getEntityPropertyMap(originalEntity).
                get(originalPropertyName).map( _.lemma))))
        }
        case _ => {
          return Success((None, None))
        }
      }
    }
    def resolveHypernymProperty(
      form : SpcForm, propertyName : String, lemma : String)
        : Option[(SpcProperty, String)]=
    {
      getFormHypernyms(form).foreach(hyperForm => {
        getFormPropertyMap(hyperForm).get(propertyName) match {
          case Some(property) => {
            return Some((property, ""))
          }
          case _ =>
        }
      })
      None
    }
    var resultVal : Option[String] = None
    var resultProp : Option[SpcProperty] = None
    def preVisit(entity : SpcEntity, propertyName : String, lemma : String) = {
      findProperty(entity.form, propertyName) match {
        case Some(property) => {
          resultProp = Some(property)
          if (property.isClosed) {
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
      resolveHypernymProperty,
      preVisit,
      postVisit
    )
    result.map(_ => (resultProp, resultVal))
  }

  private def visitEntityProperty(
    entity : SpcEntity,
    propertyName : String,
    lemma : String,
    resolveHypernymProperty :
        (SpcForm, String, String) => Option[(SpcProperty, String)],
    preVisit : (SpcEntity, String, String) => Try[Trilean],
    postVisit : (SpcEntity, String, String) => Try[Trilean])
      : Try[Trilean] =
  {
    preVisit(entity, propertyName, lemma) match {
      case Success(Trilean.Unknown) =>
      case preResult => return preResult
    }
    val hypernymSet = getFormHypernyms(entity.form).toSet
    val outgoingPropertyEdges = hypernymSet.flatMap { form =>
      getFormAssocGraph.outgoingEdgesOf(form).asScala.
        filter(_.isProperty).toSet
    }
    getEntityAssocGraph.outgoingEdgesOf(entity).asScala.
      filter(edge => outgoingPropertyEdges.contains(edge.formEdge)).
      foreach(edge => {
        val propertyEntity = graph.getPossesseeEntity(edge)
        val map = getFormPropertyMap(propertyEntity.form)
        // FIXME we should prevent violations elsewhere as well
        assert(map.size == 1)
        if (edge.getRoleName == propertyName) {
          return visitEntityProperty(
            propertyEntity,
            map.values.head.name,
            lemma,
            resolveHypernymProperty,
            preVisit,
            postVisit)
        }
        resolveHypernymProperty(
          propertyEntity.form, propertyName, lemma) match
        {
          case Some((underlyingProperty, stateName)) => {
            return visitEntityProperty(
              propertyEntity,
              underlyingProperty.name,
              stateName,
              resolveHypernymProperty,
              preVisit,
              postVisit)
          }
          case _ =>
        }
      })
    postVisit(entity, propertyName, lemma)
  }

  def resolveStateSynonym(form : SpcForm, lemma : String) : String =
  {
    normalizeFormState(form, SilPropertyState(SilWord(lemma))) match {
      case SilPropertyState(word) => word.lemma
      case _ => lemma
    }
  }

  private def getStateNormalizationMap(form : SpcForm)
      : Map[SilState, SpcStateNormalization] =
  {
    graph.stateNormalizationIndex.accessComponentMap(form)
  }

  private[platonic] def addStateNormalization(
    form : SpcForm, state : SilState, transformed : SilState)
  {
    val normalized = normalizeFormState(form, transformed)
    val map = getStateNormalizationMap(form)
    val inflected =
      new SpcStateNormalization(state, normalized, true)
    val uninflected =
      new SpcStateNormalization(foldState(state), normalized, false)
    if (!map.contains(inflected.original)) {
      graph.addComponent(form, inflected)
    }
    if ((uninflected.original != inflected.original) &&
      !map.contains(uninflected.original))
    {
      graph.addComponent(form, uninflected)
    }
  }

  private def normalizeFormState(form : SpcForm, state : SilState) : SilState =
  {
    val map = getStateNormalizationMap(form)
    map.get(state).map(_.normalized).getOrElse(
        map.get(foldState(state)).map(_.normalized).getOrElse(
          state))
  }

  private def foldState(state : SilState) : SilState =
  {
    // FIXME:  should fold compound states as well
    state match {
      case SilPropertyState(word) =>
        SilPropertyState(SilWord(word.lemma))
      case _ => state
    }
  }

  def getInflectedStateNormalizations(form : SpcForm) =
  {
    getStateNormalizationMap(form).values.filter(_.isInflected).map(
      sn => (sn.original, sn.normalized))
  }

  private[platonic] def normalizeHyperFormState(
    form : SpcForm, originalState : SilState) =
  {
    getFormHypernyms(form).foldLeft(originalState) {
      case (state, form) => {
        normalizeFormState(form, state)
      }
    }
  }

  def reifyRole(
    possessor : SpcEntity,
    role : SpcRole,
    onlyIfProven : Boolean)
  {
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formEdge) => {
        if (onlyIfProven) {
          val constraint = formEdge.constraint
          if (constraint.lower == 0) {
            return
          }
        }
        val existing = resolveGenitive(possessor, role)
        if (existing.isEmpty) {
          // make up possessee out of thin air
          val name = possessor.name + "_" + role.name
          val form = instantiateForm(SpcForm.tentativeName(SilWord(name)))
          graph.getFormsForRole(graph.getPossesseeRole(formEdge)).foreach(
            hypernym => addIdealTaxonomy(form, hypernym))
          val (possessee, success) = instantiateEntity(
            form, Seq(SilWord(name)), name)
          assert(success, tupleN((form, name)))
          addEntityAssoc(possessor, possessee, role)
        }
      }
      case _ => {
        // FIXME make up role out of thin air?
      }
    }
  }

  def addTrigger(
    trigger : SpcTrigger)
  {
    graph.triggers.addVertex(trigger)
  }

  override def applyModifications()
  {
    validateBeliefs
    assert(forkLevel != 0)
    if (forkLevel == 1) {
      graph.applyModifications
      validateBeliefs
    }
  }
}
