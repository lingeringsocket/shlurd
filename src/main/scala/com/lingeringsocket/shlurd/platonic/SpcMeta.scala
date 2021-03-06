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

import com.lingeringsocket.shlurd.ilang._

import scala.collection._

// FIXME add metamodel for
// * aliases
// * associations and their constraints
// * property "closed" attribute
object SpcMeta
{
  val ENTITY_METAFORM_NAME = "spc-entity"

  val FORM_METAFORM_NAME = "spc-form"

  val ROLE_METAFORM_NAME = "spc-role"

  val PROPERTY_METAFORM_NAME = "spc-property"

  val VALUE_METAFORM_NAME = "spc-value"

  val TYPE_METAROLE_NAME = "spc-type"

  val REALIZATION_METAROLE_NAME = "spc-realization"

  val SUPERCLASS_METAROLE_NAME = "spc-superclass"

  val ATTRIBUTE_METAROLE_NAME = "spc-attribute"

  val VALUE_METAROLE_NAME = "spc-property-value"

  val GENDER_METAROLE_NAME = "spc-gender"

  val PRONOUN_LIST_METAPROP_NAME = "spc-pronoun-list"

  val FORM_META_ENTITY_PREFIX = "SPC-Form-"

  val PLACEHOLDER_MULTI = "spc-multi-"

  def formMetaEntityName(form : SpcForm) =
    FORM_META_ENTITY_PREFIX + form.name

  def formNameFromMeta(formMetaEntityName : String) : String =
  {
    formMetaEntityName.stripPrefix(FORM_META_ENTITY_PREFIX)
  }

  def roleMetaEntityName(role : SpcRole) =
    "SPC-Role-" + role.possessor.name + "-" + role.name

  def propertyMetaEntityName(form : SpcForm, property : SpcProperty) =
    "SPC-Property-" + form.name + "-" + property.name

  def valueMetaEntityName(
    form : SpcForm, property : SpcProperty, ps : SpcPropertyState) =
  {
    "SPC-Value-" + form.name + "-" + property.name + "-" + ps.lemma
  }

  def idealMetaEntityName(ideal : SpcIdeal) =
  {
    ideal match {
      case form : SpcForm => formMetaEntityName(form)
      case role : SpcRole => roleMetaEntityName(role)
    }
  }

  def isMetaIdeal(ideal : SpcIdeal) =
    ideal.name.startsWith("spc-")

  def isMetaEntity(entity : SpcEntity) =
    entity.properName.startsWith("SPC-")
}

class SpcMeta(cosmos : SpcCosmos)
{
  import SpcMeta._

  private var enabled = false

  private var buffer : Option[mutable.Buffer[SpcBelief]] = None

  private def newAnnotator = SpcAnnotator()

  def idealExistence(
    metaFormName : String,
    idealEntityName : String,
    positive : Boolean) : Unit =
  {
    val sentence = {
      if (positive) {
        s"$idealEntityName is a $metaFormName"
      } else {
        s"$idealEntityName does not exist"
      }
    }
    val annotator = newAnnotator
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(sentence),
        annotator.nounRef(SilWord(idealEntityName)),
        SilWord(metaFormName),
        Seq(SilWord(idealEntityName)),
        idealEntityName,
        positive
      )
    )
  }

  def formExistence(form : SpcForm, positive : Boolean) : Unit =
  {
    idealExistence(FORM_METAFORM_NAME, formMetaEntityName(form), positive)
  }

  def roleExistence(role : SpcRole) : Unit =
  {
    idealExistence(ROLE_METAFORM_NAME, roleMetaEntityName(role), true)
  }

  def propertyExistence(form : SpcForm, property : SpcProperty) : Unit =
  {
    val propertyEntityName = propertyMetaEntityName(form, property)
    val formEntityName = formMetaEntityName(form)
    val annotator = newAnnotator
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(s"$propertyEntityName is an spc-property"),
        annotator.nounRef(SilWord(propertyEntityName)),
        SilWord(PROPERTY_METAFORM_NAME),
        Seq(SilWord(propertyEntityName)),
        propertyEntityName,
        true
      )
    )
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$propertyEntityName is $formEntityName's spc-attribute"),
        annotator.nounRef(SilWord(formEntityName)),
        annotator.nounRef(SilWord(propertyEntityName)),
        ENTITY_ASSOC_EXISTING,
        SilWord(ATTRIBUTE_METAROLE_NAME),
        true)
    )
  }

  def propertyValueExistence(
    form : SpcForm, property : SpcProperty, ps : SpcPropertyState) : Unit =
  {
    val propertyEntityName = propertyMetaEntityName(form, property)
    val valueEntityName = valueMetaEntityName(form, property, ps)
    val annotator = newAnnotator
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(s"$valueEntityName is an spc-value"),
        annotator.nounRef(SilWord(valueEntityName)),
        SilWord(VALUE_METAFORM_NAME),
        Seq(SilWord(valueEntityName)),
        valueEntityName,
        true
      )
    )
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$valueEntityName is $propertyEntityName's spc-property-value"),
        annotator.nounRef(SilWord(propertyEntityName)),
        annotator.nounRef(SilWord(valueEntityName)),
        ENTITY_ASSOC_EXISTING,
        SilWord(VALUE_METAROLE_NAME),
        true)
    )
  }

  def idealSuperclass(
    subclass : SpcIdeal, superclass : SpcIdeal, positive : Boolean) : Unit =
  {
    val subclassEntityName = idealMetaEntityName(subclass)
    val superclassEntityName = idealMetaEntityName(superclass)
    val modifier = {
      if (positive) {
        ""
      } else {
        "not"
      }
    }
    val annotator = newAnnotator
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$superclassEntityName is $modifier $subclassEntityName's "
            + "spc-superclass"),
        annotator.nounRef(SilWord(subclassEntityName)),
        annotator.nounRef(SilWord(superclassEntityName)),
        ENTITY_ASSOC_EXISTING,
        SilWord(SUPERCLASS_METAROLE_NAME),
        positive))
  }

  def entityExistence(entity : SpcEntity, positive : Boolean) : Unit =
  {
    val entityName = {
      if (entity.properName.isEmpty) {
        entity.name
      } else {
        entity.properName
      }
    }
    val formEntityName = formMetaEntityName(entity.form)
    val modifier = {
      if (positive) {
        ""
      } else {
        "not"
      }
    }
    val annotator = newAnnotator
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$formEntityName is $modifier $entityName's spc-type"),
        annotator.nounRef(SilWord(entityName)),
        annotator.nounRef(SilWord(formEntityName)),
        ENTITY_ASSOC_EXISTING,
        SilWord(TYPE_METAROLE_NAME),
        positive))
  }

  def afterFork(original : SpcMeta) : Unit =
  {
    if (original.enabled) {
      enable()
    }
    if (!original.buffer.isEmpty) {
      buffer = original.buffer
    }
  }

  def isFresh : Boolean =
  {
    enabled && buffer.isEmpty
  }

  def enable() : Unit =
  {
    enabled = true
  }

  def enableBuffering() : Unit =
  {
    buffer = Some(new mutable.ArrayBuffer[SpcBelief])
    enable()
  }

  def flush() : Unit =
  {
    buffer.foreach(b => {
      do {
        val saved = Seq(b.toSeq:_*)
        b.clear()
        saved.foreach(applyBelief)
      } while (!b.isEmpty);
      buffer = None
    })
  }

  def enqueueBelief(belief : SpcBelief) : Unit =
  {
    buffer match {
      case Some(beliefs) => {
        beliefs += belief
      }
      case _ => {
        applyBelief(belief)
      }
    }
  }

  private def applyBelief(belief : SpcBelief) : Unit =
  {
    if (enabled) {
      val params = SpcBeliefParams()
      val mind = new SpcMind(cosmos)
      val responder = SpcResponder(
        mind, params)
      val beliefAccepter = SpcBeliefAccepter(
        responder, params, SpcResultCollector(newAnnotator))
      beliefAccepter.applyBelief(belief)
    }
  }
}
