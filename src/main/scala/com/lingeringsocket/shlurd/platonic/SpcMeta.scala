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

  def formMetaEntityName(form : SpcForm) =
    "SPC-Form-" + form.name

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

  def idealExistence(
    metaFormName : String, idealEntityName : String, positive : Boolean)
  {
    val sentence = {
      if (positive) {
        s"$idealEntityName is a $metaFormName"
      } else {
        s"$idealEntityName does not exist"
      }
    }
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(sentence),
        SilNounReference(SilWord(idealEntityName)),
        SilWord(metaFormName),
        Seq(SilWord(idealEntityName)),
        idealEntityName,
        positive
      )
    )
  }

  def formExistence(form : SpcForm, positive : Boolean)
  {
    idealExistence(FORM_METAFORM_NAME, formMetaEntityName(form), positive)
  }

  def roleExistence(role : SpcRole)
  {
    idealExistence(ROLE_METAFORM_NAME, roleMetaEntityName(role), true)
  }

  def propertyExistence(form : SpcForm, property : SpcProperty)
  {
    val propertyEntityName = propertyMetaEntityName(form, property)
    val formEntityName = formMetaEntityName(form)
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(s"$propertyEntityName is an spc-property"),
        SilNounReference(SilWord(propertyEntityName)),
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
        SilNounReference(SilWord(formEntityName)),
        SilNounReference(SilWord(propertyEntityName)),
        false,
        SilWord(ATTRIBUTE_METAROLE_NAME),
        true)
    )
  }

  def propertyValueExistence(
    form : SpcForm, property : SpcProperty, ps : SpcPropertyState)
  {
    val propertyEntityName = propertyMetaEntityName(form, property)
    val valueEntityName = valueMetaEntityName(form, property, ps)
    enqueueBelief(
      EntityExistenceBelief(
        SilUnparsedSentence(s"$valueEntityName is an spc-value"),
        SilNounReference(SilWord(valueEntityName)),
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
        SilNounReference(SilWord(propertyEntityName)),
        SilNounReference(SilWord(valueEntityName)),
        false,
        SilWord(VALUE_METAROLE_NAME),
        true)
    )
  }

  def idealSuperclass(
    subclass : SpcIdeal, superclass : SpcIdeal, positive : Boolean)
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
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$superclassEntityName is $modifier $subclassEntityName's "
            + "spc-superclass"),
        SilNounReference(SilWord(subclassEntityName)),
        SilNounReference(SilWord(superclassEntityName)),
        false,
        SilWord(SUPERCLASS_METAROLE_NAME),
        positive))
  }

  def entityExistence(entity : SpcEntity, positive : Boolean)
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
    enqueueBelief(
      EntityAssocBelief(
        SilUnparsedSentence(
          s"$formEntityName is $modifier $entityName's spc-type"),
        SilNounReference(SilWord(entityName)),
        SilNounReference(SilWord(formEntityName)),
        false,
        SilWord(TYPE_METAROLE_NAME),
        positive))
  }

  def afterFork(original : SpcMeta)
  {
    if (original.enabled) {
      enable
    }
    if (!original.buffer.isEmpty) {
      buffer = original.buffer
    }
  }

  def isFresh() : Boolean =
  {
    enabled && buffer.isEmpty
  }

  def enable()
  {
    enabled = true
  }

  def enableBuffering()
  {
    buffer = Some(new mutable.ArrayBuffer[SpcBelief])
    enable
  }

  def flush()
  {
    buffer.foreach(b => {
      do {
        val saved = Seq(b:_*)
        b.clear
        saved.foreach(applyBelief)
      } while (!b.isEmpty);
      buffer = None
    })
  }

  def enqueueBelief(belief : SpcBelief)
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

  private def applyBelief(belief : SpcBelief)
  {
    if (enabled) {
      val beliefAccepter = SpcBeliefAccepter(new SpcMind(cosmos))
      beliefAccepter.applyBelief(belief)
    }
  }
}
