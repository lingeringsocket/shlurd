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
import com.lingeringsocket.shlurd.nlang._

import spire.math._

import scala.collection._
import scala.util._

import SnlEnglishLemmas._

trait ZooEntity extends SmcEntity with SmcNamedObject
{
  override def getUniqueIdentifier = name

  def spanish : String
}

sealed case class ZooAnimalEntity(
  name : String, spanish : String) extends ZooEntity
object ZooLion extends ZooAnimalEntity("lion", "león")
object ZooTiger extends ZooAnimalEntity("tiger", "tigre")
object ZooPolarBear extends ZooAnimalEntity("polar bear", "oso polar")
object ZooGrizzlyBear extends ZooAnimalEntity("grizzly bear", "oso pardo")
object ZooSloth extends ZooAnimalEntity("sloth", "perezoso")
object ZooPeacock extends ZooAnimalEntity("peacock", "pavo real")
object ZooHippogriff extends ZooAnimalEntity("hippogriff", "hipogrifo")
object ZooSalamander extends ZooAnimalEntity("salamander", "salamandra")
object ZooNannyGoat extends ZooAnimalEntity(
  "nanny goat", "cabra niñera")
object ZooDomesticGoat extends ZooAnimalEntity(
  "domestic goat", "cabra doméstica")
object ZooSiberianGoat extends ZooAnimalEntity(
  "siberian goat", "cabra siberiana")

sealed case class ZooLocationEntity(
  name : String, spanish : String) extends ZooEntity
object ZooFarm extends ZooLocationEntity("farm", "granja")
object ZooBigCage extends ZooLocationEntity("big cage", "jaula grande")
object ZooSmallCage extends ZooLocationEntity("small cage", "jaula pequeña")

sealed case class ZooPersonEntity(name : String) extends ZooEntity
{
  override def spanish = name
}
object ZooKeeper extends ZooPersonEntity("Muldoon")
object ZooVisitor extends ZooPersonEntity("Malcolm")

object ZooAnimalSleepinessProperty extends SmcProperty

sealed case class ZooAnimalSleepiness(
  name : String, spanish : String) extends ZooEntity
object ZooAnimalAwake extends ZooAnimalSleepiness("awake", "despierto")
object ZooAnimalAsleep extends ZooAnimalSleepiness("asleep", "dormido")

class ZooCosmos(
  isSpanish : Boolean = false
) extends SmcCosmos[SmcEntity, SmcProperty]
{
  private val LEMMA_ANIMAL = "animal"

  def nameFor(e : ZooEntity) : String =
  {
    if (isSpanish) {
      e.spanish
    } else {
      e.name
    }
  }

  private def index[T <: ZooEntity](set : Set[T]) =
    Map(set.map(x => (nameFor(x), x)).toSeq:_*)

  val animals =
    index(Set(ZooLion, ZooTiger, ZooPolarBear,
      ZooGrizzlyBear, ZooSloth,
      ZooNannyGoat, ZooDomesticGoat, ZooSiberianGoat,
      ZooPeacock, ZooHippogriff, ZooSalamander))

  val locations =
    index(Set(ZooFarm, ZooBigCage, ZooSmallCage))

  val people =
    index(Set(ZooKeeper, ZooVisitor))

  val sleepinessValues = index(Set(ZooAnimalAwake, ZooAnimalAsleep))

  // if an animal doesn't appear here, we don't have one at the
  // zoo
  val asleep = Map(
    ZooLion -> Trilean.True,
    ZooTiger -> Trilean.False,
    ZooPolarBear -> Trilean.True,
    ZooGrizzlyBear -> Trilean.False,
    ZooNannyGoat -> Trilean.True,
    ZooDomesticGoat -> Trilean.True,
    ZooSiberianGoat -> Trilean.True,
    ZooSloth -> Trilean.Unknown)

  val containment : Map[SmcEntity, ZooLocationEntity] =
    Map(
      ZooVisitor -> ZooFarm,
      ZooKeeper -> ZooBigCage,
      ZooLion -> ZooBigCage,
      ZooTiger -> ZooBigCage,
      ZooPolarBear -> ZooSmallCage,
      ZooGrizzlyBear -> ZooFarm,
      ZooDomesticGoat -> ZooFarm)

  val ownership : Map[SmcEntity, ZooPersonEntity] =
    Map(
      ZooLion -> ZooKeeper,
      ZooTiger -> ZooVisitor)

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    resolveQualifiedNoun(lemma, context, qualifiers, SnlUtils.defaultTongue)
  }

  def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiersIn : Set[String],
    tongue : SprTongue) : Try[Set[SmcEntity]] =
  {
    val gender = tongue.deriveGender(SilWord(lemma))
    val qualifiers = qualifiersIn.map(q =>
      tongue.correctGenderCount(q, gender, COUNT_SINGULAR, false))
    if ((lemma == LEMMA_WHO) || (lemma == LEMMA_WHOM) ||
      (lemma.startsWith("person")))
    {
      Success(SprUtils.orderedSet(
        people.values))
    } else if (lemma == LEMMA_ANIMAL) {
      Success(SprUtils.orderedSet(
        animals.values))
    } else {
      val name = tongue.getAdjectivePosition match {
        case MOD_AFTER_ALWAYS | MOD_AFTER_DEFAULT => {
          (lemma +: qualifiers.toSeq).mkString(" ")
        }
        case _ => {
          (qualifiers.toSeq :+ lemma).mkString(" ")
        }
      }
      if (context == REF_ADPOSITION_OBJ) {
        Success(SprUtils.orderedSet(
          locations.filterKeys(_.contains(name)).values))
      } else {
        if (animals.filterKeys(_.contains(lemma)).isEmpty) {
          val namedPeople = people.filterKeys(
            _.toLowerCase == lemma.toLowerCase).values
          if (namedPeople.isEmpty) {
            fail("I don't know about this name: " + name)
          } else {
            Success(namedPeople.toSet)
          }
        } else {
          Success(
            animals.filterKeys(_.contains(name)).
              values.filter(asleep.contains).toSet)
        }
      }
    }
  }

  override def resolvePropertyState(
    entity : SmcEntity,
    lemma : String) =
  {
    entity match {
      case a : ZooAnimalEntity => {
        val folded = lemma match {
          case "sleep" => "asleep"
          case _ => lemma
        }
        if (sleepinessValues.contains(folded)) {
          Success((ZooAnimalSleepinessProperty, folded))
        } else {
          fail("I don't know about this state: " + lemma)
        }
      }
      case _ => fail("I don't know about this entity: " + entity)
    }
  }

  override def getPropertyStateMap(property : SmcProperty) =
  {
    property match {
      case ZooAnimalSleepinessProperty => {
        Map(
          "awake" -> "awake",
          "asleep" -> "sleepify"
        )
      }
      case _ => Map.empty
    }
  }

  override def evaluateEntityProperty(
    entity : SmcEntity,
    propertyName : String,
    specific : Boolean = false) : Try[(Option[SmcProperty], Option[String])] =
  {
    fail("I don't know how to evaluate this property: " + propertyName)
  }

  override def evaluateEntityPropertyPredicate(
    entity : SmcEntity,
    property : SmcProperty,
    lemma : String) =
  {
    entity match {
      case animal : ZooAnimalEntity => {
        property match {
          case ZooAnimalSleepinessProperty => {
            sleepinessValues.get(lemma) match {
              case Some(ZooAnimalAwake) => Success(!asleep(animal))
              case Some(ZooAnimalAsleep) => Success(asleep(animal))
              case _ =>
                fail("I don't know about this state: " + lemma)
            }
          }
          case _ =>
            fail("I don't know about this property: " + property)
        }
      }
      case _ =>
        fail("I don't know about this entity: " + entity)
    }
  }
}

class ZooMind(cosmos : ZooCosmos, tongueIn : SprTongue = SnlUtils.defaultTongue)
    extends SmcMind[SmcEntity, SmcProperty, ZooCosmos](cosmos)
{
  private implicit val tongue = getTongue

  override def getTongue : SprTongue = tongueIn

  override def spawn(newCosmos : ZooCosmos) =
  {
    val mind = new ZooMind(newCosmos)
    mind.initFrom(this)
    mind
  }

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[SmcEntity]] =
  {
    cosmos.resolveQualifiedNoun(noun.toNounLemma, context, qualifiers, tongue)
  }

  def splitNoun(entity : ZooEntity) : (String, Seq[String]) =
  {
    val words = cosmos.nameFor(entity).split(" ")
    tongue.getAdjectivePosition match {
      case MOD_AFTER_ALWAYS | MOD_AFTER_DEFAULT =>
        tupleN((words.head, words.drop(1)))
      case _ =>
        tupleN((words.last, words.dropRight(1)))
    }
  }

  override def specificReference(
    annotator : AnnotatorType,
    entity : SmcEntity,
    determiner : SilDeterminer) =
  {
    entity match {
      case animal : ZooAnimalEntity => {
        val (headWord, modifiers) = splitNoun(animal)
        val nounRef = annotator.determinedNounRef(
          SilWord(headWord), determiner)
        if (modifiers.isEmpty) {
          nounRef
        } else {
          annotator.qualifiedRef(
            nounRef, modifiers.map(
              q => SilWord(q)))
        }
      }
      case e : ZooPersonEntity => {
        annotator.nounRef(
          SilWord(cosmos.nameFor(e)))
      }
      case e : ZooLocationEntity => {
        annotator.nounRef(
          SilWord(cosmos.nameFor(e)))
      }
    }
  }

  override def evaluateEntityAdpositionPredicate(
    entity : SmcEntity,
    objEntity : SmcEntity,
    adposition : SilAdposition,
    qualifiers : Set[SilWord]) : Try[Trilean] =
  {
    val map = adposition match {
      case SprPredefAdposition(PD_GENITIVE_OF) => {
        if (!objEntity.isInstanceOf[ZooPersonEntity]) {
          return Success(Trilean.False)
        }
        cosmos.ownership
      }
      case SprPredefAdposition(PD_IN | PD_ON) => {
        if (!objEntity.isInstanceOf[ZooLocationEntity]) {
          return Success(Trilean.False)
        }
        cosmos.containment
      }
      case _ => {
        return Success(Trilean.False)
      }
    }
    map.get(entity) match {
      case Some(actualLocation) =>
        Success(Trilean(objEntity == actualLocation))
      case _ =>
        Success(Trilean.False)
    }
  }

  override def deriveGender(entity : SmcEntity) : SilGender =
  {
    entity match {
      case zoo : ZooEntity => {
        val (headWord, modifiers) = splitNoun(zoo)
        tongue.deriveGender(SilWord(headWord))
      }
      case _ => super.deriveGender(entity)
    }
  }
}
