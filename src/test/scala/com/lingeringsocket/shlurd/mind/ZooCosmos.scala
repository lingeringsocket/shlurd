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

import com.lingeringsocket.shlurd.parser._

import spire.math._

import scala.collection._
import scala.util._

import SprEnglishLemmas._

sealed case class ZooAnimalEntity(name : String)
    extends SmcEntity with SmcNamedObject
object ZooLion extends ZooAnimalEntity("lion")
object ZooTiger extends ZooAnimalEntity("tiger")
object ZooPolarBear extends ZooAnimalEntity("polar bear")
object ZooGrizzlyBear extends ZooAnimalEntity("grizzly bear")
object ZooSloth extends ZooAnimalEntity("sloth")
object ZooPeacock extends ZooAnimalEntity("peacock")
object ZooHippogriff extends ZooAnimalEntity("hippogriff")
object ZooSalamander extends ZooAnimalEntity("salamander")
object ZooMountainGoat extends ZooAnimalEntity("mountain goat")
object ZooDomesticGoat extends ZooAnimalEntity("domestic goat")
object ZooSiberianGoat extends ZooAnimalEntity("siberian goat")

sealed case class ZooLocationEntity(name : String)
    extends SmcEntity with SmcNamedObject
object ZooFarm extends ZooLocationEntity("farm")
object ZooBigCage extends ZooLocationEntity("big cage")
object ZooSmallCage extends ZooLocationEntity("small cage")

sealed case class ZooPersonEntity(name : String)
    extends SmcEntity with SmcNamedObject
object ZooKeeper extends ZooPersonEntity("Muldoon")
object ZooVisitor extends ZooPersonEntity("Malcolm")

object ZooAnimalSleepinessProperty extends SmcProperty

sealed case class ZooAnimalSleepiness(name : String) extends SmcNamedObject
object ZooAnimalAwake extends ZooAnimalSleepiness("awake")
object ZooAnimalAsleep extends ZooAnimalSleepiness("asleep")

class ZooCosmos extends SmcCosmos[SmcEntity, SmcProperty]
{
  private val LEMMA_ANIMAL = "animal"

  private def index[T <: SmcNamedObject](set : Set[T]) =
    Map(set.map(x => (x.name, x)).toSeq:_*)

  private val animals =
    index(Set(ZooLion, ZooTiger, ZooPolarBear,
      ZooGrizzlyBear, ZooSloth,
      ZooMountainGoat, ZooDomesticGoat, ZooSiberianGoat,
      ZooPeacock, ZooHippogriff, ZooSalamander))

  private val locations =
    index(Set(ZooFarm, ZooBigCage, ZooSmallCage))

  private val people =
    index(Set(ZooKeeper, ZooVisitor))

  private val sleepinessValues = index(Set(ZooAnimalAwake, ZooAnimalAsleep))

  // if an animal doesn't appear here, we don't have one at the
  // zoo
  private val asleep = Map(
    ZooLion -> Trilean.True,
    ZooTiger -> Trilean.False,
    ZooPolarBear -> Trilean.True,
    ZooGrizzlyBear -> Trilean.False,
    ZooMountainGoat -> Trilean.True,
    ZooDomesticGoat -> Trilean.True,
    ZooSiberianGoat -> Trilean.True,
    ZooSloth -> Trilean.Unknown)

  private val containment : Map[SmcEntity, ZooLocationEntity] =
    Map(
      ZooVisitor -> ZooFarm,
      ZooKeeper -> ZooBigCage,
      ZooLion -> ZooBigCage,
      ZooTiger -> ZooBigCage,
      ZooPolarBear -> ZooSmallCage,
      ZooGrizzlyBear -> ZooFarm,
      ZooDomesticGoat -> ZooFarm)

  private val ownership : Map[SmcEntity, ZooPersonEntity] =
    Map(
      ZooLion -> ZooKeeper,
      ZooTiger -> ZooVisitor)

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    if ((lemma == LEMMA_WHO) || (lemma == LEMMA_WHOM) ||
      (lemma == LEMMA_PERSON))
    {
      Success(SprUtils.orderedSet(
        people.values))
    } else if (lemma == LEMMA_ANIMAL) {
      Success(SprUtils.orderedSet(
        animals.values))
    } else {
      val name = (qualifiers.toSeq ++ Seq(lemma)).mkString(" ")
      if (context == REF_ADPOSITION_OBJ) {
        Success(SprUtils.orderedSet(
          locations.filterKeys(_.endsWith(name)).values))
      } else {
        if (animals.filterKeys(_.endsWith(lemma)).isEmpty) {
          val namedPeople = people.filterKeys(
            _.toLowerCase == lemma.toLowerCase).values
          if (namedPeople.isEmpty) {
            fail("I don't know about this name: " + name)
          } else {
            Success(namedPeople.toSet)
          }
        } else {
          Success(
            animals.filterKeys(_.endsWith(name)).
              values.filter(asleep.contains).toSet)
        }
      }
    }
  }

  override def resolveProperty(
    entity : SmcEntity,
    lemma : String) =
  {
    entity match {
      case a : ZooAnimalEntity => {
        if (sleepinessValues.contains(lemma)) {
          Success((ZooAnimalSleepinessProperty, lemma))
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

  override def specificReference(
    entity : SmcEntity,
    determiner : SilDeterminer) =
  {
    entity match {
      case animal : ZooAnimalEntity => {
        val words = animal.name.split(" ")
        val nounRef = SilNounReference(
          SilWord(words.last), determiner)
        if (words.size == 1) {
          nounRef
        } else {
          SilReference.qualified(
            nounRef, words.dropRight(1).map(
              q => SilWord(q)))
        }
      }
      case ZooPersonEntity(name) => {
        SilNounReference(
          SilWord(name), DETERMINER_UNSPECIFIED)
      }
      case ZooLocationEntity(name) => {
        SilNounReference(
          SilWord(name), DETERMINER_UNSPECIFIED)
      }
    }
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

  override def evaluateEntityAdpositionPredicate(
    entity : SmcEntity,
    objEntity : SmcEntity,
    adposition : SilAdposition,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    val map = adposition match {
      case SilAdposition.GENITIVE_OF => {
        if (!objEntity.isInstanceOf[ZooPersonEntity]) {
          return Success(Trilean.False)
        }
        ownership
      }
      case SilAdposition.IN | SilAdposition.ON => {
        if (!objEntity.isInstanceOf[ZooLocationEntity]) {
          return Success(Trilean.False)
        }
        containment
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

  override def normalizeState(
    entity : SmcEntity,
    state : SilState) : SilState =
  {
    state match {
      case SilAdpositionalState(
        SilAdposition.IN,
        SilNounReference(
          SilWord("dreamland", _),
          DETERMINER_UNSPECIFIED,
          COUNT_SINGULAR)) =>
        {
          SilPropertyState(SilWord("asleep"))
        }
      case _ => state
    }
  }
}
