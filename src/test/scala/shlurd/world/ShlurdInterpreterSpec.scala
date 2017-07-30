// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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
package shlurd.world

import shlurd.parser._

import org.specs2.mutable._

import scala.util._

class ShlurdInterpreterSpec extends Specification
{
  private val world = ZooWorld

  private val interpreter = new ShlurdInterpreter(world)

  private def interpret(input : String) =
  {
    val sentence = ShlurdParser(input).parseOne
    interpreter.interpret(sentence)
  }

  "ShlurdInterpreter" should
  {
    "interpret questions" in
    {
      interpret("is the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      interpret("is the lion awake") must be equalTo(
        "No, the lion is not awake.")
      interpret("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      interpret("is there a tiger") must be equalTo(
        "Yes, there is a tiger.")
      interpret("is there a lion and a tiger") must be equalTo(
        "Yes, there is a lion and a tiger.")
      interpret("is there a lion or a peacock") must be equalTo(
        "Yes, there is a lion or a peacock.")
      interpret("is there a peacock") must be equalTo(
        "No, there is not a peacock.")
      // FIXME:  improve response phrasing
      interpret("is there no peacock") must be equalTo(
        "Yes, there is no peacock.")
      interpret("is there a lion and a peacock") must be equalTo(
        "No, there is not a lion and a peacock.")
      interpret("is there an aardvark") must be equalTo(
        "I don't know about this animal: aardvark")
    }

    "interpret statements" in
    {
      interpret("the lion is asleep") must be equalTo(
        "Right, the lion is asleep.")
      interpret("the lion is awake") must be equalTo(
        "Oh, really?")
      interpret("the tiger is asleep") must be equalTo(
        "Oh, really?")
      interpret("the tiger is awake") must be equalTo(
        "Right, the tiger is awake.")
    }
  }

  trait NamedObject {
    def name : String
  }

  sealed case class ZooAnimalEntity(name : String)
      extends ShlurdEntity with NamedObject
  object ZooLion extends ZooAnimalEntity("lion")
  object ZooTiger extends ZooAnimalEntity("tiger")
  object ZooBear extends ZooAnimalEntity("bear")
  object ZooPeacock extends ZooAnimalEntity("peacock")

  object ZooAnimalSleepinessProperty extends ShlurdProperty

  sealed case class ZooAnimalSleepiness(name : String) extends NamedObject
  object ZooAnimalAwake extends ZooAnimalSleepiness("awake")
  object ZooAnimalAsleep extends ZooAnimalSleepiness("asleep")

  object ZooWorld extends ShlurdWorld[ShlurdEntity, ShlurdProperty]
  {
    private def index[T <: NamedObject](set : Set[T]) =
      Map(set.map(x => (x.name, x)).toSeq:_*)

    private val animals = index(Set(ZooLion, ZooTiger, ZooBear, ZooPeacock))

    private val sleepinessValues = index(Set(ZooAnimalAwake, ZooAnimalAsleep))

    // if an animal doesn't appear here, we don't have one at the
    // zoo
    private val awake =
      Map(ZooLion -> true, ZooTiger -> false, ZooBear -> true)

    override def resolveUnqualifiedEntity(
      lemma : String,
      context : ShlurdReferenceContext) =
    {
      animals.get(lemma) match {
        case Some(entity) => {
          awake.get(entity) match {
            case Some(_) => Success(Set(entity))
            case _ => Success(Set.empty)
          }
        }
        case _ => fail("I don't know about this animal: " + lemma)
      }
    }

    override def resolveProperty(
      entity : ShlurdEntity,
      lemma : String) =
    {
      entity match {
        case a : ZooAnimalEntity => {
          if (sleepinessValues.contains(lemma)) {
            Success(ZooAnimalSleepinessProperty)
          } else {
            fail("I don't know about this state: " + lemma)
          }
        }
        case _ => fail("I don't know about this entity: " + entity)
      }
    }

    override def evaluateEntityPropertyPredicate(
      entity : ShlurdEntity,
      property : ShlurdProperty,
      lemma : String) =
    {
      entity match {
        case animal : ZooAnimalEntity => {
          property match {
            case ZooAnimalSleepinessProperty => {
              sleepinessValues.get(lemma) match {
                case Some(ZooAnimalAwake) => Success(!awake(animal))
                case Some(ZooAnimalAsleep) => Success(awake(animal))
                case _ => fail("I don't know about this state: " + lemma)
              }
            }
            case _ => fail("I don't know about this property: " + property)
          }
        }
        case _ => fail("I don't know about this entity: " + entity)
      }
    }

    override def evaluateEntityLocationPredicate(
      entity : ShlurdEntity,
      location : ShlurdEntity,
      locative : ShlurdLocative) =
    {
      fail("I don't know about locations yet")
    }
  }
}
