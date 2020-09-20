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
import com.lingeringsocket.shlurd.nlang._

class SmcSpanishResponderSpec extends SmcResponderSpecification
{
  private val cosmos = new ZooCosmos(true)

  abstract class SpanishResponderContext(
    responseParams : SmcResponseParams =
      SmcResponseParams(
        thirdPersonPronouns = false,
        reportExceptionCodes = true)
  ) extends ResponderContext(responseParams)
  {
    override protected def getCosmos = cosmos

    override protected def newMind = new ZooMind(cosmos, new SnlSpanishTongue(
      new SnlExternalWordnet("/spanish_net.xml")))
  }

  "SmcResponder with SnlSpanishTongue" should
  {
    "process questions" in new SpanishResponderContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      val ellipsis = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)

      process("hay un tigre") must be equalTo(
        "Claro, hay un tigre.")
      process("hay un tigre?") must be equalTo(
        "Sí, hay un tigre.")
      process("hay tigres?") must be equalTo(
        "Sí, hay tigres.")
      process("hay algún tigre?") must be equalTo(
        "Sí, hay un tigre.")
      process("hay algunos tigres?") must be equalTo(
        "Sí, hay un tigre.")
      process("hay un oso?") must be equalTo(
        "Sí, hay un oso.")
      process("hay un oso polar?") must be equalTo(
        "Sí, hay un oso polar.")
      process("hay osos?") must be equalTo(
        "Sí, hay osos.")
      process("hay un oso triste?") must be equalTo(
        "No, no hay un oso triste.")
      process("hay una salamandra?") must be equalTo(
        "No, no hay una salamandra.")
      process("hay una cabra niñera?") must be equalTo(
        "Sí, hay una cabra niñera.")
      process("el león está dormido?") must be equalTo(
        "Sí, el león está dormido.")
      process("el león está despierto?") must be equalTo(
        "No, el león no está despierto.")
      process("el león está dormido?", terse) must be equalTo(
        "Sí.")
      process("el león está despierto?", terse) must be equalTo(
        "No.")
      // FIXME:  better would be "Sí, el único león está dormido."
      process("los leones están dormidos?") must be equalTo(
        "Sí, los leones están dormidos.")
      process("el tigre está dormido?") must be equalTo(
        "No, el tigre no está dormido.")
      process("el tigre está despierto?") must be equalTo(
        "Sí, el tigre está despierto.")
      process("el león está en sueños?") must be equalTo(
        "Sí, el león está en sueños.")
      process("el tigre está en sueños?") must be equalTo(
        "No, el tigre no está en sueños.")
      process("hay alguna cabra?") must be equalTo(
        "Sí, hay tres de ellas.")
      process("hay un león y un tigre?") must be equalTo(
        "Sí, hay un león y un tigre.")
      process("hay un león o un pavo real?") must be equalTo(
        "Sí, hay un león.")
      process("hay un pavo real?") must be equalTo(
        "No, no hay un pavo real.")
      process("hay un pavo real?", ellipsis) must be equalTo(
        "No, no hay un pavo real.")
      process("hay algún pavo real?") must be equalTo(
        "No, no hay ningún pavo real.")
      process("hay algún pavo real?", ellipsis) must be equalTo(
        "No, no hay ningún pavo real.")
      process("hay algunos pavos reales?") must be equalTo(
        "No, no hay ningunos pavos reales.")
      process("hay algunos pavos reales?", ellipsis) must be equalTo(
        "No, no hay ningunos pavos reales.")
      process("hay un hipogrifo o un pavo real?") must be equalTo(
        "No, no hay ni un hipogrifo ni un pavo real.")
      // FIXME need one more ni
      process(
        "hay un hipogrifo, un pavo real, o una salamandra?") must be equalTo(
        "No, no hay ni un hipogrifo, un pavo real, ni una salamandra.")
      // FIXME:  I'm not even sure what the right answer is to this
      process("hay ningún pavo real?") must be equalTo(
        "Sí, hay ningún pavo real.")
    }

    "understand inverted order" in new SpanishResponderContext
    {
      skipped("not working yet")
      process("está el león dormido?") must be equalTo(
        "Sí, el león está dormido.")
      process("está dormido el león?") must be equalTo(
        "Sí, el león está dormido.")
    }

    "process unsupported questions" in new SpanishResponderContext
    {
      skipped("not translated/tested yet")
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      val ellipsis = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)
      process("is there a lion and a peacock") must be equalTo(
        "No, there is not a lion and a peacock.")
      process("is there a siberian tiger") must be equalTo(
        "No, there is not a siberian tiger.")
      process("is there a bear") must be equalTo(
        "Yes, there is a bear.")
      process("is there a grizzly bear") must be equalTo(
        "Yes, there is a grizzly bear.")
      process("is there a polar bear") must be equalTo(
        "Yes, there is a polar bear.")
      process("is there a fuzzy bear") must be equalTo(
        "No, there is not a fuzzy bear.")
      process("is the polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is the grizzly bear asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("is grizzly bear asleep") must be equalTo(
        "No, grizzly bear is not asleep.")
      processExceptionExpected(
        "is the fuzzy bear asleep",
        "But I don't know about any such bear.",
        ShlurdExceptionCode.NonExistent)
      processExceptionExpected(
        "is the bear asleep",
        "Please be more specific about which bear you mean.",
        ShlurdExceptionCode.NotUnique)
      processExceptionExpected(
        "orange the soccer field",
        "Sorry, I cannot understand what you said.",
        ShlurdExceptionCode.FailedParse)
      process("is any bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is any polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is some bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("are the bears asleep") must be equalTo(
        "No, the bears are not asleep.")
      process("are any bears asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("are all bears asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("are all goats asleep") must be equalTo(
        "Yes, all goats are asleep.")
      process("are any goats asleep") must be equalTo(
        "Yes, all three of them are asleep.")
      val lowLimit = SmcResponseParams(listLimit = 1)
      process("are any goats asleep", lowLimit) must be equalTo(
        "Yes, all three of them are asleep.")
      process("are any goats awake") must be equalTo(
        "No, no goats are awake.")
      process("are all goats awake") must be equalTo(
        "No, none of them are awake.")
      processExceptionExpected(
        "is there an aardvark",
        "Sorry, I don't know about any 'aardvark'.",
        ShlurdExceptionCode.UnknownForm
      )
      process("is the sloth awake") must be equalTo(
        "I don't know.")
      process("is the sloth or the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      process("is the lion or the polar bear awake") must be equalTo(
        "No, neither the lion nor the polar bear is awake.")
      process("is the grizzly bear or the tiger awake") must be equalTo(
        "Yes, both of them are awake.")
      process("is the sloth or the tiger asleep") must be equalTo(
        "I don't know.")
      process("is the sloth or the lion awake") must be equalTo(
        "I don't know.")
      process("is the sloth or the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      process("are the tiger and the grizzly bear awake") must be equalTo(
        "Yes, the tiger and the grizzly bear are awake.")
      process("are the bears and the lion asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("are the bears and the lion awake") must be equalTo(
        "No, neither the polar bear nor the lion is awake.")
      process("are the bears and the lion awake", lowLimit) must be equalTo(
        "No, two of them are not awake.")
      process("are the tiger and the lion asleep") must be equalTo(
        "No, the tiger is not asleep.")
      process("is the grizzly bear in any cage") must be equalTo(
        "No, the grizzly bear is in no cage.")
      processExceptionExpected(
        "is the tiger in the cage",
        "Please be more specific about which cage you mean.",
        ShlurdExceptionCode.NotUnique)
      process("is the tiger in the big cage") must be equalTo(
        "Yes, the tiger is in the big cage.")
      process("is there a tiger in the big cage") must be equalTo(
        "Yes, there is a tiger in the big cage.")
      process("is the tiger in the small cage") must be equalTo(
        "No, the tiger is not in the small cage.")
      process("is there a tiger in the small cage") must be equalTo(
        "No, there is not a tiger in the small cage.")
      process("is the tiger in the big cage awake") must be equalTo(
        "Yes, the tiger in the big cage is awake.")
      processExceptionExpected(
        "is the tiger in the small cage awake",
        "But I don't know about any such tiger.",
        ShlurdExceptionCode.NonExistent)
      process("is the goat on the farm awake") must be equalTo(
        "No, the goat on the farm is not awake.")
      process("which goat is awake") must be equalTo(
        "No goat is awake.")
      process("which goats are awake") must be equalTo(
        "No goats are awake.")
      process("what goat is awake") must be equalTo(
        "No goat is awake.")
      val list = "The domestic goat, the nanny goat, " +
        "and the siberian goat are asleep."
      process("which goat is asleep") must be equalTo(list)
      process("which goats are asleep") must be equalTo(list)
      process("which goat in the farm is asleep") must be equalTo(
        "The domestic goat is asleep.")
      process("which goat in the farm is awake") must be equalTo(
        "No goat in the farm is awake.")
      process("how many goats are awake") must be equalTo(
        "No goats are awake.")
      process("how many goats are asleep") must be equalTo(
        "All three of them are asleep.")
      process("how many goats in the farm are asleep") must be equalTo(
        "One of them is asleep.")
      process("how many nanny goats are asleep") must be equalTo(
        "One of them is asleep.")
      process("how many lions or polar bears are asleep") must be equalTo(
        "Both of them are asleep.")
      process("am I in the big cage") must be equalTo(
        "No, you are not in the big cage.")
      process("are you in the big cage") must be equalTo(
        "Yes, I am in the big cage.")
      processExceptionExpected(
        "is he in the big cage",
        "Sorry, when you say 'he' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "is his tiger in the big cage",
        "Sorry, when you say 'his' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "are we in the big cage",
        "Sorry, when you say 'we' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "are they in the big cage",
        "Sorry, when you say 'they' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      process("is my tiger in the big cage") must be equalTo(
        "Yes, your tiger is in the big cage.")
      processExceptionExpected(
        "is your tiger in the big cage",
        "But I don't know about any such tiger.",
        ShlurdExceptionCode.NonExistent)
      processExceptionExpected(
        "is my lion in the big cage",
        "But I don't know about any such lion.",
        ShlurdExceptionCode.NonExistent)
      process("is your lion in the big cage") must be equalTo(
        "Yes, my lion is in the big cage.")
      process("who is in the big cage") must be equalTo(
        "I am in the big cage.")
      process("does Muldoon have a lion") must be equalTo(
        "Yes, Muldoon has a lion.")
      process("does Malcolm have a lion") must be equalTo(
        "No, Malcolm does not have a lion.")
      process("has Muldoon a tiger") must be equalTo(
        "No, Muldoon does not have a tiger.")
      process("does Malcolm have a tiger") must be equalTo(
        "Yes, Malcolm has a tiger.")
      process("do I have a lion") must be equalTo(
        "No, you do not have a lion.")
      process("do you have a lion") must be equalTo(
        "Yes, I have a lion.")
      process("do I have a tiger") must be equalTo(
        "Yes, you have a tiger.")
      process("do you have a tiger") must be equalTo(
        "No, I do not have a tiger.")
      process("is the grizzly bear a bear") must be equalTo(
        "Yes, the grizzly bear is a bear.")
      process("is the grizzly bear a lion") must be equalTo(
        "No, the grizzly bear is not a lion.")
      process("is the lion a lion") must be equalTo(
        "Yes, the lion is a lion.")
      process("who is Muldoon") must be equalTo(
        "I am Muldoon.")
      process("who am I") must be equalTo(
        "You are Malcolm.")
      process("who are you") must be equalTo(
        "I am Muldoon.")
      process("who is Malcolm") must be equalTo(
        "You are Malcolm.")
      process("is the lion an animal") must be equalTo(
        "Yes, the lion is an animal.")
      process("is the lion a person") must be equalTo(
        "No, the lion is not a person.")
      process("is Muldoon an animal") must be equalTo(
        "No, Muldoon is not an animal.")
      process("is Muldoon a person") must be equalTo(
        "Yes, Muldoon is a person.")
      process("which animals are in the big cage") must be equalTo(
        "The lion and the tiger are in the big cage.")
    }
  }
}
