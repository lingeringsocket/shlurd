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
      new SnlExternalWordnet("/extjwnl_data_spa.xml")))
  }

  "SmcResponder with SnlSpanishTongue" should
  {
    "process statements" in new SpanishResponderContext
    {
      process("hay un tigre") must be equalTo(
        "Claro, hay un tigre.")
    }

    "process questions" in new SpanishResponderContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      val ellipsis = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)

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
      // arguably, this and others should introduce a double
      // negative in the response
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
      process("hay un león y un pavo real?") must be equalTo(
        "No, no hay un león y un pavo real.")
      process("hay un tigre siberiano?") must be equalTo(
        "No, no hay un tigre siberiano.")
      process("hay un oso?") must be equalTo(
        "Sí, hay un oso.")
      process("hay un oso pardo?") must be equalTo(
        "Sí, hay un oso pardo.")
      process("hay un oso polar?") must be equalTo(
        "Sí, hay un oso polar.")
      process("hay un oso rizado?") must be equalTo(
        "No, no hay un oso rizado.")
      process("el oso polar está dormido?") must be equalTo(
        "Sí, el oso polar está dormido.")
      process("el oso pardo está dormido?") must be equalTo(
        "No, el oso pardo no está dormido.")
      process("oso pardo está dormido?") must be equalTo(
        "No, oso pardo no está dormido.")
      // FIXME translate exceptions too
      processExceptionExpected(
        "el oso rizado está dormido?",
        "But I don't know about any such oso.",
        ShlurdExceptionCode.NonExistent)
      processExceptionExpected(
        "el oso está dormido?",
        "Please be more specific about which oso you mean.",
        ShlurdExceptionCode.NotUnique)
      processExceptionExpected(
        "orange the soccer field",
        "Sorry, I cannot understand what you said.",
        ShlurdExceptionCode.FailedParse)
      process("algún oso está dormido?") must be equalTo(
        "Sí, el oso polar está dormido.")
      process("algún oso polar está dormido?") must be equalTo(
        "Sí, el oso polar está dormido.")
      process("los osos están dormidos?") must be equalTo(
        "No, los osos no están dormidos.")
      process("todas cabras están dormidas?") must be equalTo(
        "Sí, todas cabras están dormidas.")
      process("algunas cabras están dormidas?") must be equalTo(
        "Sí, todas las tres están dormidas.")
      val lowLimit = SmcResponseParams(listLimit = 1)
      process("algunas cabras están dormidas?", lowLimit) must be equalTo(
        "Sí, todas las tres están dormidas.")
      process("algunas cabras están despiertas?") must be equalTo(
        "No, ningunas cabras están despiertas.")
      processExceptionExpected(
        "hay un aardvark",
        "Sorry, I don't know about any 'aardvark'.",
        ShlurdExceptionCode.UnknownForm)
      // FIXME translate canned responses
      process("el perezoso está despierto?") must be equalTo(
        "I don't know.")
      process("el perezoso o el tigre está despierto?") must be equalTo(
        "Sí, el tigre está despierto.")
      process("el león o el oso polar está despierto?") must be equalTo(
        "No, ni el león ni el oso polar no está despierto.")
      process("el oso pardo o el tigre está despierto?") must be equalTo(
        "Sí, ambos están despiertos.")
      process("el perezoso o el tigre está dormido?") must be equalTo(
        "I don't know.")
      process("el perezoso o el león está despierto?") must be equalTo(
        "I don't know.")
      process("el perezoso o el león está dormido?") must be equalTo(
        "Sí, el león está dormido.")
      process("el tigre y el oso pardo están despiertos?") must be equalTo(
        "Sí, el tigre y el oso pardo están despiertos.")
      process("los osos y el león están despiertos?", lowLimit) must be equalTo(
        "No, dos de ellos no están despiertos.")
      process("el oso pardo está en algún jaula?") must be equalTo(
        "No, el oso pardo no está en ninguna jaula.")
      processExceptionExpected(
        "el tigre está en la jaula?",
        "Please be more specific about which jaula you mean.",
        ShlurdExceptionCode.NotUnique)
      process("el tigre está en la jaula grande?") must be equalTo(
        "Sí, el tigre está en la jaula grande.")
      process("hay un tigre en la jaula grande?") must be equalTo(
        "Sí, hay un tigre en la jaula grande.")
      process("el tigre en la jaula grande está despierto?") must be equalTo(
        "Sí, el tigre en la jaula grande está despierto.")
      process("el tigre está en la jaula pequeña?") must be equalTo(
        "No, el tigre no está en la jaula pequeña.")
      process("hay un tigre en la jaula pequeña?") must be equalTo(
        "No, no hay un tigre en la jaula pequeña.")
      processExceptionExpected(
        "{el tigre en la jaula pequeña} está despierto?",
        "But I don't know about any such tigre.",
        ShlurdExceptionCode.NonExistent)
      process("la cabra en la granja está despierta?") must be equalTo(
        "No, la cabra en la granja no está despierta.")
      process("yo estoy en la jaula grande?") must be equalTo(
        "No, tú no estás en la jaula grande.")
      process("tú estás en la jaula grande?") must be equalTo(
        "Sí, yo estoy en la jaula grande.")
      process("mi tigre está en la jaula grande?") must be equalTo(
        "Sí, tu tigre está en la jaula grande.")
      process("tu león está en la jaula grande?") must be equalTo(
        "Sí, mi león está en la jaula grande.")
      process("estoy en la jaula grande?") must be equalTo(
        "No, no estás en la jaula grande.")
      process("estás en la jaula grande?") must be equalTo(
        "Sí, estoy en la jaula grande.")

      processExceptionExpected(
        "está en la jaula grande?",
        "Sorry, when you say 'él' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "ella está en la jaula grande?",
        "Sorry, when you say 'ella' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "su tigre está en la jaula grande?",
        "Sorry, when you say 'su' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "nosotros estamos en la jaula grande?",
        "Sorry, when you say 'nosotros' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "ellos están en la jaula grande?",
        "Sorry, when you say 'ellos' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      processExceptionExpected(
        "tu tigre está en la jaula grande?",
        "But I don't know about any such tigre.",
        ShlurdExceptionCode.NonExistent)
      processExceptionExpected(
        "mi león está en la jaula grande?",
        "But I don't know about any such león.",
        ShlurdExceptionCode.NonExistent)
      process("el oso pardo es un oso?") must be equalTo(
        "Sí, el oso pardo es un oso.")
      process("el oso pardo es un león?") must be equalTo(
        "No, el oso pardo no es un león.")
      process("el león es un león?") must be equalTo(
        "Sí, el león es un león.")
      process("el león es un animal?") must be equalTo(
        "Sí, el león es un animal.")
      process("el león es una persona?") must be equalTo(
        "No, el león no es una persona.")
      process("Muldoon es un animal?") must be equalTo(
        "No, Muldoon no es un animal.")
      process("Muldoon es una persona?") must be equalTo(
        "Sí, Muldoon es una persona.")
      process("algunos osos están dormidos?") must be equalTo(
        "Sí, el oso polar está dormido.")
      process("todos osos están dormidos?") must be equalTo(
        "No, el oso pardo no está dormido.")
      process("los osos y el león están dormidos?") must be equalTo(
        "No, el oso pardo no está dormido.")
      process("el tigre y el león están dormidos?") must be equalTo(
        "No, el tigre no está dormido.")
      process("los osos y el león están despiertos?") must be equalTo(
        "No, ni el oso polar ni el león no está despierto.")
      process("todas cabras están despiertas?") must be equalTo(
        "No, ninguna de ellas está despierta.")

      process("cuál cabra está despierta?") must be equalTo(
        "Ninguna cabra está despierta.")
      process("cuáles cabras están despiertas?") must be equalTo(
        "Ningunas cabras están despiertas.")
      process("qué cabra está despierta?") must be equalTo(
        "Ninguna cabra está despierta.")
      val list = "La cabra doméstica, la cabra siberiana, " +
        "y la cabra niñera están dormidas."
      process("cuál cabra está dormida?") must be equalTo(list)
      process("cuáles cabras están dormidas") must be equalTo(list)
      process("{cuál cabra en la granja} está dormida?") must be equalTo(
        "La cabra doméstica está dormida.")
      process("{cuál cabra en la granja} está despierta?") must be equalTo(
        "Ninguna cabra en la granja está despierta.")
      process("cuántas cabras están despiertas?") must be equalTo(
        "Ningunas cabras están despiertas.")
      process("cuántas cabras están dormidas?") must be equalTo(
        "Todas las tres están dormidas.")
      process("cuántas cabras en la granja están dormidas?") must be equalTo(
        "Una de ellas está dormida.")
      process("cuántas cabras niñeras están dormidas?") must be equalTo(
        "Una de ellas está dormida.")
      process("cuántos leones o osos polares están dormidos?") must be equalTo(
        "Ambos están dormidos.")

      process("quién está en la jaula grande?") must be equalTo(
        "Yo estoy en la jaula grande.")
      process("Muldoon tiene un león?") must be equalTo(
        "Sí, Muldoon tiene un león.")
      process("Malcolm tiene un león?") must be equalTo(
        "No, Malcolm no tiene un león.")
      process("Muldoon tiene un tigre?") must be equalTo(
        "No, Muldoon no tiene un tigre.")
      process("Malcolm tiene un tigre?") must be equalTo(
        "Sí, Malcolm tiene un tigre.")
      process("tengo un león?") must be equalTo(
        "No, no tienes un león.")
      process("tienes un león?") must be equalTo(
        "Sí, tengo un león.")
      process("tengo un tigre?") must be equalTo(
        "Sí, tienes un tigre.")
      process("tienes un tigre?") must be equalTo(
        "No, no tengo un tigre.")

      process("quién es Muldoon?") must be equalTo(
        "Yo soy Muldoon.")
      process("quién soy?") must be equalTo(
        "Eres Malcolm.")
      process("quién soy yo?") must be equalTo(
        "Tú eres Malcolm.")
      process("yo soy quién?") must be equalTo(
        "Tú eres Malcolm.")
      process("quién eres?") must be equalTo(
        "Soy Muldoon.")
      process("quién eres tú?") must be equalTo(
        "Yo soy Muldoon.")
      process("quién es Malcolm?") must be equalTo(
        "Tú eres Malcolm.")

      process("hay {un oso}?") must be equalTo(
        "Sí, hay un oso.")
      process("hay (un oso)?") must be equalTo(
        "Sí, hay un oso.")
      process("cuántas cabras están dormidas en la granja?") must be equalTo(
        "Una de ellas está dormida.")
      process("cuál cabra está dormida en la granja ?") must be equalTo(
        "La cabra doméstica está dormida en la granja.")
      process("cuántos osos hay?") must be equalTo(
        "Hay dos de ellos.")
      process("cuántos osos hay en la jaula pequeña?") must be equalTo(
        "Hay uno de ellos en la jaula pequeña.")
      process("cuáles animales están en la jaula grande?") must be equalTo(
        "El león y el tigre están en la jaula grande.")
    }

    "understand inverted order" in new SpanishResponderContext
    {
      process("Muldoon es una persona") must be equalTo(
        "Claro, Muldoon es una persona.")
      process("es Muldoon una persona") must be equalTo(
        "Claro, Muldoon es una persona.")

      process("es Muldoon una persona?") must be equalTo(
        "Sí, Muldoon es una persona.")
      process("Muldoon es una persona?") must be equalTo(
        "Sí, Muldoon es una persona.")

      process("está el león dormido?") must be equalTo(
        "Sí, el león está dormido.")
      process("el león está dormido?") must be equalTo(
        "Sí, el león está dormido.")
      process("está dormido el león?") must be equalTo(
        "Sí, el león está dormido.")

      process("el tigre está en la jaula grande?") must be equalTo(
        "Sí, el tigre está en la jaula grande.")
      process("está en la jaula grande el tigre?") must be equalTo(
        "Sí, el tigre está en la jaula grande.")

      process("hay cuántos osos?") must be equalTo(
        "Hay dos de ellos.")
    }

    "understand conversational singular pronoun references" in new
      SpanishResponderContext
    {
      mind.startConversation

      processExceptionExpected(
        "él está dormido",
        "Sorry, when you say 'él' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      process("está el tigre dormido?") must be equalTo(
        "No, el tigre no está dormido.")
      process("él está despierto?") must be equalTo(
        "Sí, el tigre está despierto.")

      processExceptionExpected(
        "ella está dormida",
        "Sorry, when you say 'ella' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      process("está la cabra siberiana dormida?") must be equalTo(
        "Sí, la cabra siberiana está dormida.")
      process("ella está despierta?") must be equalTo(
        "No, la cabra siberiana no está despierta.")
    }

    "understand conversational plural pronoun references" in new
      SpanishResponderContext
    {
      mind.startConversation

      processExceptionExpected(
        "ellos están dormidos",
        "Sorry, when you say 'ellos' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      process("están el león y la cabra siberiana dormidos?") must be equalTo(
        "Sí, el león y la cabra siberiana están dormidos.")
      process("ellos están dormidos?") must be equalTo(
        "Sí, el león y la cabra siberiana están dormidos.")

      processExceptionExpected(
        "ellas están dormidas",
        "Sorry, when you say 'ellas' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)

      process("están las cabras despiertas?") must be equalTo(
        "No, las cabras no están despiertas.")
      process("ellas están dormidas?") must be equalTo(
        "Sí, la cabra doméstica y la cabra siberiana y " +
          "la cabra niñera están dormidas.")

      process("ellos están dormidos?") must be equalTo(
        "Sí, el león y la cabra siberiana están dormidos.")
    }
  }
}
