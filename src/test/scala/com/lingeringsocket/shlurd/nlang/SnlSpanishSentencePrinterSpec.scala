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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

class SnlSpanishSentencePrinterSpec
    extends SilSentencePrinterSpecification(SprContext(
      new SnlSpanishTongue(
        new SnlExternalWordnet("/extjwnl_data_spa.xml"))),
      true)
{
  "SilSentencePrinter with SnlSpanishTongue" should
  {
    "preserve sentences" in
    {
      expectPreserved("el perro camina en la calle.")
      expectPreserved("el perro camina entre las calles!")
      expectPreserved("el perro camina entre las calles?")
      expectPreserved("el oso pardo está dormido?")
    }

    "normalize sentences" in
    {
      expectStatement("el perro camina en la calle")
      expectStatement("el perro está caminando en la calle")
      expectStatement("el perro camina entre las calles")
      expectStatement("los perros caminan entre las calles")
      expectStatement("ella camina")
      expectStatement("usted camina")
      expectStatement("debo de caminar")
      expectStatement("tiene que caminar")
      expectStatement("pueden caminar")
      expectStatement("ustedes caminan")
      expectStatement("podemos ser perros")
      expectStatement("hay un perro")

      expectStatement("ella me besa")
      expectStatement("ella lo besa")
      expectStatement("ella le dice")
      expectStatement("ella besa el perro")
      expectStatement("ella le da un beso")
      expectStatement("ella se lo da")
      // this is incorrect (le should change to se) but
      // we're tolerant folks around here
      expectStatement("ella le lo da")
      expectStatement("ella me lo da")

      // this one is ambiguous between indicative/imperative; we
      // default to interpreting it as indicative
      expectStatement("vives su vida")

      expectCommand("viva su vida")
      expectCommand("vivan sus vidas")
      expectCommand("vivid vuestras vidas")
      expectCommand("no viva su vida")
      expectCommand("no vivas su vida")
      expectCommand("no viváis sus vidas")
      expectCommand("no vivan sus vidas")
    }

    "handle sketchy constructs" in
    {
      // a prescriptivist parser should complain about this, but
      // maybe the speaker is congenitally sad?
      expectStatement("soy triste")

      // this one is even worse, but maybe in the future
      // we will change species like we change our clothes?
      expectStatement("estoy un perro")
    }

    "accept elided subjects" in
    {
      expectStatement("eres un perro")
      expectStatement("estoy triste")
      expectStatement("caminan")
    }

    "verify conjugations" in
    {
      // -ar present
      expectStatement("yo camino en el camino")
      expectStatement("tú caminas")
      expectStatement("él camina")
      expectStatement("nosotros caminamos")
      expectStatement("vosotros camináis")
      expectStatement("ellos caminan")

      // -ar preterite
      expectStatement("yo caminé")
      expectStatement("tú caminaste")
      expectStatement("él caminó")
      expectStatement("vosotros caminasteis")
      expectStatement("ellos caminaron")

      // -ar preterite irregular
      expectStatement("yo anduve")
      expectStatement("tú anduviste")
      expectStatement("él anduvo")
      expectStatement("nosotros anduvimos")
      expectStatement("vosotros anduvisteis")
      expectStatement("ellos anduvieron")

      // -ar future
      expectStatement("yo caminaré")
      expectStatement("tú caminarás")
      expectStatement("él caminará")
      expectStatement("nosotros caminaremos")
      expectStatement("vosotros caminaréis")
      expectStatement("ellos caminarán")

      // -er present
      expectStatement("yo bebo")
      expectStatement("tú bebes")
      expectStatement("él bebe")
      expectStatement("nosotros bebemos")
      expectStatement("vosotros bebéis")
      expectStatement("ellas beben")

      // -er present irregular
      expectStatement("yo sé")
      expectStatement("tú sabes")
      expectStatement("él sabe")
      expectStatement("nosotros sabemos")
      expectStatement("vosotros sabéis")
      expectStatement("ellas saben")

      // -er preterite
      expectStatement("yo bebí")
      expectStatement("tú bebiste")
      expectStatement("él bebió")
      expectStatement("vosotros bebisteis")
      expectStatement("ellas bebieron")

      // -er preterite irregular
      expectStatement("yo supe")
      expectStatement("tú supiste")
      expectStatement("él supo")
      expectStatement("nosotros supimos")
      expectStatement("vosotros supisteis")
      expectStatement("ellas supieron")

      // -er future
      expectStatement("yo beberé")
      expectStatement("tú beberás")
      expectStatement("él beberá")
      expectStatement("nosotros beberemos")
      expectStatement("vosotros beberéis")
      expectStatement("ellas beberán")

      // -er future irregular
      expectStatement("yo beberé")
      expectStatement("tú beberás")
      expectStatement("él beberá")
      expectStatement("nosotros beberemos")
      expectStatement("vosotros beberéis")
      expectStatement("ellas beberán")

      // -er future irregular round 2
      expectStatement("yo sabré")
      expectStatement("tú sabrás")
      expectStatement("él sabrá")
      expectStatement("nosotros sabremos")
      expectStatement("vosotros sabréis")
      expectStatement("ellas sabrán")

      // -ir present
      expectStatement("yo vivo")
      expectStatement("tú vives")
      expectStatement("él vive")
      expectStatement("nosotros vivimos")
      expectStatement("vosotros vivís")
      expectStatement("ellas viven")

      // -ir preterite
      expectStatement("yo viví")
      expectStatement("tú viviste")
      expectStatement("él vivió")
      expectStatement("vosotros vivisteis")
      expectStatement("ellas vivieron")

      // -ir future
      expectStatement("yo viviré")
      expectStatement("tú vivirás")
      expectStatement("él vivirá")
      expectStatement("nosotros viviremos")
      expectStatement("vosotros viviréis")
      expectStatement("ellas vivirán")

      // ser present
      expectStatement("yo soy un perro")
      expectStatement("tú eres un perro")
      expectStatement("él es un perro")
      expectStatement("nosotros somos perros")
      expectStatement("vosotros sois perros")
      expectStatement("ellas son perras")

      // ser preterite
      expectStatement("yo fui un perro")
      expectStatement("tú fuiste un perro")
      expectStatement("él fue un perro")
      expectStatement("nosotros fuimos perros")
      expectStatement("vosotros fuisteis perros")
      expectStatement("ellas fueron perras")

      // ser future
      expectStatement("yo seré un perro")
      expectStatement("tú serás un perro")
      expectStatement("él será un perro")
      expectStatement("nosotros seremos perros")
      expectStatement("vosotros seréis perros")
      expectStatement("ellas serán perras")

      // estar present
      expectStatement("yo estoy triste")
      expectStatement("tú estás triste")
      expectStatement("él está triste")
      expectStatement("nosotros estamos tristes")
      expectStatement("vosotros estáis tristes")
      expectStatement("ellas están tristes")

      // estar preterite
      expectStatement("yo estuve triste")
      expectStatement("tú estuviste triste")
      expectStatement("él estuvo triste")
      expectStatement("nosotros estuvimos tristes")
      expectStatement("vosotros estuvisteis tristes")
      expectStatement("ellas estuvieron tristes")

      // estar future
      expectStatement("yo estaré triste")
      expectStatement("tú estarás triste")
      expectStatement("él estará triste")
      expectStatement("nosotros estaremos tristes")
      expectStatement("vosotros estaréis tristes")
      expectStatement("ellas estarán tristes")
    }
  }
}
