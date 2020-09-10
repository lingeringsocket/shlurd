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
package com.lingeringsocket.shlurd.ilang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

class SilSpanishSentencePrinterSpec
    extends SilSentencePrinterSpecification(SprContext(
      new SprSpanishTongue(
        new ShlurdExternalWordnet("/spanish_net.xml"))),
      true)
{
  "Spanish SilSentencePrinter" should
  {
    "preserve sentences" in
    {
      expectPreserved("el perro camina en la calle.")
      expectPreserved("el perro camina entre las calles!")
      expectPreserved("camina el perro entre las calles?")
    }

    "normalize sentences" in
    {
      expectStatement("el perro camina en la calle")
      expectStatement("el perro camina entre las calles")
      expectStatement("los perros caminan entre las calles")
      expectStatement("ella camina")

      // for usted, should be viva la vida
      expectCommand("vive la vida")
    }

    "handle sketchy constructs" in
    {
      // a prescriptivist parser should complain about this, but
      // maybe the speaker is congenitally sad?
      expectStatement("soy triste")
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

      // -er present
      expectStatement("yo bebo")
      expectStatement("tú bebes")
      expectStatement("él bebe")
      expectStatement("nosotros bebemos")
      expectStatement("vosotros bebéis")
      expectStatement("ellas beben")

      // -er preterite
      expectStatement("yo bebí")
      expectStatement("tú bebiste")
      expectStatement("él bebió")
      expectStatement("vosotros bebisteis")
      expectStatement("ellas bebieron")

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

      // estar present
      expectStatement("yo estoy triste")
      expectStatement("tú estás triste")
      expectStatement("él está triste")
      expectStatement("nosotros estamos triste")
      expectStatement("vosotros estáis triste")
      expectStatement("ellas están triste")

      // estar preterite
      expectStatement("yo estuve triste")
      expectStatement("tú estuviste triste")
      expectStatement("él estuvo triste")
      expectStatement("nosotros estuvimos triste")
      expectStatement("vosotros estuvisteis triste")
      expectStatement("ellas estuvieron triste")
    }
  }
}
