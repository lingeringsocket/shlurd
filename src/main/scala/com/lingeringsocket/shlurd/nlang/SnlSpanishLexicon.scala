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

import scala.collection._

object SnlSpanishLemmas
{
  val LEMMA_A = "a"
  val LEMMA_ACA = "acá"
  val LEMMA_ADONDE_ACCENTED = "adónde"
  val LEMMA_ADONDE = "adónde"
  val LEMMA_AHI = "ahí"
  val LEMMA_AL = "al"
  val LEMMA_ALGUN = "algún"
  val LEMMA_ALGUNO = "alguno"
  val LEMMA_ALGUNA = "alguna"
  val LEMMA_ALGUNAS = "algunas"
  val LEMMA_ALGUNOS = "algunos"
  val LEMMA_ALLA = "allá"
  val LEMMA_ALLI = "allí"
  val LEMMA_AMBA = "amba"
  val LEMMA_AMBAS = "ambas"
  val LEMMA_AMBO = "ambo"
  val LEMMA_AMBOS = "ambos"
  val LEMMA_AQUI = "aquí"
  val LEMMA_AQUEL = "aquel"
  val LEMMA_AQUELLA = "aquella"
  val LEMMA_AQUELLAS = "aquellas"
  val LEMMA_AQUELLO = "aquello"
  val LEMMA_AQUELLOS = "aquellos"
  val LEMMA_CADA = "cada"
  val LEMMA_COMO_ACCENTED = "cómo"
  val LEMMA_COMO = "como"
  val LEMMA_CUAL_ACCENTED = "cuál"
  val LEMMA_CUAL = "cual"
  val LEMMA_CUALES_ACCENTED = "cuáles"
  val LEMMA_CUALES = "cuales"
  val LEMMA_CUANTOS_ACCENTED = "cuántos"
  val LEMMA_CUANTAS_ACCENTED = "cuántas"
  val LEMMA_DE = "de"
  val LEMMA_DEBER = "deber"
  val LEMMA_DEL = "del"
  val LEMMA_DONDE_ACCENTED = "dónde"
  val LEMMA_DONDE = "donde"
  val LEMMA_EL = "el"
  val LEMMA_EL_ACCENTED = "él"
  val LEMMA_ELLA = "ella"
  val LEMMA_ELLAS = "ellas"
  val LEMMA_ELLO = "ello"
  val LEMMA_ELLOS = "ellos"
  val LEMMA_ESA = "esa"
  val LEMMA_ESAS = "esas"
  val LEMMA_ESO = "eso"
  val LEMMA_ESOS = "esos"
  val LEMMA_ESTA = "esta"
  val LEMMA_ESTAR = "estar"
  val LEMMA_ESTAS = "estas"
  val LEMMA_ESTO = "esto"
  val LEMMA_ESTOS = "estos"
  val LEMMA_EXISTIR = "existir"
  val LEMMA_HABER = "haber"
  val LEMMA_HACER = "hacer"
  val LEMMA_HACIA = "hacia"
  val LEMMA_LA = "la"
  val LEMMA_LAS = "las"
  val LEMMA_LE = "le"
  val LEMMA_LES = "les"
  val LEMMA_LO = "lo"
  val LEMMA_LOS = "los"
  val LEMMA_ME = "me"
  val LEMMA_MI = "mi"
  val LEMMA_MI_ACCENTED = "mí"
  val LEMMA_MIA = "mía"
  val LEMMA_MIO = "mío"
  val LEMMA_MIAS = "mías"
  val LEMMA_MIOS = "míos"
  val LEMMA_MIS = "mis"
  val LEMMA_NI = "ni"
  val LEMMA_NINGUN = "ningún"
  val LEMMA_NINGUNA = "ninguna"
  val LEMMA_NINGUNAS = "ningunas"
  val LEMMA_NINGUNO = "ninguno"
  val LEMMA_NINGUNOS = "ningunos"
  val LEMMA_NO = "no"
  val LEMMA_NOS = "nos"
  val LEMMA_NOSOTRAS = "nosotras"
  val LEMMA_NOSOTROS = "nosotros"
  val LEMMA_NUESTRA = "nuestra"
  val LEMMA_NUESTRAS = "nuestras"
  val LEMMA_NUESTRO = "nuestro"
  val LEMMA_NUESTROS = "nuestros"
  val LEMMA_O = "o"
  val LEMMA_OS = "os"
  val LEMMA_OTRO = "otro"
  val LEMMA_PODER = "poder"
  val LEMMA_QUE_ACCENTED = "qué"
  val LEMMA_QUE = "que"
  val LEMMA_QUIEN_ACCENTED = "quién"
  val LEMMA_QUIEN = "quien"
  val LEMMA_QUIENES_ACCENTED = "quiénes"
  val LEMMA_QUIENES = "quienes"
  val LEMMA_SE = "se"
  val LEMMA_SER = "ser"
  val LEMMA_SU = "su"
  val LEMMA_SUS = "sus"
  val LEMMA_TE = "te"
  val LEMMA_TENER = "tener"
  val LEMMA_TI = "ti"
  val LEMMA_TODA = "toda"
  val LEMMA_TODAS = "todas"
  val LEMMA_TODO = "todo"
  val LEMMA_TODOS = "todos"
  val LEMMA_TU_ACCENTED = "tú"
  val LEMMA_TU = "tu"
  val LEMMA_TUS = "tus"
  val LEMMA_TUYA = "tuya"
  val LEMMA_TUYAS = "tuyas"
  val LEMMA_TUYO = "tuyo"
  val LEMMA_TUYOS = "tuyos"
  val LEMMA_SUYA = "suya"
  val LEMMA_SUYAS = "suyas"
  val LEMMA_SUYO = "suyo"
  val LEMMA_SUYOS = "suyos"
  val LEMMA_U = "u"
  val LEMMA_UN = "un"
  val LEMMA_UNA = "una"
  val LEMMA_UNAS = "unas"
  val LEMMA_UNO = "uno"
  val LEMMA_UNOS = "unos"
  val LEMMA_USTED = "usted"
  val LEMMA_USTEDES = "ustedes"
  val LEMMA_VOSOTRAS = "vosotras"
  val LEMMA_VOSOTROS = "vosotros"
  val LEMMA_VUESTRA = "vuestra"
  val LEMMA_VUESTRAS = "vuestras"
  val LEMMA_VUESTRO = "vuestro"
  val LEMMA_VUESTROS = "vuestros"
  val LEMMA_Y = "y"
  val LEMMA_YO = "yo"
}

case class SnlSpanishPronounCoord(
  person : SilPerson,
  gender : SilGender,
  count : SilCount,
  proximity : SilProximity,
  inflection : SilInflection,
  possesseeCount : SilCount = COUNT_SINGULAR,
  politeness : SilPoliteness = SilPoliteness.DEFAULT
)

object SnlSpanishLexicon
{
  import SnlUtils._
  import SnlSpanishLemmas._

  val prepositions = readLexicon("/spanish/prepositions.txt")

  val subordinates = readLexicon("/spanish/subordinates.txt")

  // for now these are all rolled together
  val proper = SnlEnglishLexicon.proper

  val nounGenders = readGenderMap("/spanish/gender.txt.gz")

  val freqs = SnlUtils.readFreqMap("/spanish/freq.txt.gz")

  val stopList = Set(
    LEMMA_ELLA, LEMMA_YO, LEMMA_NO, LEMMA_SER, LEMMA_SUS, LEMMA_AL,
    LEMMA_NOS, LEMMA_ME, LEMMA_TE, LEMMA_OS
  ) ++ stopListPunct

  val nominativeToCoord = Map(
    LEMMA_YO -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_TU_ACCENTED -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_USTED -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE,
      politeness = POLITENESS_RESPECTFUL),
    LEMMA_EL_ACCENTED -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_NOSOTROS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_NOSOTRAS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_VOSOTROS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_VOSOTRAS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_USTEDES -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE,
      politeness = POLITENESS_RESPECTFUL),
    LEMMA_ELLOS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLAS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE)
  )

  val accusativeToCoord = Map(
    LEMMA_ME -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_TE -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_NOS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_OS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LOS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LAS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE)
  )

  val adpositionedToCoord = Map(
    LEMMA_MI_ACCENTED -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED),
    LEMMA_TI -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED)
  )

  val dativeToCoord = Map(
    LEMMA_ME -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_DATIVE),
    LEMMA_TE -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_DATIVE),
    LEMMA_LE -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_DATIVE),
    LEMMA_NOS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_DATIVE),
    LEMMA_OS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_DATIVE),
    LEMMA_LES -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_DATIVE),

    // annoying substitution for le/les before
    // lo/la/los/las; note that we use COUNT_MASS
    // as a hack for representing the fact that
    // this can match either singular or plural
    LEMMA_SE -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_SOMEONE, COUNT_MASS,
      PROXIMITY_ENTITY, INFLECT_DATIVE)
  )

  val genitiveToCoord = Map(
    LEMMA_MI -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_MIS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_TU -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_TUS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_SU -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_SUS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_NUESTRO -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_NUESTROS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_NUESTRA -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_NUESTRAS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_VUESTRO -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_VUESTROS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_VUESTRA -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_VUESTRAS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL)
  )

  val possesseeToCoord = Map(
    LEMMA_MIO -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_MIA -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_MIOS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_MIAS -> SnlSpanishPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_TUYO -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_TUYA -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_TUYOS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_TUYAS -> SnlSpanishPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_SUYO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_SUYA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_SUYOS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_SUYAS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL)
  )

  val demonstrativeToCoord = Map(
    LEMMA_ESTO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTOS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTAS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUEL -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLA -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLO -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLOS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLAS -> SnlSpanishPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE)
  )

  // note that we leave out dativeToCoord here since it overlaps with
  // accusativeToCoord
  val pronounToCoord = (
    nominativeToCoord ++ accusativeToCoord ++ adpositionedToCoord ++
      genitiveToCoord ++
      possesseeToCoord ++ demonstrativeToCoord)

  val coordToPronoun = Seq(
    nominativeToCoord, accusativeToCoord, adpositionedToCoord,
    dativeToCoord, genitiveToCoord,
    possesseeToCoord, demonstrativeToCoord).map(_.map(_.swap)
  ).reduce((m1, m2) => m1 ++ m2)

  assert(coordToPronoun.size == (pronounToCoord.size + dativeToCoord.size))

  val pronounLemmas = pronounToCoord.keySet ++ dativeToCoord.keySet

  val predefToLemma : Map[SprPredef, String] = Map(
    PD_ABOVE -> "arriba de",
    PD_ADVERBIAL_TMP -> LEMMA_ADVERBIAL_TMP,
    PD_AFTER -> "después de",
    PD_ALSO -> "también",
    // FIXME Spanish doesn't discriminate from PD_BETWEEN
    PD_AMONG -> "entre",
    PD_AND -> LEMMA_Y,
    // FIXME needs to respect gender
    PD_ANOTHER -> LEMMA_OTRO,
    PD_AS -> LEMMA_COMO,
    // FIXME a lot of variations for this one
    PD_AT -> LEMMA_HACIA,
    // FIXME why does this even exist?
    PD_BACK -> "atrás de",
    // FIXME:  sometimes should be LEMMA_ESTAR instead
    PD_BE -> LEMMA_SER,
    PD_BEFORE -> "antes de",
    PD_BELIEVE -> "crea",
    PD_BEHIND -> "detrás de",
    // FIXME: agreement
    PD_BOTH -> LEMMA_AMBOS,
    PD_CONSEQUENTLY -> "consiguientemente",
    // FIXME:  the real thing
    PD_EITHER -> "cualquiera",
    PD_EQUIVALENTLY -> "equivalentemente",
    PD_EXCEPT -> "excepto",
    PD_EXIST -> LEMMA_EXISTIR,
    PD_FEMININE -> "feminino",
    // FIXME: agreement
    PD_FORMER -> "primero",
    PD_FROM -> "desde",
    PD_FRONT -> "enfrente de",
    PD_GENERALLY -> "generalmente",
    // Spanish doesn't really need this
    PD_GENITIVE_OF -> "_of_",
    PD_HOW_MANY -> LEMMA_CUANTOS_ACCENTED,
    PD_IF -> "si",
    PD_IN -> "en",
    PD_INSIDE -> "dentro de",
    PD_KIND -> "tipo",
    // FIXME: agreement
    PD_LATTER -> "último",
    PD_LEFT -> "a la izquierda de",
    PD_MASCULINE -> "masculino",
    PD_NEAR -> "cerca de",
    PD_NEARBY -> "alrededor de",
    // FIXME:  the real thing
    PD_NEITHER_NOUN -> LEMMA_NINGUNO,
    PD_NEITHER_DETERMINER -> LEMMA_NI,
    PD_NOR -> LEMMA_NI,
    PD_NEUTER -> "neutro",
    // FIXME: agreement
    PD_NONE_NOUN -> LEMMA_NINGUNO,
    PD_NONE_DETERMINER -> LEMMA_NINGUN,
    PD_NOTHING -> "nada",
    PD_NOWHERE -> "en ninguna parte",
    PD_OF -> LEMMA_DE,
    PD_ON -> "encima de",
    // FIXME: agreement
    PD_ONE -> LEMMA_UNO,
    // FIXME:  change to LEMMA_U before a vowel
    PD_OR -> LEMMA_O,
    // FIXME: agreement
    PD_OTHER -> LEMMA_OTRO,
    PD_OTHERWISE -> "contrario",
    PD_OUTSIDE -> "fuera de",
    PD_OVER -> "sobre",
    PD_RIGHT -> "al derecho de",
    PD_SAME -> "mismo",
    PD_TO -> LEMMA_A,
    PD_DATIVE_TO -> LEMMA_ADPOSITION_DATIVE,
    // FIXME: the real thing
    PD_THAT -> LEMMA_ESO,
    PD_THEN -> "entonces",
    PD_SUBSEQUENTLY -> "posteriormente",
    PD_WHAT -> LEMMA_QUE_ACCENTED,
    // FIXME need to discriminate cuando from cuándo
    PD_WHEN -> "cuándo",
    PD_WHENEVER -> "cada vez que",
    // FIXME need to discriminate donde from dónde
    PD_WHERE -> LEMMA_DONDE_ACCENTED,
    // FIXME need to deal with agreement
    PD_WHICH -> LEMMA_CUAL_ACCENTED,
    // FIXME need to deal with agreement
    PD_WHO -> LEMMA_QUIEN_ACCENTED,
    // FIXME how is this supposed to work?
    PD_WHOM -> "a quién",
    // FIXME how is this supposed to work?
    PD_WHOSE -> "de quién",
    PD_WITH -> "con",
    PD_WITHIN -> "a menos de"
  )

  // note that this mapping may not be one-to-one; in case of collisions,
  // it's necessary to sort them out via special cases in the
  // tongue's predefForLemma implementation
  val lemmaToPredef = predefToLemma.map(_.swap)
}
