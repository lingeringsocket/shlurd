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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.data._

import scala.collection._

import SprPennTreebankLabels._

object SnlSpanishLemmas
{
  val LEMMA_ACA = "acá"
  val LEMMA_ADONDE_ACCENTED = "adónde"
  val LEMMA_ADONDE = "adónde"
  val LEMMA_AHI = "ahí"
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
  val LEMMA_VOSOTRAS = "vosotras"
  val LEMMA_VOSOTROS = "vosotros"
  val LEMMA_VUESTRA = "vuestra"
  val LEMMA_VUESTRAS = "vuestras"
  val LEMMA_VUESTRO = "vuestro"
  val LEMMA_VUESTROS = "vuestros"
  val LEMMA_Y = "y"
  val LEMMA_YO = "yo"
}

case class SprPronounCoord(
  person : SilPerson,
  gender : SilGender,
  count : SilCount,
  proximity : SilProximity,
  inflection : SilInflection,
  possesseeCount : SilCount = COUNT_SINGULAR
)

object SnlSpanishLexicon
{
  import SprLexicon._
  import SnlSpanishLemmas._

  val prepositions = readLexicon("/spanish/prepositions.txt")

  val subordinates = readLexicon("/spanish/subordinates.txt")

  // FIXME
  val proper = readLexicon("/english/proper.txt")

  val stopList = Set(
    "ella"
  ) ++ stopListPunct

  val unaccentedVowels = "aeiou"

  val accentedVowels = "áéíóú"

  val nominativeToCoord = Map(
    LEMMA_YO -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_TU_ACCENTED -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_EL_ACCENTED -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLO -> SprPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_NOSOTROS -> SprPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_NOSOTRAS -> SprPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_VOSOTROS -> SprPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_VOSOTRAS -> SprPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLOS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE),
    LEMMA_ELLAS -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_NOMINATIVE)
  )

  val accusativeToCoord = Map(
    LEMMA_ME -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_TE -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LO -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_NOS -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_OS -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LOS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE),
    LEMMA_LAS -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ACCUSATIVE)
  )

  val adpositionedToCoord = Map(
    LEMMA_MI_ACCENTED -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED),
    LEMMA_TI -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED),
    LEMMA_LE -> SprPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED),
    LEMMA_LES -> SprPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_ADPOSITIONED)
  )

  val reflexiveToCoord = Map(
    LEMMA_SE -> SprPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_REFLEXIVE, INFLECT_NONE)
  )

  val genitiveToCoord = Map(
    LEMMA_MI -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_MIS -> SprPronounCoord(
      PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_TU -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_TUS -> SprPronounCoord(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_SU -> SprPronounCoord(
      PERSON_THIRD, GENDER_SOMEONE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_SUS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_NUESTRO -> SprPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_NUESTROS -> SprPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_NUESTRA -> SprPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_NUESTRAS -> SprPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_VUESTRO -> SprPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_VUESTROS -> SprPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL),
    LEMMA_VUESTRA -> SprPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE),
    LEMMA_VUESTRAS -> SprPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_ENTITY, INFLECT_GENITIVE, COUNT_PLURAL)
  )

  val possesseeToCoord = Map(
    LEMMA_MIO -> SprPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_MIA -> SprPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_MIOS -> SprPronounCoord(
      PERSON_FIRST, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_MIAS -> SprPronounCoord(
      PERSON_FIRST, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_TUYO -> SprPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_TUYA -> SprPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_TUYOS -> SprPronounCoord(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_TUYAS -> SprPronounCoord(
      PERSON_SECOND, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_SUYO -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_SUYA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE),
    LEMMA_SUYOS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL),
    LEMMA_SUYAS -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_POSSESSEE, INFLECT_NOMINATIVE, COUNT_PLURAL)
  )

  val demonstrativeToCoord = Map(
    LEMMA_ESTO -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTOS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESTAS -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_SPEAKER_HERE, INFLECT_NOMINATIVE),
    LEMMA_ESO -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESO -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_ESA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_LISTENER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUEL -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLA -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLO -> SprPronounCoord(
      PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLOS -> SprPronounCoord(
      PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE),
    LEMMA_AQUELLAS -> SprPronounCoord(
      PERSON_THIRD, GENDER_FEMININE, COUNT_PLURAL,
      PROXIMITY_OVER_THERE, INFLECT_NOMINATIVE)
  )

  val pronounToCoord = (
    nominativeToCoord ++ accusativeToCoord ++ adpositionedToCoord ++
      reflexiveToCoord ++ genitiveToCoord ++ possesseeToCoord ++
      demonstrativeToCoord)

  val coordToPronoun = pronounToCoord.map(_.swap)
  assert(coordToPronoun.size == pronounToCoord.size)

  val pronounLemmas = pronounToCoord.keySet

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
    PD_AT -> "hacia",
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
    PD_TO -> "a",
    // FIXME: the real thing
    PD_THAT -> LEMMA_ESO,
    PD_THEN -> "entonces",
    PD_SUBSEQUENTLY -> "posteriormente",
    PD_WHAT -> LEMMA_QUE_ACCENTED,
    // FIXME need to discriminate cuando from cuándo
    PD_WHEN -> "cuándo",
    PD_WHENEVER -> "cada vez que",
    // FIXME need to discriminate donde from dónde
    PD_WHERE -> "donde",
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

class SnlSpanishTongue(wordnet : SprWordnet)
    extends SprTongue(wordnet)
{
  import SnlSpanishLemmas._
  import SnlSpanishLexicon._
  import SprWordnetScorer._

  private implicit val tongue = this

  private val phraseScorers = Seq(
    scoreSpecialSpanishAdpositions,
    scoreSpanishUsage
  )

  override def newSentencePrinter(genderAnalyzer : SilGenderAnalyzer) =
  {
    new SilSentencePrinter(this, genderAnalyzer)
  }

  def newSentenceBundle() : SilSentenceBundle =
  {
    new SnlSpanishSentenceBundle(this)
  }

  override def newSyntaxAnalyzer(
    context : SprContext,
    guessedQuestion : Boolean,
    strictness : SprStrictness = SPR_STRICTNESS_LOOSE,
    enforceTransitive : Boolean = true
  ) : SprSyntaxAnalyzer =
  {
    new SnlSpanishSyntaxAnalyzer(
      context, strictness, enforceTransitive)
  }

  override def getPhraseScorers : Seq[SprWordnetScorer.PhraseScorer] =
  {
    phraseScorers
  }

  override def getStopList = stopList

  override def getAdjectivePosition = MOD_AFTER_DEFAULT

  override def allowDoubleNegatives : Boolean = true

  override def allowElidedSubject : Boolean = true

  override def combinePolarities(
    truthBoolean : Boolean,
    negateCollection : Boolean,
    subjectVariable : Boolean) : Boolean =
  {
    if (subjectVariable) {
      super.combinePolarities(
        truthBoolean, negateCollection, subjectVariable)
    } else {
      truthBoolean
    }
  }

  override def getRelPredefLemma(predef : SprRelationshipPredef) : String =
  {
    predef match {
      case REL_PREDEF_IDENTITY => LEMMA_SER
      // FIXME this is just wrong
      case REL_PREDEF_BECOME => LEMMA_HACER
      case REL_PREDEF_ASSOC => LEMMA_TENER
    }
  }

  override def getStatePredefLemma(predef : SprStatePredef) : String =
  {
    predef match {
      // FIXME this should depend on the nature of the state
      // (condition vs disposition vs existence)
      case STATE_PREDEF_BE => LEMMA_ESTAR
      // FIXME this is just wrong
      case STATE_PREDEF_BECOME => LEMMA_HACER
    }
  }

  override def getStatePredefFromLemma(lemma : String) : SprStatePredef =
  {
    lemma match {
      case LEMMA_SER | LEMMA_EXISTIR |
          LEMMA_ESTAR | LEMMA_HABER => STATE_PREDEF_BE
      case LEMMA_HACER => STATE_PREDEF_BECOME
      case _ => throw new IllegalArgumentException(
        "Non-predef state verb " + lemma)
    }
  }

  override def isProgressiveAuxLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_ESTAR
  }

  override def isBeingLemma(lemma : String) : Boolean =
  {
    // FIXME this is just wrong for LEMMA_HACER
    lemma match {
      case LEMMA_SER | LEMMA_ESTAR | LEMMA_EXISTIR |
          LEMMA_HACER | LEMMA_HABER => true
      case _ => false
    }
  }

  override def isRelationshipLemma(lemma : String) : Boolean =
  {
    // FIXME this is just wrong for LEMMA_HACER
    lemma match {
      case LEMMA_SER | LEMMA_TENER | LEMMA_HACER => true
      case _ => false
    }
  }

  override def isPossessionLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_TENER
  }

  override def isExistsLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_EXISTIR || lemma == LEMMA_HABER
  }

  override def isImpersonalVerbLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_HABER
  }

  override def isPotentialGerund(inflected : String) : Boolean =
  {
    isProgressive(inflected)
  }

  override def getPronounMap(
    gender : SilBasicGender,
    count : SilCount
  ) : SilPronounMap =
  {
    // FIXME for possessive pronoun adjectives, need to factor in
    // the count of the noun too
    // FIXME reflexives
    // FIXME distinguish direct object from indirect object
    tupleN((gender, count)) match {
      case (GENDER_MASCULINE, COUNT_SINGULAR) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_EL_ACCENTED),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LE),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case (GENDER_MASCULINE, COUNT_PLURAL) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_ELLOS),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LES),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case (GENDER_FEMININE, COUNT_SINGULAR) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_ELLA),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LE),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case (GENDER_FEMININE, COUNT_PLURAL) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_ELLAS),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LES),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case (GENDER_NEUTER, COUNT_SINGULAR) => {
        Map(
          // FIXME should typically be omitted entirely instead
          // via PROXIMITY_ELIDED
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_ELLO),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LE),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case (GENDER_NEUTER, COUNT_PLURAL) => {
        Map(
          SilPronounKey(LABEL_PRP, PERSON_THIRD) ->
            SilWord(LEMMA_ELLOS),
          SilPronounKey(LABEL_PRP_OBJ, PERSON_THIRD) ->
            SilWord(LEMMA_LES),
          SilPronounKey(LABEL_PRP_REFLEXIVE, PERSON_THIRD) ->
            SilWord(LEMMA_SE),
          SilPronounKey(LABEL_PRP_POS, PERSON_THIRD) ->
            SilWord(LEMMA_SU)
        )
      }
      case _ => SilPronounMap()
    }
  }

  override def combineGenders(gendersSeq : Seq[SilGender]) : SilGender =
  {
    val genders = gendersSeq.toSet
    if (genders.isEmpty) {
      GENDER_NEUTER
    } else if (genders.size == 1) {
      genders.head
    } else {
      val firstMasc = genders.find(
        g => (g.maybeBasic == Some(GENDER_MASCULINE)))
      firstMasc match {
        case Some(g) => g
        case _ => GENDER_NEUTER
      }
    }
  }

  override def maybeDeterminerFor(lemma : String) : Option[SilDeterminer] =
  {
    val matcher : PartialFunction[String, SilDeterminer] = {
      case LEMMA_NINGUN | LEMMA_NINGUNO | LEMMA_NINGUNA
          | LEMMA_NINGUNOS | LEMMA_NINGUNAS | LEMMA_NI => DETERMINER_NONE
      case LEMMA_AMBOS | LEMMA_AMBAS |
          LEMMA_Y | LEMMA_TODO | LEMMA_TODA | LEMMA_TODOS | LEMMA_TODAS |
          LEMMA_CADA  => DETERMINER_ALL
      case LEMMA_UN | LEMMA_UNA | LEMMA_UNAS |
          LEMMA_UNO | LEMMA_UNOS => DETERMINER_NONSPECIFIC
      case LEMMA_EL | LEMMA_LA | LEMMA_LO | LEMMA_LOS | LEMMA_LAS =>
        DETERMINER_DEFINITE
      case LEMMA_ALGUN | LEMMA_ALGUNO | LEMMA_ALGUNA |
          LEMMA_ALGUNOS | LEMMA_ALGUNAS => DETERMINER_SOME
      case LEMMA_O | LEMMA_U => DETERMINER_ANY
      case LEMMA_CUAL_ACCENTED | LEMMA_CUALES_ACCENTED =>
        DETERMINER_VARIABLE
    }
    matcher.lift(lemma)
  }

  override def isCoordinatingDeterminer(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_NI | LEMMA_O | LEMMA_U |
          LEMMA_AMBOS | LEMMA_AMBAS => true
      case _ => false
    }
  }

  override def isCoordinatingConjunction(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_Y | LEMMA_O | LEMMA_U | LEMMA_NI => true
      case _ => false
    }
  }

  override def isDemonstrative(lemma : String) =
  {
    demonstrativeToCoord.contains(lemma)
  }

  override def isModalAuxLemma(lemma : String) : Boolean =
  {
    // FIXME all the modals
    lemma match {
      case LEMMA_DEBER | LEMMA_TENER | LEMMA_PODER => true
      case _ => false
    }
  }

  override def tamForAuxLemma(
    auxLemma : String, verbLemma : String) : SilTam =
  {
    val tam = SilTam.indicative
    // FIXME all the modals
    auxLemma match {
      case LEMMA_DEBER => tam.withModality(MODAL_SHOULD)
      case LEMMA_TENER => tam.withModality(MODAL_MUST)
      case LEMMA_PODER => {
        verbLemma match {
          case LEMMA_SER | LEMMA_ESTAR => tam.withModality(MODAL_MAY)
          case _ => tam.withModality(MODAL_CAPABLE)
        }
      }
      case LEMMA_ESTAR => tam.progressive
      case _ => tam
    }
  }

  override def isFlexiblePronoun(token : String) : Boolean =
  {
    token match {
      case LEMMA_ESTO | LEMMA_ESTA |
          LEMMA_ESTOS | LEMMA_ESTAS |
          LEMMA_ESO | LEMMA_ESA |
          LEMMA_ESOS | LEMMA_ESAS |
          LEMMA_AQUEL | LEMMA_AQUELLO | LEMMA_AQUELLA |
          LEMMA_AQUELLOS | LEMMA_AQUELLAS => true
      case _ => false
    }
  }

  override def isReflexivePronoun(token : String) : Boolean =
  {
    // FIXME handle other forms such as "si mismo"
    token match {
      case LEMMA_SE | LEMMA_OS | LEMMA_NOS => true
      case _ => false
    }
  }

  override def isPossessiveAdjective(lemma : String) : Boolean =
  {
    genitiveToCoord.contains(lemma)
  }

  override def isAdpositionablePronoun(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_MI_ACCENTED | LEMMA_TI | LEMMA_EL_ACCENTED |
          LEMMA_ELLA | LEMMA_NOSOTROS | LEMMA_NOSOTRAS | LEMMA_VOSOTROS |
          LEMMA_VOSOTRAS | LEMMA_ELLOS | LEMMA_ELLAS => true
      case _ => {
        if (isPronounWord(lemma)) {
          false
        } else {
          true
        }
      }
    }
  }

  override def isAdposition(lemma : String) : Boolean =
  {
    prepositions.contains(lemma)
  }

  override def adpositionForAux(auxLemma : String) : String =
  {
    // FIXME all of 'em
    auxLemma match {
      case LEMMA_DEBER => LEMMA_DE
      case LEMMA_TENER => LEMMA_QUE
      case LEMMA_PODER => ""
      case _ => ""
    }
  }

  override def auxVerbForModal(modality : SilModality) : String =
  {
    // FIXME all of 'em
    modality match {
      case MODAL_SHOULD => LEMMA_DEBER
      case MODAL_MUST => LEMMA_TENER
      case MODAL_CAPABLE => LEMMA_PODER
      // FIXME enforce MODAL_MAY being paired only with ser/estar
      case MODAL_MAY => LEMMA_PODER
      case _ => throw new IllegalArgumentException("whoops")
    }
  }

  override def isSubordinatingConjunction(lemma : String) : Boolean =
  {
    subordinates.contains(lemma)
  }

  override def isProper(lemma : String) : Boolean =
  {
    proper.contains(lemma)
  }

  override def getPronounLemmas() : Set[String] =
  {
    pronounLemmas
  }

  override def analyzePronoun(lemma : String) =
  {
    pronounToCoord.get(lemma) match {
      case Some(coord) => {
        tupleN((coord.person, coord.count, coord.gender, coord.inflection,
          Some(coord.proximity), coord.possesseeCount))
      }
      case _ => {
        val isCustomPronoun = !isPronounWord(lemma)
        val gender = if (isCustomPronoun) {
          GENDER_SOMEONE
        } else {
          // FIXME what we really want here is an unknown between
          // NEUTER and SOMEONE, to be resolved downstream
          GENDER_NEUTER
        }
        tupleN((PERSON_THIRD, COUNT_SINGULAR, gender, INFLECT_NONE,
          None, COUNT_SINGULAR))
      }
    }
  }

  override def synthesizeMembersRef(
    annotator : SilAnnotator,
    determiner : SilDeterminer,
    gender : SilGender) : SilReference =
  {
    val lemma = determiner match {
      case DETERMINER_NONE => LEMMA_NINGUNOS
      case SilIntegerDeterminer(1) | DETERMINER_NONSPECIFIC => LEMMA_UNO
      case DETERMINER_ANY => LEMMA_ALGUNOS
      case DETERMINER_SOME => LEMMA_UNOS
      case DETERMINER_ALL => LEMMA_TODOS
      case SilIntegerDeterminer(n) => n.toString
      case _ => throw new IllegalArgumentException(determiner.toString)
    }
    val (_, count, _, _, _, _) = analyzePronoun(lemma)
    annotator.nounRef(SilWord(correctGenderCount(lemma, gender, count, false)))
  }

  override def synthesizeSummaryRef(
    annotator : SilAnnotator,
    determiner : SilDeterminer,
    summarizedRef : SilReference,
    gender : SilGender,
    genderAnalyzer : SilGenderAnalyzer
  ) : SilReference =
  {
    determiner match {
      case DETERMINER_ALL => {
        // "todas las tres"
        annotator.determinedRef(
          annotator.determinedRef(
            summarizedRef,
            DETERMINER_DEFINITE),
          determiner
        )
      }
      case _ => {
        summarizedRef match {
          // "ambos"
          case SilNounReference(SprPredefWord(PD_BOTH)) => {
            summarizedRef
          }
          // "tres de ellas"
          case _ => {
            super.synthesizeSummaryRef(
              annotator, determiner, summarizedRef,
              gender, genderAnalyzer)
          }
        }
      }
    }
  }

  override def deriveGender(word : SilWord) : SilGender =
  {
    val glosses = wordnet.getNounSenses(word.toNounLemma).
      flatMap(sense => {
        wordnet.getGlossDefinitions(sense)
      })
    glosses.headOption.flatMap(_ match {
      case "m" => Some(GENDER_MASCULINE)
      case "f" => Some(GENDER_FEMININE)
      case _ => None
    }).getOrElse {
      val lemma = word.toNounLemma
      if (lemma.endsWith("o") || lemma.endsWith("os")) {
        GENDER_MASCULINE
      } else {
        GENDER_FEMININE
      }
    }
  }

  override def deriveGender(
    ref : SilReference,
    subAnalyzer : SilGenderAnalyzer) : SilGender =
  {
    ref match {
      case pr : SilPronounReference => {
        pr.gender
      }
      case SilNounReference(noun) => {
        subAnalyzer.deriveGender(noun)
      }
      case SilConjunctiveReference(determiner, references, _) => {
        val subGenders = references.map(r => SilUtils.getGender(r, subAnalyzer))
        combineGenders(subGenders)
      }
      case SilParenthesizedReference(r, _) => {
        SilUtils.getGender(r, subAnalyzer)
      }
      case SilAppositionalReference(r, _) => {
        SilUtils.getGender(r, subAnalyzer)
      }
      case SilStateSpecifiedReference(r, _) => {
        SilUtils.getGender(r, subAnalyzer)
      }
      case SilDeterminedReference(r, _) => {
        SilUtils.getGender(r, this)
      }
      case SilGenitiveReference(_, possessee) => {
        SilUtils.getGender(possessee, subAnalyzer)
      }
      case _ : SilQuotationReference => {
        GENDER_NEUTER
      }
      case _ : SilUnknownReference => {
        GENDER_NEUTER
      }
    }
  }

  private def isProgressive(inflected : String) : Boolean =
  {
    inflected.endsWith("ando") || inflected.endsWith("iendo") ||
      inflected.endsWith("yendo")
  }

  private def isParticiple(inflected : String) : Boolean =
  {
    // FIXME irregulars such as roto
    inflected.endsWith("ado") || inflected.endsWith("ido")
  }

  override def labelVerb(token : String, lemma : String) : Set[String] =
  {
    // FIXME all the tams
    if (isProgressive(token)) {
      Set(LABEL_VBG)
    } else if (isParticiple(token)) {
      Set(LABEL_VBN)
    } else {
      SnlSpanishConjugation.getConjugationCoord(
        lemma, token).map(_.tense) match
      {
        case Some(TENSE_PAST) => Set(LABEL_VBD)
        case Some(TENSE_FUTURE) => Set(LABEL_VBF)
        case _ => Set(LABEL_VB)
      }
    }
  }

  override def labelSpecial(
    word : String,
    token : String) : Set[SprSyntaxTree] =
  {
    def leafUninflected = makeLeaf(word, token)
    def leafInflected(lemma : String) = makeLeaf(word, token, lemma)
    token match {
      case LEMMA_QUE | LEMMA_QUIEN | LEMMA_DONDE | LEMMA_ADONDE => {
        Set(SptIN(leafUninflected))
      }
      case LEMMA_QUIENES => {
        Set(SptIN(leafInflected(LEMMA_QUIEN)))
      }
      case LEMMA_QUIEN_ACCENTED =>
        Set(SptWP(leafUninflected))
      case LEMMA_QUIENES_ACCENTED =>
        Set(SptWP(leafInflected(LEMMA_QUIEN_ACCENTED)))
      case LEMMA_COMO_ACCENTED | LEMMA_DONDE_ACCENTED | LEMMA_ADONDE_ACCENTED |
          LEMMA_CUANTOS_ACCENTED =>
        {
          Set(SptWRB(leafUninflected))
        }
      case LEMMA_CUANTAS_ACCENTED => {
          Set(SptWRB(leafInflected(LEMMA_CUANTOS_ACCENTED)))
      }
      case LEMMA_QUE_ACCENTED | LEMMA_CUAL_ACCENTED => {
        Set(SptWP(leafUninflected),
          SptWDT(leafUninflected))
      }
      case LEMMA_CUALES_ACCENTED => {
        Set(SptWP(leafInflected(LEMMA_CUAL_ACCENTED)),
          SptWDT(leafInflected(LEMMA_CUAL_ACCENTED)))
      }
      case LEMMA_NO => {
        Set(SptRB(leafUninflected))
      }
      case _ => {
        Set.empty
      }
    }
  }

  override def analyzeVerbConjugation(word : SilWord)
      : (SilPerson, SilCount, SilGender, SilTam) =
  {
    val simple = word match {
      case sw : SilSimpleWord => sw
      case cw : SilCompoundWord => cw.components.last
    }
    if (isProgressive(simple.inflected)) {
      tupleN((
        PERSON_FIRST,
        COUNT_SINGULAR,
        GENDER_SOMEONE,
        SilTam.indicative.progressive
      ))
    } else {
      val coordOpt = SnlSpanishConjugation.getConjugationCoord(
        simple.lemma, simple.inflected)
      coordOpt match {
        case Some(coord) => {
          tupleN((
            coord.person,
            coord.count,
            if (coord.person == PERSON_THIRD) GENDER_NEUTER else GENDER_SOMEONE,
            SilTam.indicative.withTense(coord.tense).withMood(coord.mood)
          ))
        }
        case _ => {
          // unrecognized conjugation, just make something up
          tupleN((
            PERSON_THIRD,
            COUNT_SINGULAR,
            GENDER_MASCULINE,
            SilTam.indicative
          ))
        }
      }
    }
  }

  // FIXME generalize pronoun coordinates to cover determiners,
  // and use map instead
  override def correctGenderCount(
    lemma : String, gender : SilGender, count : SilCount,
    isModifier : Boolean) : String =
  {
    val basic = gender.maybeBasic match {
      case Some(GENDER_MASCULINE) | Some(GENDER_SOMEONE) => GENDER_MASCULINE
      case Some(GENDER_FEMININE) => GENDER_FEMININE
      case _ => GENDER_NEUTER
    }
    // FIXME refinements for COUNT_MASS and COUNT_ZERO_PLURAL
    ignoreGenderCount(lemma) match {
      case LEMMA_ALGUN => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_ALGUNA
        case (GENDER_FEMININE, _) => LEMMA_ALGUNAS
        case (_, COUNT_SINGULAR) => {
          if (isModifier) {
            LEMMA_ALGUN
          } else {
            LEMMA_ALGUNO
          }
        }
        case (_, _) => LEMMA_ALGUNOS
      }
      case LEMMA_NINGUN => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_NINGUNA
        case (GENDER_FEMININE, _) => LEMMA_NINGUNAS
        case (_, COUNT_SINGULAR) => {
          if (isModifier) {
            LEMMA_NINGUN
          } else {
            LEMMA_NINGUNO
          }
        }
        case (_, _) => LEMMA_NINGUNOS
      }
      case LEMMA_AMBOS => basic match {
        case GENDER_FEMININE => LEMMA_AMBAS
        case _ => LEMMA_AMBOS
      }
      case LEMMA_AQUEL => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_AQUELLA
        case (GENDER_FEMININE, _) => LEMMA_AQUELLAS
        case (_, COUNT_SINGULAR) => {
          if (isModifier) {
            LEMMA_AQUEL
          } else {
            LEMMA_AQUELLO
          }
        }
        case (_, _) => LEMMA_AQUELLOS
      }
      case LEMMA_CUAL => count match {
        case COUNT_SINGULAR => LEMMA_CUAL
        case _ => LEMMA_CUALES
      }
      case LEMMA_CUAL_ACCENTED => count match {
        case COUNT_SINGULAR => LEMMA_CUAL_ACCENTED
        case _ => LEMMA_CUALES_ACCENTED
      }
      case LEMMA_QUIEN => count match {
        case COUNT_SINGULAR => LEMMA_QUIEN
        case _ => LEMMA_QUIENES
      }
      case LEMMA_QUIEN_ACCENTED => count match {
        case COUNT_SINGULAR => LEMMA_QUIEN_ACCENTED
        case _ => LEMMA_QUIENES_ACCENTED
      }
      case LEMMA_ELLO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_ELLA
        case (GENDER_FEMININE, _) => LEMMA_ELLAS
        case (GENDER_MASCULINE, COUNT_SINGULAR) => LEMMA_EL_ACCENTED
        case (_, COUNT_SINGULAR) => LEMMA_ELLO
        case (_, _) => LEMMA_ELLOS
      }
      case LEMMA_LE => count match {
        case COUNT_SINGULAR => LEMMA_LE
        case _ => LEMMA_LES
      }
      case LEMMA_EL => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_LA
        case (GENDER_FEMININE, _) => LEMMA_LAS
        case (_, COUNT_SINGULAR) => LEMMA_EL
        case (_, _) => LEMMA_LOS
      }
      case LEMMA_NOSOTROS => basic match {
        case GENDER_FEMININE => LEMMA_NOSOTRAS
        case _ => LEMMA_NOSOTROS
      }
      case LEMMA_VOSOTROS => basic match {
        case GENDER_FEMININE => LEMMA_VOSOTRAS
        case _ => LEMMA_VOSOTROS
      }
      case LEMMA_SU => count match {
        case COUNT_SINGULAR => LEMMA_SU
        case _ => LEMMA_SUS
      }
      case LEMMA_TU => count match {
        case COUNT_SINGULAR => LEMMA_TU
        case _ => LEMMA_TUS
      }
      case LEMMA_UN => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_UNA
        case (GENDER_FEMININE, _) => LEMMA_UNAS
        case (_, COUNT_SINGULAR) => {
          if (isModifier) {
            LEMMA_UN
          } else {
            LEMMA_UNO
          }
        }
        case (_, _) => LEMMA_UNOS
      }
      case lemma => {
        if (lemma.endsWith("o")) {
          val suffix = tupleN((basic, count)) match {
            case (GENDER_FEMININE, COUNT_SINGULAR) => "a"
            case (GENDER_FEMININE, _) => "as"
            case (_, COUNT_SINGULAR) => "o"
            case (_, _) => "os"
          }
          lemma.stripSuffix("o") + suffix
        } else {
          count match {
            case COUNT_SINGULAR => {
              lemma
            }
            case _ => {
              // FIXME should be pluralizeAdjective
              pluralizeNoun(lemma)
            }
          }
        }
      }
    }
  }

  private def ignoreGenderCount(lemma : String) : String =
  {
    lemma match {
      case LEMMA_ALGUNO | LEMMA_ALGUNA |
          LEMMA_ALGUNOS | LEMMA_ALGUNAS => LEMMA_ALGUN
      case LEMMA_AMBOS | LEMMA_AMBAS => LEMMA_AMBOS
      case LEMMA_AQUELLA | LEMMA_AQUELLO |
          LEMMA_AQUELLAS | LEMMA_AQUELLOS => LEMMA_AQUEL
      case LEMMA_CUALES => LEMMA_CUAL
      case LEMMA_CUALES_ACCENTED => LEMMA_CUAL_ACCENTED
      case LEMMA_CUANTOS_ACCENTED | LEMMA_CUANTAS_ACCENTED =>
        LEMMA_CUANTOS_ACCENTED
      case LEMMA_QUIENES => LEMMA_QUIEN
      case LEMMA_QUIENES_ACCENTED => LEMMA_QUIEN_ACCENTED
      case LEMMA_EL_ACCENTED | LEMMA_ELLA |
          LEMMA_ELLOS | LEMMA_ELLAS => LEMMA_ELLO
      case LEMMA_LES => LEMMA_LE
      case LEMMA_LOS | LEMMA_LA | LEMMA_LAS => LEMMA_EL
      case LEMMA_NOSOTROS | LEMMA_NOSOTRAS => LEMMA_NOSOTROS
      case LEMMA_VOSOTROS | LEMMA_VOSOTRAS => LEMMA_VOSOTROS
      case LEMMA_SUS => LEMMA_SU
      case LEMMA_TUS => LEMMA_TU
      case LEMMA_UNO | LEMMA_UNA | LEMMA_UNOS | LEMMA_UNAS => LEMMA_UN
      case _ => {
        if (lemma.endsWith("a")) {
          lemma.stripSuffix("a") + "o"
        } else if (lemma.endsWith("as")) {
          lemma.stripSuffix("as") + "o"
        } else if (lemma.endsWith("os")) {
          lemma.stripSuffix("s")
        } else if (lemma.endsWith("es")) {
          lemma.stripSuffix("s")
        } else {
          lemma
        }
      }
    }
  }

  override def pronounLemma(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity,
    inflection : SilInflection,
    possesseeCount : SilCount = COUNT_SINGULAR
  ) : String =
  {
    val coord = SprPronounCoord(
      person, gender, count, proximity, inflection, possesseeCount)
    coordToPronoun.get(coord).getOrElse {
      inflection match {
        case INFLECT_ADPOSITIONED => {
          pronounLemma(
            person, gender, count, proximity,
            INFLECT_NOMINATIVE, possesseeCount)
        }
        case _ => ""
      }
    }
  }

  override def predefLemma(predef : SprPredef) : String =
  {
    predefToLemma(predef)
  }

  def predefForLemma(
    lemma : String,
    label : String = LABEL_AMBIGUOUS
  ) : Option[SprPredef] =
  {
    lemma match {
      case LEMMA_NINGUNO | LEMMA_NINGUNA => Some(PD_NONE_NOUN)
      case LEMMA_OTRO => Some(label match {
        case LABEL_DT => PD_ANOTHER
        case _ => PD_OTHER
      })
      case LEMMA_NI => Some(label match {
        case LABEL_DT => PD_NEITHER_DETERMINER
        case _ => PD_NOR
      })
      case _ => lemmaToPredef.get(ignoreGenderCount(lemma))
    }
  }

  override def proximityLemma(proximity : SilProximity) : String =
  {
    proximity match {
      case PROXIMITY_SPEAKER_HERE => LEMMA_AQUI
      case PROXIMITY_AROUND_HERE => LEMMA_ACA
      case PROXIMITY_LISTENER_THERE => LEMMA_AHI
      case PROXIMITY_OVER_THERE => LEMMA_ALLI
      case PROXIMITY_WAY_OVER_THERE => LEMMA_ALLA
      case _ => ""
    }
  }

  override def proximityForLemma(lemma : String) : Option[SilProximity] =
  {
    lemma match {
      case LEMMA_ACA => Some(PROXIMITY_AROUND_HERE)
      case LEMMA_AQUI => Some(PROXIMITY_SPEAKER_HERE)
      case LEMMA_AHI => Some(PROXIMITY_LISTENER_THERE)
      case LEMMA_ALLI => Some(PROXIMITY_OVER_THERE)
      case LEMMA_ALLA => Some(PROXIMITY_WAY_OVER_THERE)
      case _ => None
    }
  }

  override def pluralizeNoun(lemma : String) : String =
  {
    val last = lemma.last
    if (unaccentedVowels.contains(last)) {
      lemma + "s"
    } else if (accentedVowels.contains(last)) {
      val i = accentedVowels.indexOf(last)
      if ((i == 2) || (i == 4)) {
        // í or ú
        lemma + "es"
      } else {
        // á, é, or ó
        lemma + "s"
      }
    } else if (lemma.endsWith("ión")) {
      lemma.stripSuffix("ión") + "iones"
    } else if (lemma.endsWith("z")) {
      lemma.stripSuffix("z") + "ces"
    } else if (lemma.endsWith("g")) {
      lemma + "ues"
    } else if (lemma.endsWith("c")) {
      lemma.stripSuffix("c") + "ques"
    } else if (lemma.endsWith("s") || lemma.endsWith("x")) {
      val vowel = lemma.takeRight(2).head
      val i = accentedVowels.indexOf(vowel)
      if (i == -1) {
        lemma
      } else {
        lemma.dropRight(2) + unaccentedVowels(i) + "ses"
      }
    } else {
      lemma match {
        case "carácter" => "caracteres"
        case "espécimen" => "especímenes"
        case "régimen" => "regímenes"
        case _ => lemma + "es"
      }
    }
  }

  override def getNoneCount : SilCount = COUNT_SINGULAR

  override protected def getMatcherResource() =
    "/spanish/phrase-structure.txt"

  private def scoreSpecialSpanishAdpositions = phraseScorer {
    case ap : SilAdpositionalPhrase => {
      val words = ap.adposition.word.decomposed
      if (words.exists(_.lemma == PD_ADVERBIAL_TMP.toLemma)) {
        SilPhraseScore.proBig
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  private def scoreSpanishUsage = phraseScorer {
    case SilNounReference(noun) => {
      usageScore(noun.toNounLemma, POS.NOUN)
    }
    case SilPropertyState(sw : SilSimpleWord) => {
      usageScore(sw.toLemma, POS.ADJECTIVE)
    }
    case SilActionPredicate(_, sw : SilSimpleWord, _, _) => {
      usageScore(sw.toLemma, POS.VERB)
    }
    case SilBasicVerbModifier(sw : SilSimpleWord) => {
      usageScore(sw.toLemma, POS.ADVERB)
    }
  }
}
