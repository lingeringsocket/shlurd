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
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

import SprPennTreebankLabels._

object SprSpanishLexicon
{
  import SprLexicon._

  val prepositions = readLexicon("/spanish/prepositions.txt")

  val subordinates = readLexicon("/spanish/subordinates.txt")

  // FIXME
  val proper = readLexicon("/english/proper.txt")

  val stopList = Set(
    "ella"
  ) ++ stopListPunct
}

object SprSpanishLemmas
{
  val LEMMA_ALGUN = "algún"
  val LEMMA_ALGUNO = "alguno"
  val LEMMA_ALGUNA = "alguna"
  val LEMMA_ALGUNAS = "algunas"
  val LEMMA_ALGUNOS = "algunos"
  val LEMMA_AMBA = "ambas"
  val LEMMA_AMBAS = "ambas"
  val LEMMA_AMBO = "ambos"
  val LEMMA_AMBOS = "ambos"
  val LEMMA_AQUEL = "esa"
  val LEMMA_AQUELLA = "esa"
  val LEMMA_AQUELLAS = "esas"
  val LEMMA_AQUELLO = "eso"
  val LEMMA_AQUELLOS = "esos"
  val LEMMA_CADA = "cada"
  val LEMMA_CUAL = "cual"
  val LEMMA_CUALES = "cuales"
  val LEMMA_DE = "de"
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
  val LEMMA_HACER = "hacer"
  val LEMMA_LA = "la"
  val LEMMA_LAS = "las"
  val LEMMA_LE = "le"
  val LEMMA_LES = "les"
  val LEMMA_LO = "lo"
  val LEMMA_LOS = "los"
  val LEMMA_ME = "me"
  val LEMMA_MI = "mi"
  val LEMMA_MIA = "mía"
  val LEMMA_MIO = "mío"
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
  val LEMMA_SE = "se"
  val LEMMA_SER = "ser"
  val LEMMA_SU = "su"
  val LEMMA_SUS = "sus"
  val LEMMA_TENER = "tener"
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
  val LEMMA_U = "u"
  val LEMMA_UN = "un"
  val LEMMA_UNA = "una"
  val LEMMA_UNAS = "unas"
  val LEMMA_UNO = "uno"
  val LEMMA_UNOS = "unos"
  val LEMMA_USTED = "usted"
  val LEMMA_USTEDES = "ustedes"
  val LEMMA_VOS = "vos"
  val LEMMA_VOSOTRAS = "vosotras"
  val LEMMA_VOSOTROS = "vosotros"
  val LEMMA_VUESTRA = "vuestra"
  val LEMMA_VUESTRAS = "vuestras"
  val LEMMA_VUESTRO = "vuestro"
  val LEMMA_VUESTROS = "vuestros"
  val LEMMA_Y = "y"
  val LEMMA_YO = "yo"
}

class SprSpanishTongue(wordnet : ShlurdWordnet)
    extends SprTongue(wordnet)
{
  import SprSpanishLemmas._

  override def newSentencePrinter(genderAnalyzer : SilGenderAnalyzer) =
    new SilSentencePrinter(this, SilSpanishParlance, genderAnalyzer)

  override def newSyntaxAnalyzer(
    context : SprContext,
    guessedQuestion : Boolean,
    strictness : SprStrictness = SPR_STRICTNESS_LOOSE,
    enforceTransitive : Boolean = true
  ) : SprSyntaxAnalyzer =
  {
    new SprSpanishSyntaxAnalyzer(
      context, strictness, enforceTransitive)
  }

  override def getStopList = SprSpanishLexicon.stopList

  override def getRelPredefLemma(predef : SilRelationshipPredef) : String =
  {
    predef match {
      case REL_PREDEF_IDENTITY => LEMMA_SER
      // FIXME this is just wrong
      case REL_PREDEF_BECOME => LEMMA_HACER
      case REL_PREDEF_ASSOC => LEMMA_TENER
    }
  }

  override def getStatePredefLemma(predef : SilStatePredef) : String =
  {
    predef match {
      // FIXME this should depend on the nature of the state
      // (condition vs disposition)
      case STATE_PREDEF_BE => LEMMA_ESTAR
      // FIXME this is just wrong
      case STATE_PREDEF_BECOME => LEMMA_HACER
    }
  }

  override def getStatePredefFromLemma(lemma : String) : SilStatePredef =
  {
    lemma match {
      case LEMMA_SER | LEMMA_EXISTIR | LEMMA_ESTAR => STATE_PREDEF_BE
      // FIXME this is just wrong
      case LEMMA_HACER => STATE_PREDEF_BECOME
      case _ => throw new IllegalArgumentException(
        "Non-predef state verb " + lemma)
    }
  }

  override def isBeingLemma(lemma : String) : Boolean =
  {
    // FIXME this is just wrong for LEMMA_HACER
    lemma match {
      case LEMMA_SER | LEMMA_ESTAR | LEMMA_EXISTIR | LEMMA_HACER => true
      case _ => false
    }
  }

  override def isPossessionLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_TENER
  }

  override def isExistsLemma(lemma : String) : Boolean =
  {
    lemma == LEMMA_EXISTIR
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
      case LEMMA_CUAL | LEMMA_CUALES => DETERMINER_VARIABLE
    }
    matcher.lift(lemma)
  }

  override def isCoordinatingDeterminer(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_NINGUN | LEMMA_NINGUNO | LEMMA_NINGUNA
          | LEMMA_NINGUNOS | LEMMA_NINGUNAS |
          LEMMA_AMBOS | LEMMA_AMBAS |
          LEMMA_TODO | LEMMA_TODA | LEMMA_TODOS | LEMMA_TODAS => true
      case _ => false
    }
  }

  override def isCoordinatingConjunction(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_Y | LEMMA_O | LEMMA_O | LEMMA_NI => true
      case _ => false
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
      case LEMMA_SE | LEMMA_OS | LEMMA_VOS | LEMMA_NOS => true
      case _ => false
    }
  }

  override def isPossessiveAdjective(token : String) : Boolean =
  {
    token match {
      case LEMMA_MI | LEMMA_MIS |
          LEMMA_TU | LEMMA_TUS |
          LEMMA_SU | LEMMA_SUS |
          LEMMA_VUESTRA | LEMMA_VUESTRO |
          LEMMA_VUESTRAS | LEMMA_VUESTROS |
          LEMMA_NUESTRA | LEMMA_NUESTRO |
          LEMMA_NUESTRAS | LEMMA_NUESTROS => true
      case _ => false
    }
  }

  override def isAdposition(lemma : String) : Boolean =
  {
    SprSpanishLexicon.prepositions.contains(lemma)
  }

  override def isSubordinatingConjunction(lemma : String) : Boolean =
  {
    SprSpanishLexicon.subordinates.contains(lemma)
  }

  override def isProper(lemma : String) : Boolean =
  {
    SprSpanishLexicon.proper.contains(lemma)
  }

  override def isPronounWord(lemma : String) : Boolean =
  {
    lemma match {
      case LEMMA_YO | LEMMA_ME | LEMMA_NOS | LEMMA_VOS | LEMMA_OS |
          LEMMA_NOSOTROS | LEMMA_NOSOTRAS |
          LEMMA_VOSOTROS | LEMMA_VOSOTRAS |
          LEMMA_MI | LEMMA_MIS |
          LEMMA_NUESTRO | LEMMA_NUESTRA |
          LEMMA_NUESTROS | LEMMA_NUESTRAS |
          LEMMA_VUESTRO | LEMMA_VUESTRA |
          LEMMA_VUESTROS | LEMMA_VUESTRAS |
          LEMMA_MIA | LEMMA_MIO |
          LEMMA_TU_ACCENTED | LEMMA_TU |
          LEMMA_USTED | LEMMA_USTEDES |
          LEMMA_SE |
          LEMMA_SU | LEMMA_SUS |
          LEMMA_TUYA | LEMMA_TUYO |
          LEMMA_TUYAS | LEMMA_TUYOS |
          LEMMA_EL_ACCENTED |
          LEMMA_ELLA | LEMMA_ELLO |
          LEMMA_ELLAS | LEMMA_ELLOS |
          LEMMA_ESTO | LEMMA_ESTA |
          LEMMA_ESTOS | LEMMA_ESTAS |
          LEMMA_ESO | LEMMA_ESA |
          LEMMA_ESOS | LEMMA_ESAS |
          LEMMA_AQUEL | LEMMA_AQUELLO | LEMMA_AQUELLA |
          LEMMA_AQUELLOS | LEMMA_AQUELLAS |
          LEMMA_LO | LEMMA_LE | LEMMA_LES => true
      case _ => false
    }
  }

  override def analyzePronoun(lemma : String) =
  {
    val isCustomPronoun = !isPronounWord(lemma)
    val person = lemma match {
      case LEMMA_YO | LEMMA_ME | LEMMA_NOSOTROS | LEMMA_NOSOTRAS
         | LEMMA_MI | LEMMA_MIA | LEMMA_MIO | LEMMA_MIS | LEMMA_NOS |
          LEMMA_NUESTRO | LEMMA_NUESTRA |
          LEMMA_NUESTROS | LEMMA_NUESTRAS => PERSON_FIRST
      case LEMMA_TU_ACCENTED | LEMMA_TU | LEMMA_TUS |
          LEMMA_TUYA | LEMMA_TUYO | LEMMA_TUYAS | LEMMA_TUYOS |
          LEMMA_VOSOTROS | LEMMA_VOSOTRAS | LEMMA_VOS | LEMMA_OS |
          LEMMA_VUESTRO | LEMMA_VUESTRA |
          LEMMA_VUESTROS | LEMMA_VUESTRAS |
          LEMMA_USTED | LEMMA_USTEDES => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case LEMMA_ALGUNAS | LEMMA_ALGUNOS | LEMMA_AMBAS | LEMMA_AMBOS |
          LEMMA_AQUELLAS | LEMMA_AQUELLOS |
          LEMMA_CUALES | LEMMA_ELLOS | LEMMA_ELLAS |
          LEMMA_ESOS | LEMMA_ESAS | LEMMA_ESTOS | LEMMA_ESTAS |
          LEMMA_LOS | LEMMA_LAS | LEMMA_LES | LEMMA_NOS | LEMMA_VOS |
          LEMMA_MIS | LEMMA_NINGUNAS | LEMMA_NINGUNOS |
          LEMMA_NOSOTROS | LEMMA_NOSOTRAS |
          LEMMA_VOSOTROS | LEMMA_VOSOTRAS |
          LEMMA_NUESTROS | LEMMA_NUESTRAS |
          LEMMA_VUESTROS | LEMMA_VUESTRAS |
          LEMMA_SUS | LEMMA_TODOS | LEMMA_TODAS |
          LEMMA_TUS | LEMMA_TUYAS | LEMMA_TUYOS |
          LEMMA_USTEDES => COUNT_PLURAL
      case _ => COUNT_SINGULAR
    }
    val gender = lemma match {
      case LEMMA_ALGUNO | LEMMA_ALGUNOS |
          LEMMA_AMBOS |
          LEMMA_AQUELLO | LEMMA_AQUELLOS |
          LEMMA_EL | LEMMA_EL_ACCENTED | LEMMA_ELLO | LEMMA_ELLOS |
          LEMMA_ESO | LEMMA_ESOS |
          LEMMA_ESTO | LEMMA_ESTOS |
          LEMMA_LO | LEMMA_LOS |
          LEMMA_NINGUNO | LEMMA_NINGUNOS |
          LEMMA_NOSOTROS | LEMMA_NUESTRO | LEMMA_NUESTROS |
          LEMMA_VOSOTROS | LEMMA_VUESTRO | LEMMA_VUESTROS |
          LEMMA_TODO | LEMMA_TODOS |
          LEMMA_TUYO | LEMMA_TUYOS |
          LEMMA_UNO | LEMMA_UNOS => GENDER_MASCULINE
      case LEMMA_ALGUNA | LEMMA_ALGUNAS |
          LEMMA_AMBAS |
          LEMMA_AQUELLA | LEMMA_AQUELLAS |
          LEMMA_ELLA | LEMMA_ELLAS |
          LEMMA_ESA | LEMMA_ESAS |
          LEMMA_ESTA | LEMMA_ESTAS |
          LEMMA_LA | LEMMA_LAS |
          LEMMA_NINGUNA | LEMMA_NINGUNAS |
          LEMMA_NOSOTRAS | LEMMA_NUESTRA | LEMMA_NUESTRAS |
          LEMMA_VOSOTRAS | LEMMA_VUESTRA | LEMMA_VUESTRAS |
          LEMMA_TODA | LEMMA_TODAS |
          LEMMA_TUYA | LEMMA_TUYAS |
          LEMMA_UNA | LEMMA_UNAS => GENDER_FEMININE
      case _ => {
        person match {
          case PERSON_FIRST | PERSON_SECOND => GENDER_SOMEONE
          case _ => {
            if (isCustomPronoun) {
              GENDER_SOMEONE
            } else {
              // FIXME what we really want here is an unknown between
              // NEUTER and SOMEONE, to be resolved downstream
              GENDER_NEUTER
            }
          }
        }
      }
    }
    val proximityOpt = lemma match {
      case LEMMA_ESTO | LEMMA_ESTOS |
          LEMMA_ESTA | LEMMA_ESTAS => Some(PROXIMITY_HERE)
      case LEMMA_ESO | LEMMA_ESOS |
          LEMMA_ESA | LEMMA_ESAS => Some(PROXIMITY_LISTENER_THERE)
      case LEMMA_AQUEL | LEMMA_AQUELLO | LEMMA_AQUELLOS |
          LEMMA_AQUELLA | LEMMA_AQUELLAS => Some(PROXIMITY_THERE)
      case _ => None
    }
    tupleN((person, count, gender, proximityOpt))
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
    val (_, count, _, _) = analyzePronoun(lemma)
    annotator.nounRef(SilWord(correctGenderCount(lemma, gender, count, false)))
  }

  override def deriveGender(ref : SilReference) : SilGender =
  {
    ref match {
      case pr : SilPronounReference => {
        pr.gender
      }
      case SilNounReference(noun) => {
        // FIXME get this from wordnet instead
        if (noun.toNounLemma.endsWith("o")) {
          GENDER_MASCULINE
        } else {
          GENDER_FEMININE
        }
      }
      case SilConjunctiveReference(determiner, references, _) => {
        combineGenders(references.map(deriveGender))
      }
      case SilParenthesizedReference(reference, _) => {
        deriveGender(reference)
      }
      case SilAppositionalReference(primary, _) => {
        deriveGender(primary)
      }
      case SilStateSpecifiedReference(reference, _) => {
        deriveGender(reference)
      }
      case SilDeterminedReference(reference, _) => {
        deriveGender(reference)
      }
      case SilGenitiveReference(possessor, possessee) => {
        deriveGender(possessee)
      }
      case _ : SilQuotationReference => {
        GENDER_NEUTER
      }
      case _ : SilUnknownReference => {
        GENDER_NEUTER
      }
    }
  }

  override def labelVerb(token : String, lemma : String) : Set[String] =
  {
    // FIXME all the tams
    SilSpanishConjugation.getConjugationCoord(lemma, token).tense match {
      case TENSE_PAST => Set(LABEL_VBD)
      case _ => Set(LABEL_VB)
    }
  }

  override def analyzeVerbConjugation(word : SilWord)
      : (SilPerson, SilCount, SilGender, SilTam) =
  {
    val simple = word match {
      case sw : SilSimpleWord => sw
      case cw : SilCompoundWord => cw.components.last
    }
    val coord = SilSpanishConjugation.getConjugationCoord(
      simple.lemma, simple.inflected)
    tupleN((
      coord.person,
      coord.count,
      GENDER_NEUTER,
      SilTam.indicative.withTense(coord.tense).withMood(coord.mood)
    ))
  }

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
      case LEMMA_ESO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_ESA
        case (GENDER_FEMININE, _) => LEMMA_ESAS
        case (_, COUNT_SINGULAR) => LEMMA_ESO
        case (_, _) => LEMMA_ESOS
      }
      case LEMMA_ESTO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_ESTA
        case (GENDER_FEMININE, _) => LEMMA_ESTAS
        case (_, COUNT_SINGULAR) => LEMMA_ESTO
        case (_, _) => LEMMA_ESTOS
      }
      case LEMMA_NOSOTROS => basic match {
        case GENDER_FEMININE => LEMMA_NOSOTRAS
        case _ => LEMMA_NOSOTROS
      }
      case LEMMA_VOSOTROS => basic match {
        case GENDER_FEMININE => LEMMA_VOSOTRAS
        case _ => LEMMA_VOSOTROS
      }
      case LEMMA_NUESTRO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_NUESTRA
        case (GENDER_FEMININE, _) => LEMMA_NUESTRAS
        case (_, COUNT_SINGULAR) => LEMMA_NUESTRO
        case (_, _) => LEMMA_NUESTROS
      }
      case LEMMA_VUESTRO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_VUESTRA
        case (GENDER_FEMININE, _) => LEMMA_VUESTRAS
        case (_, COUNT_SINGULAR) => LEMMA_VUESTRO
        case (_, _) => LEMMA_VUESTROS
      }
      case LEMMA_SU => count match {
        case COUNT_SINGULAR => LEMMA_SU
        case _ => LEMMA_SUS
      }
      case LEMMA_TODO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_TODA
        case (GENDER_FEMININE, _) => LEMMA_TODAS
        case (_, COUNT_SINGULAR) => LEMMA_TODO
        case (_, _) => LEMMA_TODOS
      }
      case LEMMA_TU => count match {
        case COUNT_SINGULAR => LEMMA_TU
        case _ => LEMMA_TUS
      }
      case LEMMA_TUYO => tupleN((basic, count)) match {
        case (GENDER_FEMININE, COUNT_SINGULAR) => LEMMA_TUYA
        case (GENDER_FEMININE, _) => LEMMA_TUYAS
        case (_, COUNT_SINGULAR) => LEMMA_TUYO
        case (_, _) => LEMMA_TUYOS
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
    }
  }

  private def ignoreGenderCount(lemma : String) : String =
  {
    lemma match {
      case LEMMA_ALGUNO | LEMMA_ALGUNA |
          LEMMA_ALGUNOS | LEMMA_ALGUNAS => LEMMA_ALGUN
      case LEMMA_AMBAS => LEMMA_AMBOS
      case LEMMA_AQUELLA | LEMMA_AQUELLO |
          LEMMA_AQUELLAS | LEMMA_AQUELLOS => LEMMA_AQUEL
      case LEMMA_CUALES => LEMMA_CUAL
      case LEMMA_EL_ACCENTED | LEMMA_ELLA |
          LEMMA_ELLOS | LEMMA_ELLAS => LEMMA_ELLO
      case LEMMA_LES => LEMMA_LE
      case LEMMA_LOS | LEMMA_LA | LEMMA_LAS => LEMMA_EL
      case LEMMA_ESA | LEMMA_ESOS | LEMMA_ESAS => LEMMA_ESO
      case LEMMA_ESTA | LEMMA_ESTOS | LEMMA_ESTAS => LEMMA_ESTO
      case LEMMA_NOSOTRAS => LEMMA_NOSOTROS
      case LEMMA_VOSOTRAS => LEMMA_VOSOTROS
      case LEMMA_NUESTRA | LEMMA_NUESTROS | LEMMA_NUESTRAS => LEMMA_NUESTRO
      case LEMMA_VUESTRA | LEMMA_VUESTROS | LEMMA_VUESTRAS => LEMMA_VUESTRO
      case LEMMA_SUS => LEMMA_SU
      case LEMMA_TODA | LEMMA_TODOS | LEMMA_TODAS => LEMMA_TODO
      case LEMMA_TUS => LEMMA_TU
      case LEMMA_TUYA | LEMMA_TUYAS | LEMMA_TUYOS => LEMMA_TUYO
      case LEMMA_UNO | LEMMA_UNA | LEMMA_UNOS | LEMMA_UNAS => LEMMA_UN
      case _ => lemma
    }
  }
}
