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

import SnlSpanishLemmas._

import org.specs2.mutable._

import net.sf.extjwnl.data._

import scala.io._
import scala.util._

import scala.collection._
import scala.jdk.CollectionConverters._

class SnlSpanishConjugationSpec extends Specification
{
  private val wordnet = SnlUtils.spanishWordnet

  "SpanishMorphology" should
  {
    "conjugate irregular" in
    {
      SnlSpanishConjugation.conjugateVerb(
        LEMMA_SER,
        SnlSpanishConjugationCoord(
          PERSON_FIRST,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_INDICATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("soy")
    }

    "conjugate regular" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_FIRST,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_INDICATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivo")
    }

    "conjugate tú subjunctive" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_SUBJUNCTIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivas")
    }

    "conjugate usted imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_THIRD,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("viva")
    }

    "conjugate ustedes imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_THIRD,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivan")
    }

    "conjugate vosotros imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivid")
    }

    "conjugate vosotros subjunctive" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_SUBJUNCTIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("viváis")
    }

    "verify irregular forms" in
    {
      skipped("requires site specifics")

      val dir = "/home/jvs/open/lemarios/"

      val infinitives = Using.resource(
        Source.fromFile(dir + "verbos-espanol.txt")
      ) {
        source => source.getLines().map(_.stripSuffix("se")).toSeq
      }

      val conjugations = Using.resource(
        Source.fromFile(dir + "verbos-espanol-conjugaciones.txt")
      ) {
        source => source.getLines().map(
          _.replace("ándome", "ando").replace("éndome", "endo")
        ).toSet
      }

      val coords = SnlSpanishConjugation.fundamentalCoords.filterNot(coord => {
        (coord.mood == MOOD_IMPERATIVE) &&
        (coord.person == PERSON_SECOND) &&
        (coord.count == COUNT_PLURAL)
      })

      val ignored = untriaged ++ obscure

      var errors = 0
      var newIrregulars = 0

      def checkConjugation(
        form : String, infinitive : String, conjugated : String) : Unit =
      {
        if (!conjugations.contains(conjugated)) {
          println(s"INCORRECT $form $infinitive $conjugated")
          errors += 1
        } else if (wordnet.getVerbSenses(infinitive).nonEmpty) {
          val bases = wordnet.getMorphology.lookupAllBaseForms(
            POS.VERB, conjugated).asScala.toSet
          if (!bases.contains(infinitive)) {
            println(s"v#$conjugated -addexc $infinitive")
            newIrregulars += 1
          }
        }
      }

      infinitives.filterNot(ignored.contains).foreach(infinitive => {
        coords.foreach(coord => {
          val conjugated = SnlSpanishConjugation.conjugateVerb(
            infinitive, coord)
          checkConjugation(s"$coord", infinitive, conjugated)
        })

        val progressive = SnlSpanishConjugation.conjugateGerund(
          infinitive)
        checkConjugation("PROGRESSIVE", infinitive, progressive)

        val participle = SnlSpanishConjugation.conjugateParticiple(
          infinitive)
        checkConjugation("PARTICIPLE", infinitive, participle)
      })

      errors must be equalTo 0
      newIrregulars must be equalTo 0
    }
  }

  private val untriaged = Set(
    "desleír",
    "desoír",
    "desvaír",
    "embaír",
    "engreír",
    "entreoír",
    "trasoír",
    "freír",
    "sofreír",
    "sonreír",
    "refreír",
    "reír",
    "abar",
    "abducir",
    "abocanar",
    "abolir",
    "acaecer",
    "acantalear",
    "achichiguar",
    "acontecer",
    "acrecentar",
    "acupear",
    "adherir",
    "adir",
    "adormir",
    "adquirir",
    "aducir",
    "adulcir",
    "agüitar",
    "ahijar",
    "ahilar",
    "ahincar",
    "ahitar",
    "ahuchar",
    "ahumar",
    "ahusar",
    "airar",
    "aislar",
    "alborecer",
    "algaracear",
    "aloquecer",
    "amelarchiar",
    "amohinar",
    "antever",
    "anticuar",
    "antojar",
    "apirgüinar",
    "arcaizar",
    "argüendear",
    "argüir",
    "arrempujar",
    "asentir",
    "atardecer",
    "ataucar",
    "atañer",
    "aterir",
    "atraillar",
    "aullar",
    "aunar",
    "aupar",
    "autoinducir",
    "avergonzar",
    "balbucir",
    "baraustar",
    "brisar",
    "cabrahigar",
    "caler",
    "cascarrinar",
    "cellisquear",
    "cercear",
    "cernir",
    "chaparrear",
    "chiar",
    "chipiar",
    "ciar",
    "circunferir",
    "clarecer",
    "clorformar",
    "cocer",
    "coextender",
    "cohibir",
    "coitar",
    "colidir",
    "compungir",
    "concernir",
    "concordar",
    "condecir",
    "conferir",
    "conseguir",
    "consentir",
    "consolar",
    "consonar",
    "contradecir",
    "contrahacer",
    "controvertir",
    "contumeriar",
    "coproducir",
    "correntiar",
    "criar",
    "deducir",
    "deferir",
    "degollar",
    "delinquir",
    "denegar",
    "derrenegar",
    "desabrir",
    "desadvertir",
    "desahijar",
    "desahitar",
    "desahumar",
    "desainar",
    "desaislar",
    "desalentar",
    "desandar",
    "desarrendar",
    "desasentar",
    "desasir",
    "desatender",
    "desatentar",
    "desaterrar",
    "desatraillar",
    "descafeinar",
    "descerrar",
    "desceñir",
    "descimentar",
    "descomedir",
    "desconsentir",
    "desconsolar",
    "desdar",
    "desdecir",
    "desdentar",
    "desembaular",
    "desempedrar",
    "desencerrar",
    "desenraizar",
    "desentender",
    "desenterrar",
    "desfacer",
    "desfruncir",
    "deshacer",
    "deshelar",
    "desinvertir",
    "deslendrar",
    "deslucir",
    "desmedir",
    "desmelar",
    "desmembrar",
    "desmentir",
    "descocer",
    "desnegar",
    "desosar",
    "despedrar",
    "despernar",
    "despezar",
    "desplegar",
    "desraizar",
    "desterrar",
    "desteñir",
    "desuncir",
    "desventar",
    "desvergonzar",
    "desvestir",
    "diferir",
    "digerir",
    "diluviar",
    "discernir",
    "disentir",
    "distinguir",
    "educir",
    "embaular",
    "embestir",
    "empecer",
    "empedernir",
    "empedrar",
    "empigüelar",
    "empleitar",
    "encabrahigar",
    "encelajar",
    "encerrar",
    "enchagüitar",
    "encomendar",
    "endentar",
    "enfuriar",
    "engüerar",
    "enhestar",
    "enlenzar",
    "enlucir",
    "enmelar",
    "enmendar",
    "enraizar",
    "ensangrentar",
    "ensarmentar",
    "enterrar",
    "entrecerrar",
    "entredecir",
    "entredormir",
    "entrelucir",
    "entrepernar",
    "entreuntar",
    "entrever",
    "envelar",
    "enzainar",
    "erguir",
    "escarmentar",
    "escocer",
    "esparcir",
    "estajar",
    "estarcir",
    "estatuar",
    "estreñir",
    "europeizar",
    "evanescer",
    "expedir",
    "extinguir",
    "fiar",
    "fluir",
    "fosforescer",
    "fruir",
    "fruncir",
    "garantir",
    "garuar",
    "granizar",
    "gruir",
    "guiar",
    "hacer",
    "harinear",
    "hebraizar",
    "hendir",
    "herventar",
    "herver",
    "huir",
    "inferir",
    "ingerir",
    "injerir",
    "inquirir",
    "inserir",
    "inteligenciar",
    "interdecir",
    "interferir",
    "judaizar",
    "juncir",
    "juñir",
    "langüetear",
    "lengüetear",
    "liar",
    "lucir",
    "luir",
    "magiar",
    "malentender",
    "malherir",
    "manferir",
    "marcir",
    "maullar",
    "mecer",
    "melengüelear",
    "miar",
    "mollinear",
    "molliznar",
    "molliznear",
    "muir",
    "neblinear",
    "nerviar",
    "neviscar",
    "oblicuar",
    "obstar",
    "orvallar",
    "parahusar",
    "paramar",
    "paramear",
    "peneirar",
    "perniquebrar",
    "perseguir",
    "pervertir",
    "piar",
    "pintear",
    "podrir",
    "preconcebir",
    "predecir",
    "preelegir",
    "prelucir",
    "premorir",
    "presentir",
    "preterir",
    "prever",
    "proferir",
    "prohijar",
    "proseguir",
    "puar",
    "pubescer",
    "raer",
    "raizar",
    "raspahilar",
    "reargüir",
    "reaventar",
    "recalentar",
    "recentar",
    "receñir",
    "recocer",
    "recolegir",
    "reconducir",
    "reconvertir",
    "redargüir",
    "redecir",
    "reelegir",
    "reexpedir",
    "referir",
    "refregar",
    "regimentar",
    "regoldar",
    "rehacer",
    "rehenchir",
    "reherir",
    "rehervir",
    "rehilar",
    "rehuir",
    "rehundir",
    "rehurtar",
    "reilar",
    "relucir",
    "remecer",
    "remedir",
    "remendar",
    "renegar",
    "repensar",
    "replegar",
    "repodrir",
    "reproducir",
    "requebrar",
    "resaber",
    "resarcir",
    "reseguir",
    "resentir",
    "respahilar",
    "resquebrar",
    "restregar",
    "retentar",
    "reteñir",
    "retraducir",
    "reundir",
    "reunir",
    "reuntar",
    "reventar",
    "rever",
    "revertir",
    "revestir",
    "rezurcir",
    "ruar",
    "ruñir",
    "sahumar",
    "sainar",
    "satisfacer",
    "seducir",
    "sementar",
    "sobrecalentar",
    "sobreentender",
    "sobrehilar",
    "sobrentender",
    "sobresolar",
    "sobrevestir",
    "soler",
    "sonar",
    "sorripiar",
    "subdistinguir",
    "subentender",
    "subseguir",
    "subvertir",
    "superentender",
    "tardecer",
    "terapiar",
    "traillar",
    "transferir",
    "translucir",
    "trasferir",
    "traslucir",
    "trasver",
    "triar",
    "tropezar",
    "tumultuar",
    "uncir",
    "usucapir",
    "ventiscar",
    "ventisquear",
    "zaherir",
    "zaracear",
    "zurcir"
  )

  private val obscure = Set(
    "chiviar",
    "embutiar",
    "zurriar",
    "contorcer",
    "arrecir",
    "inhestar",
    "rocear"
  )
}
