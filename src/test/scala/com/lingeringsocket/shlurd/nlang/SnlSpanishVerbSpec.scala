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

import org.specs2.mutable._
import org.specs2.specification.core._

import SnlSpanishLemmas._

class SnlSpanishVerbSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private implicit val tongue = SnlUtils.spanishTongue

  private val context = SprContext(tongue)

  case class ParsedVerb(
    subject : SilReference,
    rhs : Option[SilPhrase],
    lemma : String,
    tam : SilTam,
    question : Option[(SilQuestion, SilInflection)]
  )

  private def isRelationship(lemma : String, inflection : SilInflection) =
  {
    lemma match {
      case LEMMA_SER | LEMMA_TENER => (inflection != INFLECT_ACCUSATIVE)
      case _ => false
    }
  }

  private def parse(input : String) : ParsedVerb =
  {
    val sentence = SprParser(
      input, context).parseOne.sentence
    sentence match {
      case SilPredicateSentence(
        SilActionPredicate(subject, SilWordLemma(lemma), rhs, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, rhs, lemma, tam, None)
      }
      case SilPredicateSentence(
        SilRelationshipPredicate(subject, verb, complement, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, Some(complement), verb.toLemma, tam, None)
      }
      case SilPredicateSentence(
        SilStatePredicate(subject, SprStatePredefVerb(STATE_PREDEF_BE),
          rhs @ SilPropertyState(state : SilSimpleWord), Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, Some(rhs), LEMMA_ESTAR,
          tam, None)
      }
      case SilPredicateQuery(
        SilActionPredicate(subject, SilWordLemma(lemma), rhs, Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(
          subject, rhs, lemma, tam,
          Some((question, answerInflection)))
      }
      case SilPredicateQuery(
        SilRelationshipPredicate(subject, verb, complement, Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(
          subject, Some(complement), verb.toLemma, tam,
          Some((question, answerInflection)))
      }
      case SilPredicateQuery(
        SilStatePredicate(
          subject, SprStatePredefVerb(STATE_PREDEF_BE),
          rhs @ SilPropertyState(state : SilSimpleWord), Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(subject, Some(rhs), LEMMA_ESTAR,
          tam, Some((question, answerInflection)))
      }
      case _ => {
        throw new RuntimeException(s"unexpected sentence: $sentence")
      }
    }
  }

  private def generateInput(
    subject : SilReference,
    rhs : Option[SilPhrase],
    lemma : String,
    tam : SilTam,
    question : Option[(SilQuestion, SilInflection)]) : String =
  {
    def expectReference(phrase : SilPhrase) =
    {
      phrase match {
        case ref : SilReference => ref
        case _ => throw new RuntimeException(s"unexpected phrase $phrase")
      }
    }
    val predicate = {
      rhs match {
        case Some(state : SilState) => {
          SilStatePredicate(
            subject,
            STATE_PREDEF_BE.toVerb,
            state
          )
        }
        case _ => {
          if (isRelationship(
            lemma, question.map(_._2).getOrElse(INFLECT_NONE)) &&
            !tam.isProgressive)
          {
            SilRelationshipPredicate(
              subject,
              SilWord(lemma),
              rhs.map(expectReference).get
            )
          } else {
            SilActionPredicate(
              subject,
              SilWord.uninflected(lemma),
              rhs.map(expectReference),
              Seq.empty
            )
          }
        }
      }
    }
    val sentence = question match {
      case Some((q, answerInflection)) => {
        SilPredicateQuery(predicate, q, answerInflection, tam)
      }
      case _ => {
        SilPredicateSentence(predicate, tam)
      }
    }
    val printer = tongue.newSentencePrinter(tongue)
    printer.print(sentence)
  }

  private def pronounSeq : Seq[SilPronounReference] =
  {
    Seq(
      annotator.basicPronounRef(
        PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR),
      annotator.basicPronounRef(
        PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR),
      annotator.basicPronounRef(
        PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
        politeness = POLITENESS_RESPECTFUL),
      annotator.basicPronounRef(
        PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR),
      annotator.basicPronounRef(
        PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL),
      annotator.basicPronounRef(
        PERSON_SECOND, GENDER_FEMININE, COUNT_PLURAL),
      annotator.basicPronounRef(
        PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
        politeness = POLITENESS_RESPECTFUL),
      annotator.basicPronounRef(
        PERSON_THIRD, GENDER_MASCULINE, COUNT_PLURAL)
    )
  }

  private def moodSeq : Seq[SilTam] =
  {
    // SilTam.imperative is handled via a separate matrix
    Seq(
      SilTam.indicative,
      SilTam.interrogative
    )
  }

  private def modalitySeq : Seq[SilModality] =
  {
    Seq(
      MODAL_NEUTRAL,
      MODAL_MUST
    )
  }

  private def tenseSeq : Seq[SilTense] =
  {
    Seq(
      TENSE_PRESENT,
      TENSE_PAST
    )
  }

  private def aspectSeq : Seq[SilAspect] =
  {
    Seq(
      ASPECT_SIMPLE,
      ASPECT_PROGRESSIVE
    )
  }

  private def polaritySeq : Seq[SilPolarity] =
  {
    Seq(
      POLARITY_POSITIVE,
      POLARITY_NEGATIVE
    )
  }

  private def rhsSeq(
    question : Option[(SilQuestion, SilInflection)],
    pronoun : SilPronounReference)
      : Seq[Option[SilPhrase]] =
  {
    val count = question match {
      case Some((QUESTION_HOW_MANY, _)) => {
        COUNT_PLURAL
      }
      case _ => pronoun.count
    }
    val (clientWord, stateWord) = tupleN(
      pronoun.gender, count
    ) match {
      case (GENDER_FEMININE, COUNT_SINGULAR) => {
        tupleN(
          SilWord("clienta"),
          SilWord("ridícula", "ridículo"))
      }
      case (GENDER_FEMININE, COUNT_PLURAL) => {
        tupleN(
          SilWord("clientas", "clienta"),
          SilWord("ridículas", "ridículo"))
      }
      case (_, COUNT_SINGULAR) => {
        tupleN(
          SilWord("cliente"),
          SilWord("ridículo"))
      }
      case _ => {
        tupleN(
          SilWord("clientes", "cliente"),
          SilWord("ridículos", "ridículo"))
      }
    }
    question match {
      case Some((QUESTION_WHO, INFLECT_ACCUSATIVE)) => {
        // FIXME should be "a quién"
        val lemma = count match {
          case COUNT_SINGULAR => LEMMA_QUIEN_ACCENTED
          case _ => LEMMA_QUIENES_ACCENTED
        }
        Seq(
          Some(annotator.nounRef(SilWord(lemma), count))
        )
      }
      case Some((QUESTION_WHICH, INFLECT_ACCUSATIVE)) => {
        Seq(
          Some(annotator.determinedNounRef(
            clientWord, DETERMINER_VARIABLE, count))
        )
      }
      case Some((QUESTION_HOW_MANY, INFLECT_ACCUSATIVE)) => {
        Seq(
          Some(annotator.nounRef(
            clientWord, count
          ))
        )
      }
      case _ => {
        Seq(
          None,
          Some(annotator.determinedNounRef(
            clientWord, DETERMINER_DEFINITE, count)),
          Some(SilPropertyState(stateWord))
        )
      }
    }
  }

  private def questionSeq(pronoun : SilPronounReference)
      : Seq[(SilReference, (SilQuestion, SilInflection))] =
  {
    val quienWord = pronoun.count match {
      case COUNT_SINGULAR => SilWord(LEMMA_QUIEN_ACCENTED)
      case _ => SilWord(LEMMA_QUIENES_ACCENTED, LEMMA_QUIEN_ACCENTED)
    }
    // FIXME properly implement QUESTION_WHAT and test that;
    // also test non-modifier version of QUESTION_WHICH
    // FIXME test ungrammatical "who" in accusative
    // FIXME rework representation for QUESTION_WHERE and test that
    // FIXME test INFLECT_ADPOSITIONED and INFLECT_GENITIVE, plus
    // adpositional objects
    Seq(
      (annotator.nounRef(quienWord, pronoun.count),
        (QUESTION_WHO, INFLECT_NOMINATIVE)),
      (annotator.determinedNounRef(SilWord("agente"), DETERMINER_DEFINITE),
        (QUESTION_WHO, INFLECT_ACCUSATIVE)),
      (annotator.determinedNounRef(SilWord("agente"), DETERMINER_VARIABLE),
        (QUESTION_WHICH, INFLECT_NOMINATIVE)),
      (annotator.determinedNounRef(SilWord("agente"), DETERMINER_DEFINITE),
        (QUESTION_WHICH, INFLECT_ACCUSATIVE)),
      (annotator.nounRef(SilWord("agentes", "agente"), COUNT_PLURAL),
        (QUESTION_HOW_MANY, INFLECT_NOMINATIVE)),
      (annotator.determinedNounRef(SilWord("agente"), DETERMINER_DEFINITE),
        (QUESTION_HOW_MANY, INFLECT_ACCUSATIVE))
    )
  }

  private def isConsistent(
    pronoun : SilPronounReference,
    rhs : Option[SilPhrase], lemma : String,
    tam : SilTam, inflection : SilInflection = INFLECT_NONE) : Boolean =
  {
    if (!tam.isValid) {
      false
    } else {
      (!isRelationship(lemma, inflection) || !rhs.isEmpty) &&
        ((pronoun.person == PERSON_SECOND) || !tam.isImperative) &&
      (rhs.isEmpty || !rhs.get.isInstanceOf[SilState] ||
          (lemma == LEMMA_ESTAR)) &&
      (rhs.isEmpty || rhs.get.isInstanceOf[SilState] ||
        // FIXME should be (lemma != LEMMA_ESTAR)
          (lemma == LEMMA_SER)) &&
      ((inflection != INFLECT_ACCUSATIVE) ||
        ((lemma != LEMMA_ESTAR) && (lemma != LEMMA_SER))) &&
      (!tam.isProgressive ||
        ((lemma != LEMMA_ESTAR) && (lemma != LEMMA_SER))) &&
      (rhs.nonEmpty ||
        ((lemma != LEMMA_ESTAR))) &&
      (!tam.isPast ||
        (pronoun.person != PERSON_FIRST) || (pronoun.count != COUNT_PLURAL))
    }
  }

  private def mainSeq(lemma : String)
      : Seq[(SilPronounReference, Option[SilPhrase], String, SilTam)] =
  {
    pronounSeq.flatMap(
      pronoun => moodSeq.flatMap(
        mood => tenseSeq.flatMap(
          tense => aspectSeq.flatMap(
            aspect => modalitySeq.flatMap(
              modality => rhsSeq(None, pronoun).flatMap(
                rhs => polaritySeq.flatMap(
                  polarity => {
                    val tam = SilTamImmutable(
                      mood.mood,
                      polarity,
                      modality,
                      aspect,
                      tense)
                    if (isConsistent(
                      pronoun, rhs, lemma, tam))
                    {
                      Some((pronoun, rhs, lemma, tam))
                    } else {
                      None
                    }
                  }
                )
              )
            )
          )
        )
      )
    ).distinct
  }

  private def querySeq(lemma : String)
      : Seq[(SilReference, Option[SilPhrase], String, SilTam,
        (SilQuestion, SilInflection))] =
  {
    pronounSeq.flatMap(
      pronoun => tenseSeq.flatMap(
        tense => aspectSeq.flatMap(
          aspect => modalitySeq.flatMap(
            modality => questionSeq(pronoun).flatMap(
              question => rhsSeq(
                Some(question._2),
                pronoun).flatMap(
                rhs => polaritySeq.flatMap(
                  polarity => {
                    val tam = SilTamImmutable(
                      MOOD_INTERROGATIVE,
                      polarity,
                      modality,
                      aspect,
                      tense)
                    val subject = question._1
                    if (isConsistent(
                      pronoun, rhs, lemma, tam, question._2._2))
                    {
                      Some((subject, rhs, lemma, tam, question._2))
                    } else {
                      None
                    }
                  }
                )
              )
            )
          )
        )
      )
    ).distinct
  }

  private def imperativeSeq(lemma : String)
      : Seq[(SilReference, Option[SilPhrase], String, SilTam)] =
  {
    // FIXME support POLARITY_NEGATIVE
    val usted = annotator.basicPronounRef(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
      politeness = POLITENESS_RESPECTFUL)
    val ustedes = annotator.basicPronounRef(
      PERSON_SECOND, GENDER_SOMEONE, COUNT_PLURAL,
      politeness = POLITENESS_RESPECTFUL)
    val vosotros = annotator.basicPronounRef(
      PERSON_SECOND, GENDER_MASCULINE, COUNT_PLURAL,
      politeness = POLITENESS_FAMILIAR)
    Seq(usted, ustedes, vosotros).flatMap(pronoun => {
      rhsSeq(None, pronoun).flatMap(
        rhs => {
          val tam = SilTamImmutable(
            MOOD_IMPERATIVE,
            POLARITY_POSITIVE,
            MODAL_NEUTRAL,
            ASPECT_SIMPLE,
            TENSE_PRESENT)
          if (isConsistent(
            pronoun, rhs, lemma, tam))
          {
            Some((pronoun, rhs, lemma, tam))
          } else {
            None
          }
        }
      )
    }).distinct
  }

  "SnlSpanishVerbParser" should
  {
    "parse main matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_SER, LEMMA_ESTAR, LEMMA_TENER, "engañar").flatMap(mainSeq)
      ) {
        case (
          subject, rhs, lemma, tam
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, None)
          "in phrase: " + input >> {
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, None)
          }
        }
      }
    }

    "parse query matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_SER, LEMMA_ESTAR, LEMMA_TENER, "molestar").flatMap(querySeq)
      ) {
        case (
          subject, rhs, lemma, tam, question
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, Some(question))
          "in query: " + input >> {
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, Some(question))
          }
        }
      }
    }

    "parse imperative matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_SER, LEMMA_ESTAR, LEMMA_TENER, "ejecutar").flatMap(
          imperativeSeq)
      ) {
        case (
          subject, rhs, lemma, tam
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, None)
          "in command: " + input >> {
            if (lemma != "ejecutar") {
              skipped("not ready for prime time")
            }
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, None)
          }
        }
      }
    }

    // can be edited for a specific scenario and then run by itself
    "parse one" in
    {
      val subject = annotator.nounRef(
        SilWord("agentes", "agente"), COUNT_PLURAL)
      val rhs = None
      val lemma = "molestar"
      val tam = SilTam.interrogative.past.progressive.negative
      val question = Some((QUESTION_HOW_MANY, INFLECT_NOMINATIVE))
      val input = generateInput(
        subject, rhs, lemma, tam, question)
      if (false) {
        println(s"INPUT:  $input")
        SnlUtils.debug(input, context)
      }
      parse(input) must be equalTo ParsedVerb(
        subject, rhs, lemma, tam, question)
    }
  }
}
