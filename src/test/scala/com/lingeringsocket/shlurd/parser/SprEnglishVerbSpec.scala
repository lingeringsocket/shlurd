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

import org.specs2.mutable._
import org.specs2.specification.core._

import SprEnglishLemmas._
import ShlurdEnglishAffixes._

// FIXME:  add coverage for various modifier types
// LEMMA_EXIST, including "there is" form
class SprEnglishVerbSpec extends Specification
{
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
      case LEMMA_BE | LEMMA_HAVE => (inflection != INFLECT_ACCUSATIVE)
      case _ => false
    }
  }

  private def parse(input : String) : ParsedVerb =
  {
    val sentence = SprParser(input).parseOne
    sentence match {
      case SilPredicateSentence(
        SilActionPredicate(subject, SilWordLemma(lemma), rhs, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, rhs, lemma, tam, None)
      }
      case SilPredicateSentence(
        SilRelationshipPredicate(subject, complement, verb, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, Some(complement), verb.toLemma, tam, None)
      }
      case SilPredicateSentence(
        SilStatePredicate(subject,
          rhs @ SilPropertyState(state : SilSimpleWord), Seq()),
        tam, _
      ) => {
        if (state.inflected.endsWith(SUFFIX_ING)) {
          ParsedVerb(subject, None, state.lemma,
            tam.progressive, None)
        } else {
          ParsedVerb(subject, Some(rhs), LEMMA_BE,
            tam, None)
        }
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
        SilRelationshipPredicate(subject, complement, verb, Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(
          subject, Some(complement), verb.toLemma, tam,
          Some((question, answerInflection)))
      }
      case SilPredicateQuery(
        SilStatePredicate(
          subject, rhs @ SilPropertyState(state : SilSimpleWord), Seq()),
        question, answerInflection, tam, _
      ) => {
        if (state.inflected.endsWith(SUFFIX_ING)) {
          ParsedVerb(
            subject, None, state.lemma, tam.progressive,
            Some((question, answerInflection)))
        } else {
          ParsedVerb(subject, Some(rhs), LEMMA_BE,
            tam, Some((question, answerInflection)))
        }
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
              rhs.map(expectReference).get,
              SilWord(lemma)
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
    val printer = new SilSentencePrinter
    printer.print(sentence)
  }

  private def pronounSeq() : Seq[SilPronounReference] =
  {
    Seq(
      SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR),
      SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR),
      SilPronounReference(PERSON_THIRD, GENDER_F, COUNT_SINGULAR),
      SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_PLURAL),
      SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL))
  }

  private def moodSeq() : Seq[SilTam] =
  {
    // SilTam.imperative is handled via a separate matrix
    Seq(
      SilTam.indicative,
      SilTam.interrogative
    )
  }

  private def modalitySeq() : Seq[SilModality] =
  {
    Seq(
      MODAL_NEUTRAL,
      MODAL_MUST,
      MODAL_EMPHATIC
    )
  }

  private def tenseSeq() : Seq[SilTense] =
  {
    Seq(
      TENSE_PRESENT,
      TENSE_PAST
    )
  }

  private def aspectSeq() : Seq[SilAspect] =
  {
    Seq(
      ASPECT_SIMPLE,
      ASPECT_PROGRESSIVE
    )
  }

  private def polaritySeq() : Seq[SilPolarity] =
  {
    Seq(
      POLARITY_POSITIVE,
      POLARITY_NEGATIVE
    )
  }

  private def rhsSeq(
    question : Option[(SilQuestion, SilInflection)] = None)
      : Seq[Option[SilPhrase]] =
  {
    question match {
      case Some((QUESTION_WHO, INFLECT_ACCUSATIVE)) => {
        Seq(
          Some(SilNounReference(SilWord(LEMMA_WHOM)))
        )
      }
      case Some((QUESTION_WHICH, INFLECT_ACCUSATIVE)) => {
        Seq(
          Some(SilNounReference(SilWord("customer")))
        )
      }
      case Some((QUESTION_HOW_MANY, INFLECT_ACCUSATIVE)) => {
        Seq(
          Some(SilNounReference(SilWord("customers", "customer"),
            DETERMINER_UNSPECIFIED, COUNT_PLURAL))
        )
      }
      case _ => {
        Seq(
          None,
          Some(SilNounReference(SilWord("customer"), DETERMINER_UNIQUE)),
          Some(SilPropertyState(SilWord("ridiculous")))
        )
      }
    }
  }

  private def questionSeq
      : Seq[(SilReference, (SilQuestion, SilInflection))] =
  {
    // FIXME properly implement QUESTION_WHAT and test that;
    // also test non-modifier version of QUESTION_WHICH
    // FIXME test ungrammatical "who" in accusative
    // FIXME rework representation for QUESTION_WHERE and test that
    // FIXME test INFLECT_ADPOSITIONED and INFLECT_GENITIVE, plus
    // adpositional objects
    Seq(
      (SilNounReference(SilWord(LEMMA_WHO)),
        (QUESTION_WHO, INFLECT_NOMINATIVE)),
      (SilNounReference(SilWord("agent"), DETERMINER_UNIQUE),
        (QUESTION_WHO, INFLECT_ACCUSATIVE)),
      (SilNounReference(SilWord("agent")),
        (QUESTION_WHICH, INFLECT_NOMINATIVE)),
      (SilNounReference(SilWord("agent"), DETERMINER_UNIQUE),
        (QUESTION_WHICH, INFLECT_ACCUSATIVE)),
      (SilNounReference(SilWord("agents", "agent"),
        DETERMINER_UNSPECIFIED, COUNT_PLURAL),
        (QUESTION_HOW_MANY, INFLECT_NOMINATIVE)),
      (SilNounReference(SilWord("agent"), DETERMINER_UNIQUE),
        (QUESTION_HOW_MANY, INFLECT_ACCUSATIVE))
    )
  }

  private def isConsistent(
    pronoun : SilPronounReference,
    rhs : Option[SilPhrase], lemma : String,
    tam : SilTam, inflection : SilInflection = INFLECT_NONE) : Boolean =
  {
    if (!tam.isValid()) {
      false
    } else {
      (!isRelationship(lemma, inflection) || !rhs.isEmpty) &&
        ((pronoun.person == PERSON_SECOND) || !tam.isImperative) &&
        (rhs.isEmpty || !rhs.get.isInstanceOf[SilState] ||
          (lemma == LEMMA_BE)) &&
        ((tam.modality != MODAL_EMPHATIC) ||
          (!tam.isInterrogative && !tam.isNegative && (lemma != LEMMA_BE))) &&
        ((inflection != INFLECT_ACCUSATIVE) || (lemma != LEMMA_BE))
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
              modality => rhsSeq().flatMap(
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
            modality => questionSeq.flatMap(
              question => rhsSeq(Some(question._2)).flatMap(
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
    // FIXME support POLARITY_NEGATIVE, MODALITY_EMPHATIC
    val pronoun = SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR)
    rhsSeq().flatMap(
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
  }.distinct

  private def notYetWorking(
    tam : SilTam, rhs : Option[SilPhrase],
    inflection : SilInflection) : Boolean =
  {
    // FIXME corenlp doesn't seem to understand progressives in these contexts
    tupleN((tam.polarity, tam.aspect, rhs, inflection)) match {
      case (POLARITY_POSITIVE, ASPECT_PROGRESSIVE,
        _, INFLECT_ACCUSATIVE) => true
      case (POLARITY_NEGATIVE, ASPECT_PROGRESSIVE,
        Some(_ : SilState), INFLECT_NONE) => true
      case _ => false
    }
  }

  "SprEnglishVerbParser" should
  {
    "parse main matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_BE, LEMMA_HAVE, "bamboozle").flatMap(mainSeq)
      ) {
        case (
          subject, rhs, lemma, tam
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, None)
          "in phrase: " + input >> {
            if (notYetWorking(tam, rhs, INFLECT_NONE)) {
              skipped("not working yet")
            }
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, None)
          }
        }
      }
    }

    "parse query matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_BE, LEMMA_HAVE, "pester").flatMap(querySeq)
      ) {
        case (
          subject, rhs, lemma, tam, question
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, Some(question))
          "in query: " + input >> {
            if (notYetWorking(tam, rhs, question._2)) {
              skipped("not working yet")
            }
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, Some(question))
          }
        }
      }
    }

    "parse imperative matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_BE, LEMMA_HAVE, "execute").flatMap(imperativeSeq)
      ) {
        case (
          subject, rhs, lemma, tam
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, None)
          "in command: " + input >> {
            if (lemma != "execute") {
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
      val subject = SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)
      val rhs = Some(SilPropertyState(SilWord("ridiculous")))
      val lemma = LEMMA_BE
      val tam = SilTam.interrogative.progressive
      val question = None
      val input = generateInput(
        subject, rhs, lemma, tam, question)
      if (false) {
        println(s"INPUT:  $input")
        SprParser.debug(input)
      }
      parse(input) must be equalTo ParsedVerb(
        subject, rhs, lemma, tam, question)
    }
  }
}
