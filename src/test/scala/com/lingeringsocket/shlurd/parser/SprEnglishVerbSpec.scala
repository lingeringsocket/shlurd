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

import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification.core._

import SprEnglishLemmas._

// FIXME:  add coverage for state predicates; LEMMA_EXIST;
// various modifier types
class SprEnglishVerbSpec extends Specification
{
  case class ParsedVerb(
    subject : SilReference,
    rhs : Option[SilReference],
    lemma : String,
    tam : SilTam,
    question : Option[(SilQuestion, SilInflection)]
  )

  private def isRelationship(lemma : String) =
  {
    lemma match {
      case LEMMA_BE | LEMMA_HAVE => true
      case _ => false
    }
  }

  private def relationshipFor(lemma : String) =
  {
    lemma match {
      case LEMMA_BE => REL_IDENTITY
      case LEMMA_HAVE => REL_ASSOCIATION
      case _ => throw new RuntimeException(s"unexpected lemma $lemma")
    }
  }

  private def lemmaFor(rel : SilRelationship) =
  {
    rel match {
      case REL_IDENTITY => LEMMA_BE
      case REL_ASSOCIATION => LEMMA_HAVE
    }
  }

  private def parse(input : String) : ParsedVerb =
  {
    val sentence = SprParser(input).parseOne
    sentence match {
      case SilPredicateSentence(
        SilActionPredicate(subject, action, rhs, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, rhs, action.lemma, tam, None)
      }
      case SilPredicateSentence(
        SilRelationshipPredicate(subject, complement, rel, Seq()),
        tam, _
      ) => {
        ParsedVerb(subject, Some(complement), lemmaFor(rel), tam, None)
      }
      case SilPredicateQuery(
        SilActionPredicate(subject, action, rhs, Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(
          subject, rhs, action.lemma, tam,
          Some((question, answerInflection)))
      }
      case SilPredicateQuery(
        SilRelationshipPredicate(subject, complement, rel, Seq()),
        question, answerInflection, tam, _
      ) => {
        ParsedVerb(
          subject, Some(complement), lemmaFor(rel), tam,
          Some((question, answerInflection)))
      }
      case _ => {
        throw new RuntimeException(s"unexpected sentence: $sentence")
      }
    }
  }

  private def generateInput(
    subject : SilReference,
    rhs : Option[SilReference],
    lemma : String,
    tam : SilTam,
    question : Option[(SilQuestion, SilInflection)]) : String =
  {
    val predicate = {
      if (isRelationship(lemma) && !tam.isProgressive) {
        SilRelationshipPredicate(
          subject,
          rhs.get,
          relationshipFor(lemma)
        )
      } else {
        SilActionPredicate(
          subject,
          SilWord("", lemma),
          rhs,
          Seq.empty
        )
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

  private def rhsSeq : Seq[Option[SilReference]] =
  {
    Seq(
      None,
      Some(SilNounReference(SilWord("customer"), DETERMINER_UNIQUE))
    )
  }

  private def questionSeq
      : Seq[(SilReference, (SilQuestion, SilInflection))] =
  {
    Seq(
      (SilNounReference(SilWord(LEMMA_WHO)),
        (QUESTION_WHO, INFLECT_NOMINATIVE)),
      (SilNounReference(SilWord("agent")),
        (QUESTION_WHICH, INFLECT_NOMINATIVE))
    )
  }

  private def isConsistent(
    pronoun : SilPronounReference,
    rhs : Option[SilReference], lemma : String,
    tam : SilTam) : Boolean =
  {
    if (!tam.isValid()) {
      false
    } else {
      (!isRelationship(lemma) || !rhs.isEmpty) &&
        ((pronoun.person == PERSON_SECOND) || !tam.isImperative) &&
        ((tam.modality != MODAL_EMPHATIC) ||
          (!tam.isInterrogative && !tam.isNegative && (lemma != LEMMA_BE)))
    }
  }

  private def mainSeq(lemma : String)
      : Seq[(SilReference, Option[SilReference], String, SilTam)] =
  {
    pronounSeq.flatMap(
      pronoun => moodSeq.flatMap(
        mood => tenseSeq.flatMap(
          tense => aspectSeq.flatMap(
            aspect => modalitySeq.flatMap(
              modality => rhsSeq.flatMap(
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
      : Seq[(SilReference, Option[SilReference], String, SilTam,
        (SilQuestion, SilInflection))] =
  {
    pronounSeq.flatMap(
      pronoun => tenseSeq.flatMap(
        tense => aspectSeq.flatMap(
          aspect => modalitySeq.flatMap(
            modality => rhsSeq.flatMap(
              rhs => questionSeq.flatMap(
                question => polaritySeq.flatMap(
                  polarity => {
                    val tam = SilTamImmutable(
                      MOOD_INTERROGATIVE,
                      polarity,
                      modality,
                      aspect,
                      tense)
                    val subject = question._1
                    if (isConsistent(
                      pronoun, rhs, lemma, tam))
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
      : Seq[(SilReference, Option[SilReference], String, SilTam)] =
  {
    // FIXME support POLARITY_NEGATIVE, MODALITY_EMPHATIC
    val pronoun = SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR)
    rhsSeq.flatMap(
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
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, None)
          }
        }
      }
    }

    "parse query matrix" >>
    {
      Fragment.foreach(
        Seq(LEMMA_BE, LEMMA_HAVE, "chase").flatMap(querySeq)
      ) {
        case (
          subject, rhs, lemma, tam, question
        ) => {
          val input = generateInput(
            subject, rhs, lemma, tam, Some(question))
          "in phrase: " + input >> {
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
          "in phrase: " + input >> {
            skipped("not ready for prime time")
            parse(input) must be equalTo ParsedVerb(
              subject, rhs, lemma, tam, None)
          }
        }
      }
    }

    // can be edited for a specific scenario and then run by itself
    "parse one" in
    {
      val subject = SilNounReference(SilWord("agent"))
      val rhs = Some(SilNounReference(SilWord("customer"), DETERMINER_UNIQUE))
      val lemma = LEMMA_BE
      val tam = SilTam.interrogative.withModality(MODAL_MUST)
      val question = Some((QUESTION_WHICH, INFLECT_NOMINATIVE))
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
