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
    pronoun : SilPronounReference,
    rhs : Option[SilReference],
    lemma : String,
    tam : SilTam,
    question : Option[(SilQuestion, SilInflection)]) : String =
  {
    val predicate = {
      if (isRelationship(lemma) && !tam.isProgressive) {
        SilRelationshipPredicate(
          pronoun,
          rhs.get,
          relationshipFor(lemma)
        )
      } else {
        SilActionPredicate(
          pronoun,
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
    // FIXME SilTam.imperative is not ready for prime time
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

  private def questionSeq : Seq[Option[(SilQuestion, SilInflection)]] =
  {
    // FIXME this has all kinds of problems
    // Some((QUESTION_WHICH, INFLECT_ACCUSATIVE))
    Seq(
      None
    )
  }

  private def isConsistent(
    pronoun : SilPronounReference,
    rhs : Option[SilReference], lemma : String,
    tam : SilTam, question : Option[(SilQuestion, SilInflection)]) : Boolean =
  {
    if (!tam.isValid()) {
      false
    } else {
      (!isRelationship(lemma) || !rhs.isEmpty) &&
        (question.isEmpty || tam.isInterrogative) &&
        ((pronoun.person == PERSON_SECOND) || !tam.isImperative) &&
        ((tam.modality != MODAL_EMPHATIC) ||
          (!tam.isInterrogative && !tam.isNegative))
    }
  }

  private def allSeq(lemma : String)
      : Seq[(SilPronounReference, Option[SilReference], String, SilTam,
        Option[(SilQuestion, SilInflection)])] =
  {
    pronounSeq.flatMap(
      pronoun => moodSeq.flatMap(
        mood => tenseSeq.flatMap(
          tense => aspectSeq.flatMap(
            aspect => modalitySeq.flatMap(
              modality => rhsSeq.flatMap(
                rhs => questionSeq.flatMap(
                  question => polaritySeq.flatMap(
                    polarity => {
                      val tam = SilTamImmutable(
                        mood.mood,
                        polarity,
                        modality,
                        aspect,
                        tense)
                      if (isConsistent(
                        pronoun, rhs, lemma, tam, question))
                      {
                        Some((pronoun, rhs, lemma, tam, question))
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
      )
    )
  }

  "SprEnglishVerbParser" should
  {
    "parse matrix" >>
    {
      Fragment.foreach(
        allSeq(LEMMA_BE) ++ allSeq(LEMMA_HAVE) ++ allSeq("bamboozle")
      ) {
        case (
          pronoun, rhs, lemma, tam, question
        ) => {
          val input = generateInput(
            pronoun, rhs, lemma, tam, question)
          "in phrase: " + input >> {
            parse(input) must be equalTo ParsedVerb(
              pronoun, rhs, lemma, tam, question)
          }
        }
      }
    }

    // can be edited for a specific scenario and then run by itself
    "parse one" in
    {
      val pronoun = SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_PLURAL)
      val rhs = Some(SilNounReference(SilWord("customer"), DETERMINER_UNIQUE))
      val lemma = "bamboozle"
      val tam = SilTam.interrogative.negative
      val input = generateInput(
        pronoun, rhs, lemma, tam, None)
      if (false) {
        println(s"INPUT:  $input")
      }
      parse(input) must be equalTo ParsedVerb(
        pronoun, rhs, lemma, tam, None)
    }
  }
}
