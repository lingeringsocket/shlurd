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

import org.specs2.mutable._

import com.lingeringsocket.shlurd.parser._

abstract class SilSentencePrinterSpecification(
  val context : SprContext,
  clearInflections : Boolean = false
) extends Specification
{
  protected val printer =
    context.getTongue.newSentencePrinter(context.getTongue)

  protected def normalize(s : String) : String =
  {
    val parsed = SprParser(s, context).parseOne.sentence
    val normalized = normalize(parsed)
    val cleared = {
      if (clearInflections) {
        val annotator = SilBasicAnnotator()
        val rewriter = new SilPhraseRewriter(annotator)
        def clearWords = rewriter.replacementMatcher(
          "clearInflections", {
            case nr @ SilNounReference(noun) if noun.isProper => {
              nr
            }
            case phrase : SilPhrase if (phrase.maybeWord.nonEmpty) => {
              phrase.withNewWord(phrase.maybeWord.get.toUninflected)
            }
          }
        )
        rewriter.rewrite(clearWords, normalized)
      } else {
        normalized
      }
    }
    printer.print(cleared)
  }

  protected def normalize(parsed : SilSentence) : SilSentence =
  {
    parsed match {
      case SilPredicateSentence(predicate, tam, formality) => {
        tam.mood match {
          case MOOD_IMPERATIVE => SilPredicateSentence(
            predicate, tam,
            formality.copy(force = FORCE_EXCLAMATION))
          case _ => parsed
        }
      }
      case _ : SilConditionalSentence => parsed
      case _ : SilPredicateQuery => parsed
      case SilAmbiguousSentence(alternatives, _) => {
        SilAmbiguousSentence(alternatives.map(normalize))
      }
      case _ : SilConjunctiveSentence => {
        // FIXME
        parsed
      }
      case _ : SilUnknownSentence => parsed
      case _ : SilUnparsedSentence => parsed
    }
  }

  protected def expectPreserved(s : String) =
  {
    normalize(s) must be equalTo s
  }

  protected def expectStatement(s : String) =
  {
    normalize(s) must be equalTo (s + ".")
  }

  protected def expectCommand(s : String) =
  {
    normalize(s) must be equalTo (s + "!")
  }

  protected def expectQuestion(s : String) =
  {
    normalize(s) must be equalTo (s + "?")
  }

  protected def expectNormalized(s : String, normalized : String) =
  {
    normalize(s) must be equalTo(normalized)
  }
}
