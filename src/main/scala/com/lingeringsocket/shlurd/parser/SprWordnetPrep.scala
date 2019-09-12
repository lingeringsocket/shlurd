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
import scala.util._

import java.io._

// to compare CoreNLP against Wordnet:
// sbt corenlp/console
//   new com.lingeringsocket.shlurd.corenlp.CorenlpTestSetup
//   SprWordnetPrep.runAll()
object SprWordnetPrep
{
  SprParser.enableCache(Some(new File("run/test-parser-cache.dat")))
  private val parserCache = SprParser.getCache

  private val context = SprContext()

  private def parseOne(
    sentence : String,
    dumpAnalysis : Boolean)
      : Option[(SilSentence, SilSentence)] =
  {
    val parser = SprParser(sentence)
    val sil = parser.parseOne.sentence
    if (!sil.hasUnknown && !sil.isInstanceOf[SilAmbiguousSentence]) {
      println("SENTENCE = " + sentence)
      println
      if (dumpAnalysis) {
        println("BASELINE TREE = " + sil.maybeSyntaxTree)
        println
        println("BASELINE SIL = " + sil)
        println
      }
      val wnParser = SprParser.prepareHeuristic(
        context, sentence, dumpAnalysis, "DEBUG")
      Some(tupleN((wnParser.parseOne.sentence, sil)))
    } else {
      None
    }
  }

  private def allSentences() =
  {
    parserCache.keys.map(_.sentence).toSet
  }

  def buildMatcher()
  {
    val matcher = new SprPhrasePatternMatcher
    def addToMatcher(syntaxTree : SprSyntaxTree)
    {
      syntaxTree match {
        case _ : SprSyntaxLeaf  =>
        case _ : SprSyntaxPreTerminal =>
        case _ => {
          matcher.addRule(syntaxTree)
          syntaxTree.children.foreach(addToMatcher)
        }
      }
    }
    val sentences = allSentences
    sentences.foreach(sentence => {
      val parser = SprParser(sentence)
      val sil = parser.parseOne.sentence
      if (!sil.hasUnknown) {
        sil.maybeSyntaxTree.foreach(syntaxTree => {
          addToMatcher(syntaxTree)
        })
      }
    })
    println("PATTERN MATCHER = " + matcher)
  }

  def runAll(
    sentences : Iterable[String] = allSentences,
    dumpAnalysis : Boolean = false)
  {
    println("TOTAL = " + sentences.size)
    var processed = 0
    var succeeded = 0
    var mismatched = 0
    var slow = 0
    var failed = 0
    var ambiguous = 0
    def reportStatus() {
      println("PROCESSED = " + processed)
      println("SUCCEEDED = " + succeeded)
      println("MISMATCHED = " + mismatched)
      println("SLOW = " + slow)
      println("FAILED = " + failed)
      println("AMBIGUOUS = " + ambiguous)
      println
    }
    sentences.foreach(sentence => {
      Try(parseOne(sentence, dumpAnalysis)) match {
        case Success(Some((silWordnet, silCorenlp))) => {
          silWordnet match {
            case _ if (silWordnet.hasUnknown) => {
              println("FAILED PARSE")
              failed += 1
            }
            case SilAmbiguousSentence(alternatives, _) => {
              println("AMBIGUOUS PARSE")
              println("SIL = " + alternatives)
              println("SPR = " + alternatives.map(_.maybeSyntaxTree))
              ambiguous += 1
            }
            case _ => {
              val resolver = new SprAmbiguityResolver(context)
              resolver.resolveAmbiguousSentence(
                SilAmbiguousSentence(Seq(silCorenlp, silWordnet))) match
              {
                case _ : SilAmbiguousSentence => {
                  println("MISMATCHED PARSE")
                  println("BASELINE = " + silCorenlp)
                  println("WN = " + silWordnet)
                  mismatched += 1
                }
                case _ => {
                  succeeded += 1
                }
              }
            }
          }
          processed += 1
          if ((processed % 10) == 0) {
            reportStatus
          }
        }
        case Failure(SprParseComplexityException()) => {
          slow += 1
          println("PARSER TOO SLOW")
        }
        case _ => {
          if (dumpAnalysis) {
            println("BASELINE PARSE FAILED")
          }
        }
      }
    })
    reportStatus
  }

  def runOne()
  {
    val sentence = "the hammer is no longer in the box"
    runAll(Seq(sentence), true)
  }
}
