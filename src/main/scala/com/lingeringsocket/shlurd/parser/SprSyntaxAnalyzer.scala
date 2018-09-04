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

abstract class SprSyntaxAnalyzer(guessedQuestion : Boolean)
{
  private[parser] def analyzeSentence(
    tree : SptS)
      : SilSentence

  private[parser] def analyzeSQ(
    tree : SprSyntaxTree, forceSQ : Boolean)
      : SilSentence

  private[parser] def analyzeSBARQ(
    tree : SptSBARQ)
      : SilSentence

  private[parser] def analyzeNounPhrase(
    tree : SptNP)
      : SilReference

  private[parser] def analyzePronounReference(
    leaf : SprSyntaxLeaf)
      : SilPronounReference

  private[parser] def expectPropertyState(
    syntaxTree : SprSyntaxTree) : SilExpectedPropertyState

  private[parser] def expectAdpositionalState(
    tree : SprSyntaxTree)
    : SilState

  private[parser] def expectTemporalVerbModifier(
    tmod : SptTMOD)
      : SilVerbModifier

  private[parser] def expectVerbModifierPhrase(
    tree : SprSyntaxPhrase)
      : SilVerbModifier

  private[parser] def expectBasicVerbModifier(
    preTerminal : SprSyntaxPreTerminal)
      : SilVerbModifier

  private[parser] def expectAdpositionalVerbModifier(
    tree : SprSyntaxTree)
      : SilVerbModifier

  private[parser] def expectPropertyComplementState(
    tree : SprSyntaxTree)
      : SilState

  private[parser] def getCount(
    tree : SprSyntaxTree)
      : SilCount

  private[parser] def getWord(
    leaf : SprSyntaxLeaf)
      : SilWord

  private[parser] def isProhibitedPropertyState(
    preTerminal : SprSyntaxPreTerminal) : Boolean

  private[parser] def specifyReference(
    ref : SilReference, specifiedState : SilState)
      : SilReference
}
