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

trait SprSyntaxAnalyzer
{
  def analyzeSentence(
    tree : SptS)
      : SilSentence

  def analyzeSQ(
    tree : SprSyntaxTree, forceSQ : Boolean)
      : SilSentence

  def analyzeSBARQ(
    tree : SptSBARQ)
      : SilSentence

  def analyzeNounPhrase(
    tree : SptNP)
      : SilReference

  def analyzePronounReference(
    leaf : SprSyntaxLeaf)
      : SilPronounReference

  def expectComplementState(
    tree : SprSyntaxTree) : SilExpectedComplementState

  def expectPropertyState(
    syntaxTree : SprSyntaxTree) : SilExpectedPropertyState

  def expectAdpositionalState(
    tree : SprSyntaxTree)
    : SilState

  def expectTemporalVerbModifier(
    tmod : SptTMOD)
      : SilVerbModifier

  def expectVerbModifierPhrase(
    tree : SprSyntaxPhrase)
      : SilVerbModifier

  def expectBasicVerbModifier(
    preTerminal : SprSyntaxPreTerminal)
      : SilVerbModifier

  def expectAdpositionalVerbModifier(
    tree : SprSyntaxTree)
      : SilVerbModifier

  def expectPropertyComplementState(
    tree : SprSyntaxTree)
      : SilState

  def getCount(
    tree : SprSyntaxTree)
      : SilCount

  def getWord(
    leaf : SprSyntaxLeaf)
      : SilWord

  def isProhibitedPropertyState(
    preTerminal : SprSyntaxPreTerminal) : Boolean

  def specifyReference(
    ref : SilReference, specifiedState : SilState)
      : SilReference

  def isNounPhraseModifier(tree : SprSyntaxTree) : Boolean

  def isNounPhraseHead(tree : SprSyntaxTree) : Boolean
}
