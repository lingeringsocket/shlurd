// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

trait ShlurdSyntaxTree
{
  def label : String

  def lemma : String

  def token : String

  def children : Seq[ShlurdSyntaxTree]

  def numChildren = children.size

  def firstChild = children.head

  def lastChild = children.last

  def isLeaf = children.isEmpty

  def isPreTerminal = ((children.size == 1) && firstChild.isLeaf)

  def isPrePreTerminal = !isLeaf && children.forall(_.isPreTerminal)
}

case class ShlurdPennNode(label : String, children : Seq[ShlurdSyntaxTree])
    extends ShlurdSyntaxTree
{
  override def token = ""

  override def lemma = ""
}

case class ShlurdPennLeaf(label : String, lemma : String, token : String)
    extends ShlurdSyntaxTree
{
  override def children = Seq.empty
}
