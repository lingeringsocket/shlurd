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
package shlurd.world

import shlurd.parser._

import scala.util._

sealed trait ShlurdReferenceContext
case object REF_SUBJECT extends ShlurdReferenceContext
case object REF_LOCATION extends ShlurdReferenceContext
case object REF_LOCATED extends ShlurdReferenceContext

trait ShlurdEntity
{
}

trait ShlurdProperty
{
}

trait ShlurdWorld
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveReference(
    reference : ShlurdReference,
    context : ShlurdReferenceContext) : Try[ShlurdEntity]

  def resolveProperty(
    entity : ShlurdEntity,
    lemma : String) : Try[ShlurdProperty]

  def evaluateEntityPropertyPredicate(
    entity : ShlurdEntity,
    property : ShlurdProperty,
    lemma : String) : Try[Boolean]

  def evaluateEntityLocationPredicate(
    entity : ShlurdEntity,
    location : ShlurdEntity,
    locative : ShlurdLocative) : Try[Boolean]
}
