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

import scala.collection._

abstract class SilAbstractRefNote(
  ref : SilReference
) {
  def getCount() : SilCount
}

case class SilBasicRefNote(
  ref : SilReference,
  var count : Option[SilCount] = None
) extends SilAbstractRefNote(ref)
{
  override def getCount() : SilCount =
  {
    count.getOrElse {
      val newCount = SilUtils.deriveCount(ref)
      count = Some(newCount)
      newCount
    }
  }
}

trait SilAnnotator
{
  def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType) : ReferenceType

  def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType) : ReferenceType

  def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType)

  def getBasicNote(ref : SilAnnotatedReference) : SilAbstractRefNote
}

trait SilAnnotation[NoteType <: SilAbstractRefNote]
{
  def getNote(ref : SilAnnotatedReference) : NoteType
}

class SilTypedAnnotator[NoteType <: SilAbstractRefNote](
  noteSupplier : (SilReference) => NoteType
) extends SilAnnotator with SilAnnotation[NoteType]
{
  private var nextId = 0

  private val map = new mutable.LinkedHashMap[Int, NoteType]

  override def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType) : ReferenceType =
  {
    if (!ref.hasAnnotation) {
      nextId += 1
      ref.registerAnnotation(this, nextId)
    }
    ref
  }

  override def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType) : ReferenceType =
  {
    val annotatedRef = register(ref)
    preserveNote(oldRef, annotatedRef)
    annotatedRef
  }

  override def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType
  ) {
    if (oldRef.hasAnnotation) {
      map.get(oldRef.getAnnotationId).foreach(
        note => map.put(newRef.getAnnotationId, note))
    }
  }

  override def getNote(ref : SilAnnotatedReference) : NoteType =
  {
    register(ref)
    map.getOrElseUpdate(ref.getAnnotationId, noteSupplier(ref))
  }

  override def getBasicNote(ref : SilAnnotatedReference) : SilAbstractRefNote =
  {
    getNote(ref)
  }
}