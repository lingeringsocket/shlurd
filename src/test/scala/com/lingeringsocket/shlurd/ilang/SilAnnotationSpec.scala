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

class SilAnnotationSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private def makeSingular =
    annotator.pronounRef(PERSON_THIRD, GENDER_N, COUNT_SINGULAR)

  private def makePlural =
    annotator.pronounRef(PERSON_THIRD, GENDER_N, COUNT_PLURAL)

  "SilAnnotationSpec" should
  {
    "annotate counts" in
    {
      val singular = makeSingular
      val plural = makePlural

      val singularAnnotation = annotator.register(singular)
      val pluralAnnotation = annotator.register(plural)
      val singularNote = annotator.getNote(singularAnnotation)
      val pluralNote = annotator.getNote(pluralAnnotation)
      singularNote.getCount must be equalTo COUNT_SINGULAR
      pluralNote.getCount must be equalTo COUNT_PLURAL
    }

    "preserve annotators" in
    {
      val plural = makePlural

      val originalAnnotation = annotator.register(plural)
      val originalNote = annotator.getNote(originalAnnotation)
      originalNote.getCount must be equalTo COUNT_PLURAL

      val plural2 = plural.copy(count = COUNT_SINGULAR)
      val copiedAnnotation = annotator.register(plural2)
      annotator.preserveNote(originalAnnotation, copiedAnnotation)
      val copiedNote = annotator.getNote(copiedAnnotation)
      copiedNote.getCount must be equalTo COUNT_PLURAL

      val plural3 = plural.copy(count = COUNT_SINGULAR)
      val transformedAnnotation =
        annotator.transform(originalAnnotation, plural3)
      val transformedNote = annotator.getNote(transformedAnnotation)
      transformedNote.getCount must be equalTo COUNT_PLURAL
    }
  }
}
