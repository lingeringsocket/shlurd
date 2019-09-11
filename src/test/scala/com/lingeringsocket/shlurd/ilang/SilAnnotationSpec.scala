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

  private def makeNoun =
    annotator.nounRef(SilWord("sheep"), COUNT_PLURAL)

  "SilAnnotationSpec" should
  {
    "annotate counts" in
    {
      val singular = makeSingular
      val plural = makePlural

      val singularNote = annotator.getNote(singular)
      val pluralNote = annotator.getNote(plural)
      singularNote.getCount must be equalTo COUNT_SINGULAR
      pluralNote.getCount must be equalTo COUNT_PLURAL
    }

    "preserve annotators during transformation" in
    {
      val plural = makePlural

      val originalNote = annotator.getNote(plural)
      originalNote.getCount must be equalTo COUNT_PLURAL

      val plural2 = annotator.register(
        plural.copy(count = COUNT_SINGULAR))
      annotator.preserveNote(plural, plural2)
      val copiedNote = annotator.getNote(plural2)
      copiedNote.getCount must be equalTo COUNT_PLURAL

      val plural3 = annotator.register(
        plural.copy(count = COUNT_SINGULAR))
      val transformedAnnotation =
        annotator.transform(plural, plural3)
      val transformedNote = annotator.getNote(transformedAnnotation)
      transformedNote.getCount must be equalTo COUNT_SINGULAR

      val plural4 = annotator.register(
        plural.copy(count = COUNT_SINGULAR))
      val transformedAnnotation4 =
        annotator.transform(
          plural, plural4, SilPhraseCopyOptions(preserveNotes = true))
      val transformedNote4 = annotator.getNote(transformedAnnotation4)
      transformedNote4.getCount must be equalTo COUNT_PLURAL
    }

    "copy annotated references" in
    {
      val plural = makePlural
      val noun = makeNoun

      val genitive = annotator.genitiveRef(plural, noun)
      SilAnnotator.sanityCheck(annotator, genitive)

      val copySameAnnotator = annotator.copy(genitive)
      SilAnnotator.sanityCheck(annotator, genitive)
      SilAnnotator.sanityCheck(annotator, copySameAnnotator)
      copySameAnnotator must be equalTo genitive
      copySameAnnotator.getAnnotationId must not be equalTo(
        genitive.getAnnotationId)
      SilUtils.getCount(copySameAnnotator.possessee) must
        be equalTo COUNT_SINGULAR

      val annotator2 = SilBasicAnnotator()
      annotator must not be equalTo(annotator2)
      val copyOtherAnnotator = annotator2.copy(genitive)
      SilAnnotator.sanityCheck(annotator, genitive)
      SilAnnotator.sanityCheck(annotator2, copyOtherAnnotator)
      copyOtherAnnotator must be equalTo genitive
      SilUtils.getCount(copyOtherAnnotator.possessee) must
        be equalTo COUNT_SINGULAR
    }

    "preserve notes during copy" in
    {
      val plural = makePlural
      val noun = makeNoun
      annotator.getNote(noun).getCount must be equalTo COUNT_PLURAL
      val genitive = annotator.genitiveRef(plural, noun)

      val copySameAnnotator = annotator.copy(
        genitive, SilPhraseCopyOptions(preserveNotes = true))
      copySameAnnotator must be equalTo genitive
      SilUtils.getCount(copySameAnnotator.possessee) must
        be equalTo COUNT_PLURAL

      val annotator2 = SilBasicAnnotator()
      val copyOtherAnnotator = annotator2.copy(
        genitive, SilPhraseCopyOptions(preserveNotes = true))
      copyOtherAnnotator must be equalTo genitive
      SilUtils.getCount(copyOtherAnnotator.possessee) must
        be equalTo COUNT_PLURAL
    }

    "preserve ids during copy" in
    {
      makePlural
      val noun = makeNoun

      val annotator2 = SilBasicAnnotator()
      val copyOtherAnnotator = annotator2.copy(
        noun, SilPhraseCopyOptions(preserveIds = true))
      SilAnnotator.sanityCheck(annotator, noun)
      SilAnnotator.sanityCheck(annotator2, copyOtherAnnotator)
      copyOtherAnnotator must be equalTo noun
      copyOtherAnnotator.getAnnotationId must be equalTo noun.getAnnotationId

      annotator.copy(
        noun, SilPhraseCopyOptions(preserveIds = true)
      ) must throwA[IllegalArgumentException]
    }
  }
}
