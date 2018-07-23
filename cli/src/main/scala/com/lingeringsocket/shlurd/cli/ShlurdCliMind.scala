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
package com.lingeringsocket.shlurd.cli

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.cosmos._
import com.lingeringsocket.shlurd.platonic._

import ShlurdEnglishLemmas._

class ShlurdCliMind(cosmos : SpcCosmos) extends SpcMind(cosmos)
{
  private lazy val entityInterviewer = uniqueEntity(
    cosmos.resolveQualifiedNoun(
      "interviewer", REF_SUBJECT, Set()))

  private lazy val entityShlurd = uniqueEntity(
    cosmos.resolveQualifiedNoun(
      LEMMA_PERSON, REF_SUBJECT, Set("shlurd")))

  override def resolvePronoun(
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    distance : SilDistance) =
  {
    if (count == COUNT_SINGULAR) {
      person match {
        case PERSON_FIRST => entityInterviewer.map(Set(_))
        case PERSON_SECOND => entityShlurd.map(Set(_))
        case _ => super.resolvePronoun(person, gender, count, distance)
      }
    } else {
      super.resolvePronoun(person, gender, count, distance)
    }
  }
}
