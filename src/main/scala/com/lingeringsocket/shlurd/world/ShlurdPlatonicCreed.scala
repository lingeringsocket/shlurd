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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._

class ShlurdPlatonicCreed(world : ShlurdPlatonicWorld)
{
  def allBeliefs() : Iterable[ShlurdSentence] =
  {
    world.getForms.values.flatMap(formBeliefs(_))
  }

  def formBeliefs(form : ShlurdPlatonicForm) : Iterable[ShlurdSentence] =
  {
    form.properties.values.map(
      formPropertyBelief(form, _)) ++
    form.getInflectedStateNormalizations.map(
      formStateNormalizationBelief(form, _))
  }

  def formPropertyBelief(
    form : ShlurdPlatonicForm,
    property : ShlurdPlatonicProperty) : ShlurdSentence =
  {
    ShlurdPredicateSentence(
      ShlurdStatePredicate(
        ShlurdNounReference(ShlurdWord(form.name), DETERMINER_NONSPECIFIC),
        if (property.states.size == 1) {
          propertyState(property.states.head)
        } else {
          ShlurdConjunctiveState(
            DETERMINER_ANY,
            property.states.map(propertyState).toSeq,
            SEPARATOR_CONJOINED)
        }
      ),
      ShlurdIndicativeMood(
        true,
        if (property.isClosed) MODAL_MUST else MODAL_MAY)
    )
  }

  def formStateNormalizationBelief(
    form : ShlurdPlatonicForm,
    entry : (ShlurdState, ShlurdState)) : ShlurdSentence =
  {
    ShlurdPredicateSentence(
      ShlurdStatePredicate(
        ShlurdStateSpecifiedReference(
          ShlurdNounReference(ShlurdWord(form.name), DETERMINER_NONSPECIFIC),
          entry._1),
        entry._2
      )
    )
  }

  private def propertyState(entry : (String, String)) =
  {
    ShlurdPropertyState(ShlurdWord(entry._2, entry._1))
  }
}
