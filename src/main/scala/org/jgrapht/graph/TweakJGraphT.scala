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
package org.jgrapht.graph

import org.jgrapht._

import scala.collection._

import java.util.function._

class TweakedMaskVertexSet[V](
  vertexSet : java.util.Set[V],
  mask : Predicate[V],
  minusSet : Set[V]
) extends MaskVertexSet[V](vertexSet, mask)
{
  override def size =
  {
    // this is incorrect in the case where minusSet is not a
    // subset of vertexSet, but it's only used as an estimate
    vertexSet.size - minusSet.size
  }
}

class TweakedMaskEdgeSet[V, E](
  graph : Graph[V, E],
  edgeSet : java.util.Set[E],
  vertexMask : Predicate[V],
  edgeMask : Predicate[E],
  minusSet : Set[E]
) extends MaskEdgeSet[V, E](graph, edgeSet, vertexMask, edgeMask)
{
  override def size =
  {
    // this is even less correct since it doesn't account
    // for vertex masking, but hey
    edgeSet.size - minusSet.size
  }
}
