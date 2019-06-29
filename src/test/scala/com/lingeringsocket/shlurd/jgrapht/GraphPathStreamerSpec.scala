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
package com.lingeringsocket.shlurd.jgrapht

import org.jgrapht.graph._

import scala.collection.immutable._

import org.specs2.mutable._

class GraphPathStreamerSpec extends Specification
{
  private def forceSeq[T](stream : Stream[Stream[T]]) : Seq[Seq[T]] =
  {
    stream.map(_.force.toSeq).force.toSeq
  }

  "GraphPathStreamer" should
  {
    "stream paths" in
    {
      val graph = new SimpleDirectedGraph[Int, String](classOf[String])
      graph.addVertex(1)
      graph.addVertex(2)
      graph.addVertex(3)
      graph.addVertex(4)
      graph.addVertex(5)
      graph.addVertex(6)
      graph.addEdge(1, 2, "A")
      graph.addEdge(2, 3, "B")
      graph.addEdge(1, 3, "C")
      graph.addEdge(2, 4, "D")
      graph.addEdge(3, 4, "E")
      graph.addEdge(4, 5, "F")
      graph.addEdge(4, 6, "G")

      val allStreamer = new GraphPathStreamer(graph, 1)
      forceSeq(allStreamer.pathStream) must be equalTo Seq(
        Seq("A", "B", "E", "F"),
        Seq("A", "B", "E", "G"),
        Seq("A", "D", "F"),
        Seq("A", "D", "G"),
        Seq("C", "E", "F"),
        Seq("C", "E", "G")
      )

      val excludedStreamer = new GraphPathStreamer(graph, 2) {
        override protected def isExcluded(v : Int) : Boolean =
        {
          v >= 5
        }
      }
      forceSeq(excludedStreamer.pathStream) must be equalTo Seq(
        Seq("B", "E"),
        Seq("D")
      )

      val skipStreamer = new GraphPathStreamer(graph, 1)
      val skipStream = skipStreamer.pathStream
      val head = skipStream.head
      val tail = skipStream.tail
      head.take(2).force.toSeq must be equalTo(
        Seq("A", "B")
      )
      forceSeq(tail) must be equalTo Seq(
        Seq("A", "D", "F"),
        Seq("A", "D", "G"),
        Seq("C", "E", "F"),
        Seq("C", "E", "G")
      )
    }
  }
}

