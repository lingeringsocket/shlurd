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

import org.jgrapht._

import scala.collection._
import scala.collection.JavaConverters._

class GraphPathStreamer[V, E](graph : Graph[V, E], start : V)
{
  private case class PathEdgeAddress(v : V, edgeIndex : Int)
  {}

  private type PathAddress = Seq[PathEdgeAddress]

  private val edgeMap = new mutable.HashMap[V, Seq[E]]

  private var nextPath : Option[PathAddress] =
    Some(Vector(PathEdgeAddress(start, 0)))

  private def getEdges(v : V) : Seq[E] =
  {
    edgeMap.getOrElseUpdate(v, {
      graph.outgoingEdgesOf(v).asScala.toSeq
    })
  }

  protected def isExcluded(v : V) : Boolean = false

  private def expandPath(index : PathAddress) : Option[PathAddress] =
  {
    val last = index.last
    val edges = getEdges(last.v)
    if (last.edgeIndex >= edges.size) {
      None
    } else {
      val edge = edges(last.edgeIndex)
      val edgeEnd = graph.getEdgeTarget(edge)
      if (isExcluded(edgeEnd)) {
        None
      } else {
        Some(index :+ PathEdgeAddress(edgeEnd, 0))
      }
    }
  }

  private def incrementPath(index : PathAddress) : Option[PathAddress] =
  {
    if (index.isEmpty) {
      None
    } else {
      val last = index.last
      val edges = getEdges(last.v)
      val nextEdge = last.edgeIndex + 1
      if (nextEdge >= edges.size) {
        incrementPath(index.dropRight(1))
      } else {
        Some(index.dropRight(1) :+ last.copy(edgeIndex = nextEdge))
      }
    }
  }

  private def pathEdgeStream(index : PathAddress, pos : Int) : Stream[E] =
  {
    val nextPos = pos + 1
    if (nextPos < index.size) {
      val x = index(pos)
      val edge = getEdges(x.v)(x.edgeIndex)
      edge #:: pathEdgeStream(index, nextPos)
    } else {
      expandPath(index) match {
        case Some(expanded) => {
          nextPath = incrementPath(expanded)
          pathEdgeStream(expanded, pos)
        }
        case _ => {
          Stream.empty
        }
      }
    }
  }

  private def nextStream() : Option[Stream[E]] =
  {
    nextPath match {
      case Some(path) => {
        expandPath(path) match {
          case Some(expanded) => {
            nextPath = incrementPath(expanded)
            Some(pathEdgeStream(expanded, 0))
          }
          case _ => {
            nextPath = incrementPath(path)
            nextStream()
          }
        }
      }
      case _ => {
        None
      }
    }
  }

  def pathStream() : Stream[Stream[E]] =
  {
    nextStream() match {
      case Some(stream) => {
        stream #:: pathStream
      }
      case _ => Stream.empty
    }
  }
}
