package io.github.scala_tessella.dcel.structure

import scala.util.{Try, Success}

opaque type VertexId = Int

object VertexId:

  val prefix: String = "V"

  def apply(i: Int): VertexId = i

  private def prefixedString(i: Int): String = s"$prefix$i"

  def fromString(s: String): VertexId =
    Try(
      s.tail.toInt
    ) match
      case Success(i) if s == prefixedString(i) => i
      case _                                    => throw new IllegalArgumentException(s"Invalid vertex id: $s")

  extension (id: VertexId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)

type HalfEdgeId = (VertexId, VertexId)
