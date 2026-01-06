package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

opaque type VertexId = Int

object VertexId extends Prefixable:

  given Ordering[VertexId] with
    def compare(x: VertexId, y: VertexId): Int =
      x.value.compare(y.value)

  val prefix: String = "V"

  def apply(i: Int): VertexId = i

  def fromString(s: String): Either[ValidationError, VertexId] = fromStringUntrusted(s)

  private[dcel] def fromStringUnsafe(s: String): VertexId = fromStringTrusted(s)

  extension (id: VertexId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)

type HalfEdgeId = (VertexId, VertexId)
