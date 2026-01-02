package io.github.scala_tessella.dcel.structure

opaque type VertexId = Int

object VertexId extends GenericId:

  val prefix: String = "V"

  def apply(i: Int): VertexId = i

  def fromString(s: String): VertexId = fromStringSafe(s)

  extension (id: VertexId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)

type HalfEdgeId = (VertexId, VertexId)
