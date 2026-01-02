package io.github.scala_tessella.dcel.structure

opaque type VertexId = String

object VertexId:

  def apply(s: String): VertexId = s

  extension (id: VertexId)

    def value: String = id

type HalfEdgeId = (VertexId, VertexId)
