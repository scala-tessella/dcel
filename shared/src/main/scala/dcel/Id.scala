package dcel

// Define opaque types for IDs to prevent mixing them up
opaque type VertexId = String
opaque type FaceId = String

object VertexId:
  def apply(s: String): VertexId = s
  extension (id: VertexId)
    def value: String = id

object FaceId:
  def apply(s: String): FaceId = s
  extension (id: FaceId)
    def value: String = id

  val outerId: FaceId = FaceId("F0")

  val firstInnerId: FaceId = FaceId("F1")
