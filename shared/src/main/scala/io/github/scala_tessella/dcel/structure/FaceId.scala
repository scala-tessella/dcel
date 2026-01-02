package io.github.scala_tessella.dcel.structure

opaque type FaceId = Int

object FaceId extends GenericId:

  val prefix: String = "F"

  val outerId: FaceId = FaceId(0)

  val firstInnerId: FaceId = FaceId(1)

  def apply(i: Int): FaceId = i

  def fromString(s: String): FaceId = fromStringSafe(s)

  extension (id: FaceId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)
