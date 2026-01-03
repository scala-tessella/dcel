package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

opaque type FaceId = Int

object FaceId extends Prefixable:

  val prefix: String = "F"

  val outerId: FaceId = FaceId(0)

  val firstInnerId: FaceId = FaceId(1)

  def apply(i: Int): FaceId = i

  def fromString(s: String): Either[ValidationError, FaceId] = fromStringUntrusted(s)

  extension (id: FaceId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)
