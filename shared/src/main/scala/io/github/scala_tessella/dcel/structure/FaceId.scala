package io.github.scala_tessella.dcel.structure

import scala.util.{Try, Success}

opaque type FaceId = Int

object FaceId:

  val prefix: String = "F"

  val outerId: FaceId = FaceId(0)

  val firstInnerId: FaceId = FaceId(1)

  def apply(i: Int): FaceId = i

  private def prefixedString(i: Int): String = s"$prefix$i"

  def fromString(s: String): FaceId =
    Try(
      s.tail.toInt
    ) match
      case Success(i) if s == prefixedString(i) => i
      case _                                    => throw new IllegalArgumentException(s"Invalid face id: $s")

  extension (id: FaceId)

    def value: Int = id

    def toPrefixedString: String = prefixedString(id)
