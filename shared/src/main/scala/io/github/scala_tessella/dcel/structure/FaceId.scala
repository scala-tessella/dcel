package io.github.scala_tessella.dcel.structure

opaque type FaceId = String

object FaceId:

  val outerId: FaceId = FaceId("F0")

  val firstInnerId: FaceId = FaceId("F1")

  def apply(s: String): FaceId = s

  extension (id: FaceId)

    def value: String = id
