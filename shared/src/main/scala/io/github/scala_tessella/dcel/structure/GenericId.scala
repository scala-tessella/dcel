package io.github.scala_tessella.dcel.structure

import scala.util.{Try, Success}

trait GenericId:

  val prefix: String

  private[structure] def prefixedString(i: Int): String = s"$prefix$i"

  private[structure] def fromStringSafe(s: String): Int =
    Try(
      s.tail.toInt
    ) match
      case Success(i) if s == prefixedString(i) => i
      case _                                    => throw new IllegalArgumentException(s"Invalid id: $s")
