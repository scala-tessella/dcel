package io.github.scala_tessella.dcel.structure

import scala.util.Try

trait GenericId:

  val prefix: String

  private[structure] def prefixedString(i: Int): String = s"$prefix$i"

  private[structure] def fromStringSafe(s: String): Int =
    if s.startsWith(prefix) then
      val numericPart = s.substring(prefix.length)
      Try(numericPart.toInt).toOption match
        case Some(i) if s == prefixedString(i) => i
        case _                                 => throw IllegalArgumentException(s"Invalid numeric part in id: `$s`")
    else
      throw IllegalArgumentException(s"Invalid id prefix: `$s` (expected prefix `$prefix`)")
