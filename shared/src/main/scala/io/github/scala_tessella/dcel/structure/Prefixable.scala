package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

import scala.util.Try

/** A trait for types that can be used as IDs for DCEL entities. */
trait Prefixable:

  val prefix: String

  private[structure] def prefixedString(i: Int): String = s"$prefix$i"

  private[structure] def fromStringUntrusted(s: String): Either[ValidationError, Int] =
    if s.startsWith(prefix) then
      val numericPart = s.substring(prefix.length)
      Try(numericPart.toInt).toOption match
        case Some(i) if s == prefixedString(i) => Right(i)
        case _                                 => Left(ValidationError(s"Invalid numeric part in id: `$s`"))
    else
      Left(ValidationError(s"Invalid id prefix: `$s` (expected prefix `$prefix`)"))
