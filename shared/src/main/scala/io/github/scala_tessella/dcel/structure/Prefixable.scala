package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

/** Mixin for ID companion objects that produces a short string form combining a fixed prefix with the
  * underlying numeric value. Used by [[VertexId]] (`V`) and [[FaceId]] (`F`); see their `toPrefixedString`
  * extensions. The reverse direction is offered as a validated parse ([[fromStringUntrusted]]) plus a
  * fast trusted variant for internal call sites.
  */
trait Prefixable:

  /** The leading string that every id of this type starts with (e.g. `"V"` for `VertexId`). */
  val prefix: String

  private[structure] def prefixedString(i: Int): String = s"$prefix$i"

  private[structure] def fromStringTrusted(s: String): Int =
    Integer.parseInt(s.substring(prefix.length))

  /** Parses a string representation of an ID into its numeric component, ensuring the string contains the
    * required prefix and a valid numeric part.
    *
    *   - Avoids Try allocation: Using a basic try/catch with Integer.parseInt is significantly faster on the
    *     JVM and JS than wrapping in Try(...).toOption because it avoids two object allocations per call.
    *
    * @param s
    *   the string to parse, which must begin with the expected prefix and contain a numeric suffix.
    * @return
    *   either a `ValidationError` if the string is invalid, or the parsed integer if the string is valid.
    */
  private[structure] def fromStringUntrusted(s: String): Either[ValidationError, Int] =
    if s != null && s.startsWith(prefix) then // scalafix:ok DisableSyntax.null
      val startPos = prefix.length
      if startPos == s.length then
        Left(ValidationError(s"Missing numeric part in id: `$s`"))
      else
        // Use a more direct parsing approach to avoid Try overhead
        val numericPart = s.substring(startPos)
        val parsed      =
          try
            Integer.parseInt(numericPart)
          catch {
            case _: NumberFormatException => -1
          }
        if parsed >= 0 then
          Right(parsed)
        else
          Left(ValidationError(s"Invalid numeric part in id: `$s`"))
    else
      Left(ValidationError(s"Invalid id prefix: `$s` (expected prefix `$prefix`)"))
