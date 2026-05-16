package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

/** Opaque integer identifier for a [[Face]]. Prefixed string form is `F<n>` (e.g. `F7`) via the
  * [[Prefixable]] mixin on the companion. Two reserved values: [[FaceId.outerId]] (`F0`, the unbounded
  * outer face) and [[FaceId.firstInnerId]] (`F1`, the first inner face produced by constructors).
  */
opaque type FaceId = Int

/** Companion object for [[FaceId]]. Holds the reserved ids and the smart constructors. */
object FaceId extends Prefixable:

  /** Prefix used in the string form: `"F"`. */
  val prefix: String = "F"

  /** The reserved id of the unbounded outer face (`F0`). Every tiling has exactly one outer face. */
  val outerId: FaceId = FaceId(0)

  /** The id of the first inner face produced by the polygon constructors (`F1`). */
  val firstInnerId: FaceId = FaceId(1)

  /** Wraps an `Int` as a [[FaceId]]. */
  def apply(i: Int): FaceId = i

  /** Parses a prefixed string (e.g. `"F7"`) back into a [[FaceId]]. Returns [[ValidationError]] if the
    * input is null, lacks the `F` prefix, or has no valid numeric tail.
    */
  def fromString(s: String): Either[ValidationError, FaceId] = fromStringUntrusted(s)

  private[dcel] def fromStringUnsafe(s: String): FaceId = fromStringTrusted(s)

  extension (id: FaceId)

    /** Unwraps to the underlying `Int`. */
    def value: Int = id

    /** Prefixed string form of the id, e.g. `"F7"`. */
    def toPrefixedString: String = prefixedString(id)
