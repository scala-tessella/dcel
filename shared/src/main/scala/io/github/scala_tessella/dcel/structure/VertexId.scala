package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.ValidationError

/** Opaque integer identifier for a [[Vertex]]. Prefixed string form is `V<n>` (e.g. `V42`) via the
  * [[Prefixable]] mixin on the companion.
  */
opaque type VertexId = Int

/** Companion object for [[VertexId]]. Offers `apply` from `Int`, validated `fromString` parsing of the
  * prefixed form, and an `Ordering` by underlying value.
  */
object VertexId extends Prefixable:

  given Ordering[VertexId] with
    def compare(x: VertexId, y: VertexId): Int =
      x.value.compare(y.value)

  /** Prefix used in the string form: `"V"`. */
  val prefix: String = "V"

  /** Wraps an `Int` as a [[VertexId]]. */
  def apply(i: Int): VertexId = i

  /** Parses a prefixed string (e.g. `"V42"`) back into a [[VertexId]]. Returns [[ValidationError]] if the
    * input is null, lacks the `V` prefix, or has no valid numeric tail.
    */
  def fromString(s: String): Either[ValidationError, VertexId] = fromStringUntrusted(s)

  private[dcel] def fromStringUnsafe(s: String): VertexId = fromStringTrusted(s)

  extension (id: VertexId)

    /** Unwraps to the underlying `Int`. */
    def value: Int = id

    /** Prefixed string form of the id, e.g. `"V42"`. */
    def toPrefixedString: String = prefixedString(id)

/** Pair of [[VertexId]]s identifying a half-edge by its origin and destination vertex ids. Used as a
  * compact key in lookup tables that need to address half-edges without holding object references.
  */
type HalfEdgeId = (VertexId, VertexId)
