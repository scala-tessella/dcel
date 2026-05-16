package io.github.scala_tessella.dcel

/** Failure outcome returned in the `Left` of any `Either[TilingError, _]` produced by the library.
  *
  * Errors fall into a small set of categories that signal *what kind* of invariant was broken:
  *   - [[ValidationError]] — an operation precondition or argument is wrong (typical of `maybeAdd*` /
  *     `maybeDelete*` calls), or [[TilingValidation.validate]] is reporting an aggregate failure of the
  *     topology / geometry / spatial checks.
  *   - [[IncompleteError]] — a vertex, half-edge or face has unset or inconsistent internal state.
  *   - [[TopologyError]] — half-edge linkage (twin / next / prev / incident-face) or a face cycle is broken,
  *     or a deletion would partition the tiling.
  *   - [[GeometryError]] — interior angles are invalid or the inner-face / boundary angle vector does not
  *     close to a simple polygon.
  *   - [[SpatialError]] — coordinates conflict: two vertices share a position, an edge is not unit length, or
  *     two edges properly intersect.
  *   - [[NotFoundError]] — a lookup by `VertexId` or `FaceId` matched no entity in the tiling.
  *
  * All variants carry a human-readable [[message]]; `NotFoundError` additionally carries the queried entity
  * type and id for structured matching. See [[TilingError.combineErrors]] for the helper used to fold
  * multiple per-check messages into a single error of a chosen category.
  */
sealed trait TilingError:
  def message: String

/** Operation precondition or argument failed, or [[TilingValidation.validate]] aggregated several underlying
  * validation failures into a single error.
  *
  * Typical sources: a `maybeAdd*` / `maybeDelete*` call where the named vertex is not on the boundary, an
  * edge is missing its twin, or the requested action would otherwise violate a precondition. Also the
  * category used by `validate` to wrap a "Multiple validation errors: …" message when more than one of the
  * topology / geometry / spatial checks fails.
  */
case class ValidationError(message: String) extends TilingError

/** A vertex, half-edge or face is missing required state (e.g. a vertex without a leaving edge, a half-edge
  * without an incident face). Returned by the completeness stage of [[TilingValidation.validate]]; on
  * failure, the later checks are not run.
  */
case class IncompleteError(message: String) extends TilingError

/** Half-edge linkage (twin / next / prev / incident-face) or a face cycle is broken, the outer face is
  * unreachable, an inner face contains holes, or a deletion would partition the tiling into disconnected
  * components. Surfaced by the topology stage of [[TilingValidation.validate]] and by `maybeDelete*`
  * operations that would violate a topological invariant.
  */
case class TopologyError(message: String) extends TilingError

/** An interior angle is invalid (e.g. a full circle on a single edge) or an angle vector — for an inner face,
  * for the boundary's interior view, for the boundary's exterior view, or around an interior vertex — does
  * not close to a valid simple polygon or to 360°. Surfaced by the geometry stage of
  * [[TilingValidation.validate]].
  */
case class GeometryError(message: String) extends TilingError

/** Coordinate-level conflict: the boundary is not a simple polygon, two vertices share a position, an edge is
  * not of unit length, or two edges properly intersect. Surfaced by the spatial stage of
  * [[TilingValidation.validate]] and by `SimplePolygon.fromUntrusted` when the candidate fails the sweep-line
  * simplicity check.
  */
case class SpatialError(message: String) extends TilingError

/** A lookup by `VertexId` or `FaceId` did not match any entity in the tiling. Carries the queried `entity`
  * label (e.g. `"Vertex"`) and the `id` string for structured matching.
  */
case class NotFoundError(entity: String, id: String) extends TilingError:
  def message: String = s"$entity with ID '$id' not found."

/** The error category carries its own user-facing label and the factory that builds the corresponding
  * [[TilingError]] from a message string. `NotFoundError` is intentionally excluded: it carries two fields
  * and does not fit the single-string factory shape used by [[TilingError.combineErrors]].
  */
enum ErrorCategory(val label: String, val build: String => TilingError):
  case Validation extends ErrorCategory("validation", ValidationError(_))
  case Incomplete extends ErrorCategory("completeness", IncompleteError(_))
  case Topology   extends ErrorCategory("topology", TopologyError(_))
  case Geometry   extends ErrorCategory("geometry", GeometryError(_))
  case Spatial    extends ErrorCategory("spatial", SpatialError(_))

object TilingError:

  /** Combine one or more error messages into a single [[TilingError]] of the given category. A single message
    * is wrapped as-is; multiple messages are joined under a "Multiple ${label} errors: ..." prefix.
    */
  def combineErrors(errors: List[String], category: ErrorCategory): TilingError =
    errors match
      case single :: Nil => category.build(single)
      case many          => category.build(s"Multiple ${category.label} errors: ${many.mkString("; ")}")

  /** Convenience for embedding the failure message of an `Either[TilingError, Unit]` into a larger
    * human-readable string: returns `" [<message>]"` when the either is a `Left`, otherwise the empty string.
    * Useful for one-line success/failure logging where the caller wants to keep the message tail-position
    * when present.
    */
  extension (either: Either[TilingError, Unit])
    def toErrorSuffix: String =
      either
        .swap
        .map: error =>
          s" [${error.message}]"
        .getOrElse("")
