package io.github.scala_tessella.dcel

sealed trait TilingError:
  def message: String

case class ValidationError(message: String)          extends TilingError
case class IncompleteError(message: String)          extends TilingError
case class TopologyError(message: String)            extends TilingError
case class GeometryError(message: String)            extends TilingError
case class SpatialError(message: String)             extends TilingError
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

  extension (either: Either[TilingError, Unit])
    def toErrorSuffix: String =
      either
        .swap
        .map: error =>
          s" [${error.message}]"
        .getOrElse("")
