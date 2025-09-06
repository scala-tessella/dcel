package dcel

sealed trait TilingError:
  def message: String

case class ValidationError(message: String)          extends TilingError
case class TopologyError(message: String)            extends TilingError
case class GeometryError(message: String)            extends TilingError
case class SpatialError(message: String)             extends TilingError
case class NotFoundError(entity: String, id: String) extends TilingError:
  def message: String = s"$entity with ID '$id' not found."

object TilingError:

  // Helper methods for common error creation patterns
  def validation(msg: String): TilingError              = ValidationError(msg)
  def topology(msg: String): TilingError                = TopologyError(msg)
  def geometry(msg: String): TilingError                = GeometryError(msg)
  def spatial(msg: String): TilingError                 = SpatialError(msg)
  def notFound(entity: String, id: String): TilingError = NotFoundError(entity, id)

  // Helper to combine multiple errors
  def combineErrors(errors: List[String], f: String => TilingError): TilingError =
    if errors.length == 1 then f(errors.head)
    else
      val errorType =
        f.apply("whatever") match
          case ValidationError(message)  => "validation"
          case TopologyError(message)    => "topology"
          case GeometryError(message)    => "geometry"
          case SpatialError(message)     => "spatial"
          case NotFoundError(entity, id) => ""
      f(s"Multiple $errorType errors: ${errors.mkString("; ")}")
