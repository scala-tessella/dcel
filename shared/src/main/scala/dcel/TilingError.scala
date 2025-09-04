package dcel

sealed trait TilingError:
  def message: String

case class ValidationError(message: String) extends TilingError
case class TopologyError(message: String) extends TilingError
case class GeometryError(message: String) extends TilingError
case class NotFoundError(entity: String, id: String) extends TilingError:
  def message: String = s"$entity with ID '$id' not found."

object TilingError:

  // Helper methods for common error creation patterns
  def validation(msg: String): TilingError = ValidationError(msg)
  def topology(msg: String): TilingError = TopologyError(msg)
  def geometry(msg: String): TilingError = GeometryError(msg)
  def notFound(entity: String, id: String): TilingError = NotFoundError(entity, id)

  // Helper to combine multiple validation errors
  def combineValidationErrors(errors: List[String]): TilingError =
    if errors.length == 1 then ValidationError(errors.head)
    else ValidationError(s"Multiple validation errors: ${errors.mkString("; ")}")
