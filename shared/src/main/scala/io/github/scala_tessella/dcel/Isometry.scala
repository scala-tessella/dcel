package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}

/** A rigid motion of the plane, used to describe the copy added by [[TilingDCEL.maybeAddCopy]]. The four
  * cases are the four plane isometries (distance-preserving): translation and rotation preserve orientation,
  * reflection and glide reflection reverse it. Scaling is deliberately excluded — it would break the
  * unit-edge invariant. See ADR-0011.
  */
enum Isometry:

  /** Slide by the vector from `from` to `to` (arbitrary points). Exact in `BigDecimal`. */
  case Translation(from: BigPoint, to: BigPoint)

  /** Rotate about `center` by `degrees`, positive clockwise as rendered (ADR-0011). */
  case Rotation(center: BigPoint, degrees: AngleDegree)

  /** Reflect across the line through `axisP1` and `axisP2`. Orientation-reversing. */
  case Reflection(axisP1: BigPoint, axisP2: BigPoint)

  /** Reflect across the line through `axisP1` and `axisP2`, then glide along it by the vector
    * `axisP2 - axisP1`. The fourth plane isometry; orientation-reversing. Exact in `BigDecimal`.
    */
  case GlideReflection(axisP1: BigPoint, axisP2: BigPoint)
