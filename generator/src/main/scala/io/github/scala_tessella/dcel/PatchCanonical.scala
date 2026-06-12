package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{BigPoint, BigRadian}

/** Canonical congruence form of a finite patch, used as the sound search-state key of the Krotenheerdt
  * enumeration: two patches share a key if and only if they are congruent (equal up to a plane isometry), and
  * congruent patches have identical sets of continuations, so merging their search states never loses a
  * tiling — unlike the boundary-signature key it replaces, which could conflate non-congruent interiors.
  *
  * The key is computed by anchoring the patch at every boundary edge in both orientations (the frame mapping
  * that edge's origin to the plane origin and its direction to the positive x axis, optionally mirrored),
  * describing every inner face as (sides, rounded transformed centroid), and taking the lexicographically
  * smallest sorted description over all frames. Rounding follows the library's 9-decimal discipline
  * (coordinates carry ~1e-12 polar-construction error; 9 decimals folds coincident points together).
  */
object PatchCanonical:

  private val SCALE = 9

  private def rounded(value: BigDecimal): BigDecimal =
    value.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)

  /** One face under a frame: side count plus rounded centroid coordinates. */
  private type FaceDescriptor = (Int, BigDecimal, BigDecimal)

  given Ordering[FaceDescriptor] =
    Ordering.Tuple3(Ordering.Int, Ordering.BigDecimal, Ordering.BigDecimal)

  def congruenceKey(tiling: TilingDCEL): List[(Int, BigDecimal, BigDecimal)] =
    val faces: List[(Int, BigPoint)] =
      tiling.innerFaces.map: face =>
        val vertices = face.getVerticesUnsafe.map(_.coords)
        (vertices.size, vertices.centroid)

    val descriptionsPerFrame =
      for
        edge <- tiling.boundaryEdgesUnsafe
        flip <- List(false, true)
      yield
        // A reflection reverses boundary orientation, so mirrored frames anchor the edge reversed:
        // the anti-frame (destination -> origin, then y-negated) of this patch matches a direct
        // frame of its mirror image, making the key reflection-invariant.
        val (origin, destination) =
          if flip then (edge.destinationUnsafe.coords, edge.origin.coords)
          else (edge.origin.coords, edge.destinationUnsafe.coords)
        val rotation              = BigRadian(-origin.angleTo(destination).toDouble)
        faces
          .map: (sides, centroid) =>
            val framed = (centroid - origin).rotatedAround(BigPoint.origin, rotation)
            val y      = if flip then -framed.y else framed.y
            (sides, rounded(framed.x), rounded(y))
          .sorted

    import scala.math.Ordering.Implicits.seqOrdering
    descriptionsPerFrame.min
