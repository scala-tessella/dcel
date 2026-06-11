package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.deepCopy
import io.github.scala_tessella.dcel.TilingMultiplication.*
import io.github.scala_tessella.dcel.TilingUniformity.regularPolygonsUnsafeFrom
import io.github.scala_tessella.dcel.Tree
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree, BigPoint, BigRadian, RegularPolygon, SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}

/** A [[TilingDCEL]] that is known to satisfy the full invariants of [[TilingValidation.validate]] — the
  * compiler-checked form of ADR-0003's safe/`Unsafe` convention (ADR-0017).
  *
  * `Tiling` is an opaque subtype of `TilingDCEL`: every query, export and analysis extension defined on the
  * raw type works on a `Tiling` unchanged, at zero runtime cost. What the type adds is provenance — a
  * `Tiling` can only be obtained from
  *
  *   - the public constructors ([[TilingBuilder]] and the [[TilingDCEL]] companion re-exports),
  *   - the mutating operations defined on `Tiling` itself (each returns `Either[TilingError, Tiling]`), or
  *   - [[Tiling.from]], which runs the full validation on an arbitrary [[TilingDCEL]].
  *
  * The guarantee is "valid at wrap time, never mutable from outside the package": the structural wiring is
  * `private[dcel]` and every mutating operation works on a deep copy (ADR-0002). Inside the package, every
  * wrap goes through [[Tiling.trusted]] so the complete trust boundary is greppable.
  */
opaque type Tiling <: TilingDCEL = TilingDCEL

object Tiling:

  /** Certifies an arbitrary [[TilingDCEL]] by running [[TilingValidation.validate]] on it. The structurally
    * empty tiling validates as the legitimate blank canvas.
    *
    * @return
    *   The certified tiling, or the [[TilingError]] reported by validation.
    */
  def from(tilingDCEL: TilingDCEL): Either[TilingError, Tiling] =
    validate(tilingDCEL).map: _ =>
      tilingDCEL

  /** Wraps without validating. Internal trust boundary (ADR-0017): callers assert that `tilingDCEL` is valid
    * — either by construction or because validation already ran. Every use site must be justified in the
    * ADR-0017 trust table; `grep "Tiling.trusted"` audits the boundary.
    */
  private[dcel] def trusted(tilingDCEL: TilingDCEL): Tiling =
    tilingDCEL

  /** The empty tiling: a certified blank canvas. */
  val empty: Tiling = trusted(TilingDCEL.empty)

  extension (tiling: Tiling)

    /** Adds a regular polygon to the tiling along the outer boundary.
      *
      * Preconditions:
      *   - onEdgeStartingWithVertexId identifies a half-edge that belongs to the current outer boundary.
      *   - sides >= 3.
      *   - The operation must not introduce self-intersections on the boundary.
      *
      * Postconditions on success:
      *   - A new inner face representing the regular polygon is added.
      *   - The outer boundary is updated to exclude any edges consumed by the new face and include the new
      *     outer edges.
      *   - All DCEL invariants are preserved: every inserted half-edge has a twin, coherent next/prev links,
      *     correct incident faces, and updated vertex leaving edges.
      *   - The returned TilingDCEL is a new instance reflecting the mutation.
      *
      * Failure cases:
      *   - Returns a TilingError when the edge is not on the boundary, sides are invalid, or the growth would
      *     cause boundary intersections or violate topology/geometry constraints.
      */
    def maybeAddRegularPolygonToBoundary(
        onEdgeStartingWith: VertexId,
        polygon: RegularPolygon
    ): Either[TilingError, Tiling] =
      tiling.deepCopy.addRegularPolygonToBoundary(onEdgeStartingWith, polygon).map(trusted)

    /** Adds an arbitrary simple polygon (described by its interior angles, in degrees) to the outer boundary,
      * starting at the given vertex. The polygon's first edge sits on the boundary edge starting at
      * `onEdgeStartingWith`.
      *
      * @return
      *   A fresh tiling with the polygon appended, or a `TilingError` if the angles do not close, the polygon
      *   would self-intersect, or the precondition checks fail.
      */
    def maybeAddSimplePolygonToBoundary(
        onEdgeStartingWith: VertexId,
        angles: Vector[AngleDegree]
    ): Either[TilingError, Tiling] =
      tiling.deepCopy.addUntrustedSimplePolygonToBoundary(onEdgeStartingWith, angles).map(trusted)

    /** Adds a regular polygon to the outer boundary along the path between two boundary vertices, growing
      * outward. Use this overload when you want to specify both endpoints rather than only the starting edge.
      */
    def maybeAddRegularPolygon(
        start: VertexId,
        end: VertexId,
        polygon: RegularPolygon
    ): Either[TilingError, Tiling] =
      tiling.deepCopy.addRegularPolygon(start, end, polygon).map(trusted)

    /** Two-endpoint variant of [[maybeAddSimplePolygonToBoundary]]: adds a simple polygon (described by its
      * interior angles) to the boundary along the path from `start` to `end`.
      */
    def maybeAddSimplePolygon(
        start: VertexId,
        end: VertexId,
        angles: Vector[AngleDegree]
    ): Either[TilingError, Tiling] =
      tiling.deepCopy.addUntrustedSimplePolygon(start, end, angles).map(trusted)

    /** Doubles the tiling area by translating it across one of its parallelogon period directions (or by
      * reflecting it across the centroid for an equilateral-triangle boundary). Useful for growing periodic
      * tilings without computing per-vertex additions.
      *
      * @return
      *   The doubled tiling, or [[ValidationError]] when the tiling's boundary is not a parallelogon (and not
      *   an equilateral triangle).
      */
    def doubleArea: Either[TilingError, Tiling] =
      if tiling.isEmpty then
        Right(tiling)
      else
        val polygon       = tiling.boundarySimplePolygonUnsafe
        lazy val boundary = tiling.boundaryVerticesUnsafe
        polygon.parallelogonDoubleIndices match
          case None if polygon.isEquilateralTriangle =>
            val angles = polygon.toAngles
            val origin =
              angles.indexWhere: angleDegree =>
                angleDegree == AngleDegree(60)
            val repeat = (angles.size / 3) + origin
            Right(trusted(tiling.rawDouble(
              boundary(origin),
              boundary(repeat),
              withInversion = true
            )))
          case None                                  => Left(ValidationError("Tiling is not a parallelogon, cannot fill the whole plane."))
          case Some((origin, repeat))                =>
            Right(trusted(tiling.rawDouble(boundary(origin), boundary(repeat))))

    /** Adds a copy of the whole tiling to itself under the given [[Isometry]] (translation, rotation,
      * reflection or glide reflection), merging it in and validating the result in full. The single primitive
      * behind the four named `maybeAdd…Copy` convenience methods.
      *
      * The result is valid only if the copy lands outside the existing tiling or exactly follows its
      * composition: coincident vertices are unified, shared edges collapsed and fully-overlapping faces
      * deduplicated. An isometry that maps the tiling onto an existing symmetry reproduces the original.
      *
      * @return
      *   The grown tiling, or a [[TilingError]] when the copy conflicts with the existing composition
      *   (partial overlap, crossing edges, over-full vertex angles, a degenerate reflection axis, ...).
      */
    def maybeAddCopy(isometry: Isometry): Either[TilingError, Tiling] =
      isometry match
        case Isometry.Translation(from, to)           =>
          val delta: BigPoint = to - from
          tiling.addIsometricCopy(_ + delta).map(trusted)
        case Isometry.Rotation(center, degrees)       =>
          val angle: BigRadian = degrees.toBigRadian
          tiling.addIsometricCopy(_.rotatedAround(center, angle)).map(trusted)
        case Isometry.Reflection(axisP1, axisP2)      =>
          if axisP1.almostEquals(axisP2) then
            Left(ValidationError("A mirror axis requires two distinct points."))
          else
            tiling.addReflectedCopy(_.reflectedAcross(axisP1, axisP2)).map(trusted)
        case Isometry.GlideReflection(axisP1, axisP2) =>
          if axisP1.almostEquals(axisP2) then
            Left(ValidationError("A glide-reflection axis requires two distinct points."))
          else
            val glide: BigPoint = axisP2 - axisP1
            tiling.addReflectedCopy(point => point.reflectedAcross(axisP1, axisP2) + glide).map(trusted)

    def maybeAddTranslatedCopy(from: BigPoint, to: BigPoint): Either[TilingError, Tiling] =
      maybeAddCopy(Isometry.Translation(from, to))

    /** Adds a rotated copy of the whole tiling to itself, rotating it about `center` (an arbitrary point — a
      * vertex, an edge midpoint, a face centre, anything) by `degrees`.
      *
      * Sign convention (ADR-0011): `degrees` is positive **clockwise** as rendered in the SVG view (negative
      * counterclockwise). Internally this is a counterclockwise rotation in the y-up model frame, which the
      * `flippedY` export turns into a clockwise on-screen rotation.
      *
      * As with [[maybeAddTranslatedCopy]], the result is valid only if the copy lands outside the existing
      * tiling or exactly follows its composition; the merged tiling is validated in full. Because the copy
      * must snap onto the existing unit-edge composition, only rotations by a local symmetry angle (a
      * multiple of 60°/90°/... by configuration) will succeed — most arbitrary angles are rejected.
      *
      * @return
      *   The grown tiling, or a [[TilingError]] when the copy conflicts with the existing composition.
      */
    def maybeAddRotatedCopy(center: BigPoint, degrees: AngleDegree): Either[TilingError, Tiling] =
      maybeAddCopy(Isometry.Rotation(center, degrees))

    /** Adds a mirrored copy of the whole tiling to itself, reflecting it across the line through `axisP1` and
      * `axisP2` (arbitrary points — vertices, edge midpoints, anything).
      *
      * Reflection is the orientation-reversing isometry, so the copy's DCEL wiring is rebuilt with reversed
      * orientation (unlike translate/rotate); the coordinate map itself is exact in `BigDecimal`. As with the
      * other copy operations, the result is valid only if the copy lands outside the existing tiling or
      * exactly follows its composition, and the merged tiling is validated in full. Reflecting across an
      * existing symmetry axis reproduces the original.
      *
      * @return
      *   The grown tiling, or a [[TilingError]] when the two axis points coincide, or when the copy conflicts
      *   with the existing composition.
      */
    def maybeAddMirroredCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, Tiling] =
      maybeAddCopy(Isometry.Reflection(axisP1, axisP2))

    /** Adds a glide-reflected copy of the whole tiling to itself: reflect across the line through `axisP1`
      * and `axisP2`, then slide along that line by the vector `axisP2 - axisP1`. The fourth plane isometry,
      * orientation-reversing like a plain reflection but with a built-in translation along the axis (the
      * symmetry of running-bond and many monohedral tilings).
      *
      * @return
      *   The grown tiling, or a [[TilingError]] when the two axis points coincide, or when the copy conflicts
      *   with the existing composition.
      */
    def maybeAddGlideReflectedCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, Tiling] =
      maybeAddCopy(Isometry.GlideReflection(axisP1, axisP2))

    /** Fills the gap around `vertexId` with as many triangles as fit, producing a fan whose apex is the named
      * vertex. Useful for closing small angular gaps on the boundary.
      */
    def fanAt(vertexId: VertexId): Either[TilingError, Tiling] =
      for
        vertex <- tiling.findVertex(vertexId)
        result <- tiling.rawFan(vertex)
      yield trusted(result)

    /** Fans `order` rotated copies of the whole tiling around an arbitrary `center` point — each rotated by a
      * full `360 / order` slice — into one rotationally-symmetric patch. The general, full-ring counterpart
      * of [[fanAt]]: the centre may be any point (a face centroid, an edge midpoint, ...), not only a
      * boundary vertex.
      *
      * The ring always spans the complete 360°: it succeeds only if every wedge fits the existing composition
      * (a centre face that maps onto itself is deduplicated; coincident vertices and edges are unified) and
      * the whole patch validates — otherwise it returns a [[TilingError]] (strict). For example, mirroring a
      * pentagon across an edge and then `fanAround`-ing the central pentagon's centroid with `order = 5`
      * produces the six-pentagon Dürer cluster.
      *
      * For a regular n-gon centred on its own centroid, `order = n` is the canonical full fan; any divisor of
      * `n` is also geometrically valid. A best-effort partial fan is intentionally not offered here — chain
      * [[maybeAddRotatedCopy]] until the first `Left` to fill one side as far as it goes.
      *
      * @return
      *   The fanned tiling, or a [[TilingError]] if `order < 2` or any wedge conflicts with the composition.
      */
    def fanAround(center: BigPoint, order: Int): Either[TilingError, Tiling] =
      tiling.rawFanAround(center, order).map(trusted)

    /** Repeats the whole tiling `count` times in a strip, each copy translated by a further step of the
      * vector from `from` to `to` (`k · (to − from)` for `k = 0 until count`). The translational counterpart
      * of [[fanAround]]; the `count = 2` period-detected special case is [[doubleArea]].
      *
      * All copies are merged and the completed strip is validated once; a copy that conflicts with the
      * composition surfaces as a self-intersection or over-full vertex in that final check. Copies that
      * exactly overlap (a period equal to part of the tiling) are deduplicated.
      *
      * @return
      *   The repeated tiling (the original unchanged when `count == 1`), or a [[TilingError]] if `count < 1`
      *   or a copy conflicts with the composition.
      */
    def repeatAlong(from: BigPoint, to: BigPoint, count: Int): Either[TilingError, Tiling] =
      tiling.rawRepeatAlong(to - from, count).map(trusted)

    /** Repeats the whole tiling over a 2-D lattice: `countA` copies stepped by `toA − from`, each of those
      * rows then repeated `countB` times stepped by `toB − from`. A convenience composition of two
      * [[repeatAlong]] sweeps (the editor can equally chain `repeatAlong(...).flatMap(_.repeatAlong(...))`).
      *
      * @return
      *   The lattice patch, or a [[TilingError]] if either count is `< 1` or a copy conflicts with the
      *   composition.
      */
    def repeatGrid(
        from: BigPoint,
        toA: BigPoint,
        countA: Int,
        toB: BigPoint,
        countB: Int
    ): Either[TilingError, Tiling] =
      repeatAlong(from, toA, countA).flatMap: strip =>
        strip.repeatAlong(from, toB, countB)

    /** Removes the vertex and the edges/faces incident to it. Fails with a [[TopologyError]] if the deletion
      * would disconnect the tiling or break a topological invariant.
      */
    def maybeDeleteVertex(vertexId: VertexId): Either[TilingError, Tiling] =
      tiling.deepCopy.deleteVertex(vertexId).map(trusted)

    /** Removes the edge connecting `startVertexId` to `endVertexId` (and its twin), merging the two incident
      * faces. Fails with a [[TopologyError]] if the edge is missing or the merge would violate an invariant.
      */
    def maybeDeleteEdge(startVertexId: VertexId, endVertexId: VertexId): Either[TilingError, Tiling] =
      tiling.deepCopy.deleteEdge(startVertexId, endVertexId).map(trusted)

    /** Deletes an inner face from the tiling.
      *
      * Preconditions:
      *   - faceId references an existing inner (bounded) face.
      *   - Deleting the face does not partition the tiling into disconnected components except for the
      *     intended boundary change.
      *
      * Postconditions on success:
      *   - The face and its incident half-edges that are not shared with other faces are removed or
      *     repurposed.
      *   - If the deleted face touches the outer boundary, the boundary expands accordingly by relinking or
      *     creating appropriate boundary half-edges.
      *   - All DCEL invariants remain satisfied (twin, next/prev, incidentFace, vertex leaving edges).
      *   - The returned TilingDCEL is a new instance reflecting the mutation. If the deleted face was the
      *     only inner face, the result may be an empty tessellation.
      *
      * Failure cases:
      *   - Returns a TilingError when the face does not exist, when the removal would split the tiling
      *     invalidly, or when integrity checks fail.
      */
    def maybeDeleteFace(faceId: FaceId): Either[TilingError, Tiling] =
      tiling.deepCopy.deleteFace(faceId).map(trusted)

    /** The outer boundary expressed as a `SimplePolygon` (angles are the conjugates of the boundary half-edge
      * angles, since the polygon is traversed externally). Total on a certified tiling; cached on the
      * underlying instance.
      */
    def boundarySimplePolygon: SimplePolygon =
      tiling.boundarySimplePolygonUnsafe

    /** Computes the gonality trees for the given uniformity tree structure, generating a list of partial
      * trees with representative vertex ids. It ensures that the root branches are not empty.
      *
      * @return
      *   A list of trees where each tree represents a simplified slice of the original uniformity tree, with
      *   just one representative vertex id instead of the full list.
      */
    def gonalityTrees: List[Tree[VertexId]] =
      val adjusted = tiling.uniformityTree match
        case Leaf(Nil)         => Leaf(Nil)
        case Leaf(value)       => Branch(value, List(Leaf(value)))
        case branch: Branch[?] => branch
      adjusted.ensureDepthOneBranchesHaveValidValues(_.isEmpty, _.head.firstLeaf.get)
        .children
        .map: child =>
          child.map: vertexIds =>
            vertexIds.headOption.getOrElse(VertexId(-1))
        .map:
          case leaf: Leaf[VertexId]     => leaf
          case child @ Branch(value, _) =>
            Branch(
              value,
              child.flattenLeaves.map: vertexId =>
                Leaf(vertexId)
            )

    /** Pairs each gonality tree with the list of regular polygons incident to its representative vertex. On a
      * certified tiling every vertex surround is well-formed, so no fallible variant is needed.
      */
    def gonalityTreesWithPolygons: List[(List[RegularPolygon], Tree[VertexId])] =
      gonalityTrees.map: tree =>
        (tiling.regularPolygonsUnsafeFrom(tree.value), tree)
