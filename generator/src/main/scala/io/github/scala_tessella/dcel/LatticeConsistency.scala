package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.interiorAngle
import io.github.scala_tessella.dcel.geometry.BigPoint

import scala.collection.mutable

/** Λ-consistency oracle for the fixed-lattice enumeration (ADR-0018 successor / fixed-Λ engine).
  *
  * Given a candidate translation lattice Λ = (v, w) and an origin, a planar patch is **Λ-consistent** iff
  * every *torus vertex* (planar vertices identified when equal mod Λ) carries a single, conflict-free local
  * structure: no angular slot around a torus vertex is claimed by two different incident polygons. A genuine
  * Λ-periodic tiling always satisfies this; an off-lattice "decoration" placement breaks it at the slot it
  * lands on, so gating growth through this predicate prunes the aperiodic scatter at the branch point.
  *
  * The test is exact. Edge directions of regular-polygon tilings are multiples of 15° — the gcd of the 30°
  * world of {3,4,6,12} and the 45° world of the octagon — so the circle splits into 24 integer slots and each
  * polygon covers a fixed integer span: triangle 4, square 6, hexagon 8, octagon 9, dodecagon 10. Polar
  * coordinates carry ~1e-12 float error (ADR-0009), but true directions are exact multiples of 15°, so
  * snapping the direction to the nearest slot is safe; the mod-Λ position key uses the SCALE=9 tolerance the
  * rest of the pipeline uses.
  *
  * Correctness does not depend on the left/right (CCW/CW) sector convention: as long as `(startSlot, size)`
  * is derived uniformly from `(vertex, incidentFace)`, a face and its lattice translate map to the same key,
  * so a slot conflict arises **iff** two physically distinct faces claim the same torus slot — i.e. iff the
  * patch is not Λ-periodic.
  */
object LatticeConsistency:

  private val Slots = 24

  private def slotSpan(size: Int): Int =
    math.round(interiorAngle(size).toRational.toDouble / 15.0).toInt

  /** Angular slot (0..23, units of 15°) of the direction from `from` to `to`. */
  private def directionSlot(from: BigPoint, to: BigPoint): Int =
    val deg = math.toDegrees(math.atan2((to.y - from.y).toDouble, (to.x - from.x).toDouble))
    val s   = math.round(deg / 15.0).toInt
    ((s % Slots) + Slots) % Slots

  private def frac(x: BigDecimal): BigDecimal =
    val r = x.setScale(9, BigDecimal.RoundingMode.HALF_UP)
    val f = r - r.setScale(0, BigDecimal.RoundingMode.FLOOR)
    if f >= BigDecimal(1) then BigDecimal(0).setScale(9)
    else f.setScale(9, BigDecimal.RoundingMode.HALF_UP)

  /** True iff `tiling` can extend to a tiling periodic with lattice (v, w): every torus vertex's committed
    * incident polygons occupy a conflict-free set of angular slots. Degenerate (collinear) bases are never
    * consistent.
    */
  def isConsistent(tiling: TilingDCEL, v: BigPoint, w: BigPoint, origin: BigPoint): Boolean =
    val det = v.x * w.y - v.y * w.x
    if det.abs < BigDecimal("1e-9") then false
    else
      def torusKey(p: BigPoint): (BigDecimal, BigDecimal) =
        val d     = p - origin
        val alpha = (d.x * w.y - w.x * d.y) / det
        val beta  = (v.x * d.y - d.x * v.y) / det
        (frac(alpha), frac(beta))

      // torus-vertex key -> (slot -> owning polygon as (startSlot, size))
      val coverage = mutable.Map.empty[(BigDecimal, BigDecimal), mutable.Map[Int, (Int, Int)]]
      tiling.vertices.forall: vertex =>
        val slotMap = coverage.getOrElseUpdate(torusKey(vertex.coords), mutable.Map.empty)
        vertex.incidentEdgesUnsafe.forall: edge =>
          edge.incidentFace.filter(_ != tiling.outerFace) match
            case None       => true
            case Some(face) =>
              val start = directionSlot(vertex.coords, edge.destinationUnsafe.coords)
              val size  = face.halfEdgesUnsafe.size
              (0 until slotSpan(size)).forall: k =>
                val slot = (start + k) % Slots
                slotMap.get(slot) match
                  case Some(owner) if owner != ((start, size)) => false
                  case _                                       =>
                    slotMap(slot) = (start, size)
                    true
