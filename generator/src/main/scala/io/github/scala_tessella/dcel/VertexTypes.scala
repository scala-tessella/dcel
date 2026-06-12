package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.ring_seq.RingSeq.bracelet

/** The vertex-type combinatorics behind the Krotenheerdt enumeration (OEIS A068600).
  *
  * A vertex type is the cyclic sequence of regular polygons around a fully surrounded vertex, taken up to
  * rotation and reflection (bracelet normal form). Over the alphabet below there are exactly 15 valid types —
  * interior angles summing to exactly 360°.
  */
object VertexTypes:

  /** Cyclic sequence of polygon side-counts around a vertex, bracelet-normalized. */
  type VertexSignature = List[Int]

  /** The polygon alphabet. Includes the octagon deliberately: its only valid vertex type is `4.8.8`, so
    * octagon branches self-extinguish in any search beyond the 1-uniform `4.8.8` tiling — the enumeration
    * re-derives the known exclusion instead of assuming it.
    */
  val polygonSides: List[Int] = List(3, 4, 6, 8, 12)

  /** Interior angle of the unit regular polygon with the given number of sides. */
  def interiorAngle(sides: Int): AngleDegree =
    RegularPolygon(sides).alpha

  def normalize(signature: VertexSignature): VertexSignature =
    signature.bracelet

  private val fullCircle = AngleDegree(360)

  /** All valid vertex types over [[polygonSides]]: sequences of 3 to 6 polygons whose interior angles sum to
    * exactly 360°, bracelet-normalized. Exactly 15 exist.
    */
  val validSignatures: Set[VertexSignature] =
    def extend(partial: List[Int], partialSum: AngleDegree): Set[List[Int]] =
      if partialSum == fullCircle then
        if partial.sizeIs >= 3 then Set(partial) else Set.empty
      else if partial.sizeIs >= 6 then Set.empty
      else
        polygonSides.toSet.flatMap: sides =>
          val sum = partialSum + interiorAngle(sides)
          if sum.toRational <= fullCircle.toRational then extend(sides :: partial, sum)
          else Set.empty[List[Int]]
    extend(Nil, AngleDegree(0)).map(normalize)

  /** Every contiguous run of polygons (length 1 until the full type) that can appear around a
    * not-yet-complete vertex of a valid tiling, read in either direction: the proper contiguous subsequences
    * of the valid signatures. A boundary fan outside this set can never be completed to a valid vertex — the
    * central pruning test of the enumeration, subsuming dead-angle checks.
    */
  val validPartialFans: Set[List[Int]] =
    validSignatures.flatMap: signature =>
      val n       = signature.size
      val doubled = signature ++ signature
      val runs    =
        for
          start  <- 0 until n
          length <- 1 until n
        yield doubled.slice(start, start + length)
      runs.toSet.flatMap(run => Set(run, run.reverse))

  /** True when the ordered fan of polygons at a boundary vertex can still be completed to a valid vertex type
    * (the fan being a proper contiguous run of some valid signature, in either reading direction).
    */
  def isExtendableFan(fan: List[Int]): Boolean =
    validPartialFans.contains(fan)

  /** True when the full cyclic fan at a completed (interior) vertex is a valid vertex type. */
  def isCompleteVertex(fan: List[Int]): Boolean =
    validSignatures.contains(normalize(fan))
