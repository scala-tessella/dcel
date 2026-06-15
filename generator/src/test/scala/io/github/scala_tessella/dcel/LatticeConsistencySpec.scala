package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingLattice.translationLattice
import io.github.scala_tessella.dcel.geometry.{BigPoint, RegularPolygon}
import org.scalatest.OptionValues.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Phase 0 of the fixed-Λ engine: the Λ-consistency oracle in isolation. */
class LatticeConsistencySpec extends AnyFlatSpec with Matchers:

  /** A compact field of `sides`-gons: repeatedly add one polygon at the first boundary vertex it fits. */
  private def field(
      sides: Int,
      steps: Int
  ): Tiling = (0 until steps).foldLeft(TilingBuilder.createRegularPolygon(RegularPolygon(sides))): (t, _) =>
    t.boundaryVerticesUnsafe.iterator
      .flatMap(v => t.maybeAddRegularPolygonToBoundary(v.id, RegularPolygon(sides)).toOption)
      .nextOption()
      .getOrElse(t)

  private def origin(t: Tiling): BigPoint = t.vertices.head.coords

  behavior of "LatticeConsistency (Phase 0)"

  it should "accept a triangle field (3^6) under its own translation lattice" in:
    val t      = field(3, 24)
    val (v, w) = t.translationLattice().value
    LatticeConsistency.isConsistent(t, v, w, origin(t)) shouldBe true

  it should "accept a square field (4^4) under its own translation lattice" in:
    val t      = field(4, 20)
    val (v, w) = t.translationLattice().value
    LatticeConsistency.isConsistent(t, v, w, origin(t)) shouldBe true

  it should "accept a hexagon field (6.6.6) under its own translation lattice" in:
    val t      = field(6, 16)
    val (v, w) = t.translationLattice().value
    LatticeConsistency.isConsistent(t, v, w, origin(t)) shouldBe true

  // Pure Bravais fields (3^6, 4^4) are single-orbit, so any commensurate lattice merges identical
  // vertices without conflict — only a multi-orbit patch exposes a wrong lattice. The honeycomb has two
  // orbits with oppositely-oriented fans, and every edge connects the two orbits, so a lattice that uses
  // an edge vector as a basis vector merges them into a guaranteed angular-slot conflict.
  it should "reject a hexagon field under a lattice that merges its two vertex orbits" in:
    val t       = field(6, 16)
    val (_, hw) = t.translationLattice().value
    val v0      = t.vertices.head
    val edge    = v0.adjacentVerticesUnsafe.head.coords - v0.coords
    LatticeConsistency.isConsistent(t, edge, hw, origin(t)) shouldBe false
