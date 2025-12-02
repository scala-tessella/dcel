package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.Symmetry.*
import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SymmetrySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "Symmetry"

  it should "be calculated for a rhombus structure" in:
    val rhombus = List(1, 2, 1, 2)
    allAssert(
      rhombus.rotationalSymmetry shouldBe 2,
      rhombus.reflectionalSymmetryIndices shouldBe List(1, 3),
      rhombus.reflectionalSymmetryAxes shouldBe List((1, 3), (0, 2)),
      rhombus.reflectionalSymmetry shouldBe 2
    )

  it should "be calculated for an equilateral triangle structure" in:
    val equilateral = List(1, 1, 1)
    allAssert(
      equilateral.rotationalSymmetry shouldBe 3,
      equilateral.reflectionalSymmetryIndices shouldBe List(0, 1, 2),
      equilateral.reflectionalSymmetryAxes shouldBe List((1, (2, 0)), (2, (0, 1)), (0, (1, 2))),
      equilateral.reflectionalSymmetry shouldBe 3
    )

  it should "be calculated for an irregular triangle structure" in :
    val irregular = List(1, 2, 3)
    allAssert(
      irregular.rotationalSymmetry shouldBe 1,
      irregular.reflectionalSymmetryIndices shouldBe Nil,
      irregular.reflectionalSymmetry shouldBe 0
    )

  it should "be calculated for an isosceles triangle structure" in :
    val isosceles = List(1, 2, 1)
    allAssert(
      isosceles.rotationalSymmetry shouldBe 1,
      isosceles.reflectionalSymmetryIndices shouldBe List(0),
      isosceles.reflectionalSymmetry shouldBe 1
    )

  it should "be calculate for a specular pentagon structure" in:
    val pentagon = List(1, 1, 2, 3, 2)
    allAssert(
      pentagon.rotationalSymmetry shouldBe 1,
      pentagon.reflectionalSymmetryIndices shouldBe List(3),
      pentagon.reflectionalSymmetryAxes shouldBe List((3, (0, 1))),
      pentagon.reflectionalSymmetry shouldBe 1
    )
