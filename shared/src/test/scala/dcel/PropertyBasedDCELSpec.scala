package dcel

import dcel.BigDecimalGeometry.AngleDegree
import dcel.Polygon.SimplePolygon

import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

class PropertyBasedDCELSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with EitherValues
    with TilingTestHelpers:

  // Generators and helpers

  private val genSides: Gen[Int] = Gen.oneOf(3, 4, 6)

  private val genInitialSides: Gen[Int] = genSides

  private val genSteps: Gen[Int] = Gen.choose(0, 10) // keep runtime modest

  private def createRegular(s: Int): TilingDCEL =
    // Guard against ScalaCheck shrinking producing invalid values (e.g., 0)
    TilingBuilder.createRegularPolygon(math.max(3, s)).value

  private def validateAll(tiling: TilingDCEL): Unit =
    // Topology
    TilingDCEL.validateTopologically(tiling).isRight shouldBe true
    // Geometry
    TilingDCEL.validateGeometrically(tiling).isRight shouldBe true
    // Space
    TilingDCEL.validateSpatially(tiling).isRight shouldBe true

  private def interiorVertices(tiling: TilingDCEL): List[Vertex] =
    val boundary = tiling.boundaryVerticesUnsafe.toSet
    tiling.vertices.filterNot(boundary.contains)

  private def angleSumIsFullCircle(angles: List[AngleDegree]): Boolean =
    angles.nonEmpty && angles.map(_.normalised).sum2.isFullCircle

  private def random: Random = new Random(0xC0FFEE) // deterministic seed for reproducibility

  private def randomBoundaryEdgeStart(tiling: TilingDCEL): Option[VertexId] =
    val edges = tiling.boundaryEdgesUnsafe
    if edges.isEmpty then None
    else Some(edges(random.nextInt(edges.length)).origin.id)

  /**
   * Try to add a regular polygon by sampling boundary edges and sides.
   * Returns the updated tiling and a boolean "performed" indicating whether any addition succeeded.
   */
  private def tryRandomAddition(tiling: TilingDCEL, attempts: Int = 12): (TilingDCEL, Boolean) =
    var current = tiling
    var i = 0
    var done = false
    while i < attempts && !done do
      randomBoundaryEdgeStart(current) match
        case None =>
          i = attempts // no boundary to grow
        case Some(startVid) =>
          val s = genSides.sample.getOrElse(3)
          current.maybeAddRegularPolygonToBoundary(startVid, s) match
            case Right(next) =>
              current = next
              done = true
            case Left(_) =>
              // try again with another random pick
              ()
      i += 1
    (current, done)

  private def tryRandomDeletion(tiling: TilingDCEL, attempts: Int = 12): (TilingDCEL, Boolean) =
    var current = tiling
    var i = 0
    var done = false
    while i < attempts && !done do
      if current.innerFaces.isEmpty then
        i = attempts
      else
        val f = current.innerFaces(random.nextInt(current.innerFaces.length))
        current.maybeDeleteFace(f.id) match
          case Right(next) =>
            current = next
            done = true
          case Left(_) => ()
      i += 1
    (current, done)

  // Properties

  behavior of "Interior vertex angles"

  it should "sum to a full circle for interior vertices across random growth sequences" in {
    forAll(genInitialSides, genSteps) { (initialSides, steps) =>
      var t = createRegular(initialSides)
      validateAll(t)

      var i = 0
      while i < steps do
        val (grown, performed) = tryRandomAddition(t)
        t = grown
        if performed then validateAll(t)
        i += 1

      val interior = interiorVertices(t)
      all(interior.map(_.id.value)) should not be empty

      interior.foreach { v =>
        val angles = t.getAnglesAtVertex(v.id).value
        withClue(s"Interior vertex ${v.id}: angles=$angles") {
          angleSumIsFullCircle(angles) shouldBe true
        }
      }
    }
  }

  behavior of "Face angles and edge count consistency"

  it should "match number of half-edges and pass polygon angle sum validation" in {
    forAll(genInitialSides, genSteps) { (initialSides, steps) =>
      var t = createRegular(initialSides)
      var i = 0
      while i < steps do
        val (grown, _) = tryRandomAddition(t)
        t = grown
        i += 1

      // Check each inner face
      t.innerFaces.foreach { f =>
        val edges = f.halfEdges.value
        val angles = edges.flatMap(_.angle)
        withClue(s"Face ${f.id}: edges=${edges.length}, angles=${angles.length}") {
          angles.length shouldEqual edges.length
        }
        SimplePolygon.validatePolygonAngles(angles).isRight shouldBe true
      }
    }
  }

  behavior of "Edge linkage invariants"

  it should "preserve twin/next/prev cycles under random additions and deletions" in {
    forAll(genInitialSides, genSteps) { (initialSides, steps) =>
      var t = createRegular(initialSides)

      // Random growth
      var i = 0
      while i < steps do
        val (grown, performedAdd) = tryRandomAddition(t)
        t = grown
        if performedAdd then
          TilingDCEL.validateTopologically(t).isRight shouldBe true
        i += 1

      // Random deletions (up to the same count), validate after each successful deletion
      var j = 0
      while j < steps && t.innerFaces.nonEmpty do
        val (shrunk, performedDel) = tryRandomDeletion(t)
        t = shrunk
        if performedDel then
          TilingDCEL.validateTopologically(t).isRight shouldBe true
        j += 1
    }
  }

  behavior of "Random growth end-to-end validation"

  it should "validate after each step of random boundary growth" in {
    forAll(genInitialSides, genSteps) { (initialSides, steps) =>
      var t = createRegular(initialSides)
      validateAll(t)

      var i = 0
      while i < steps do
        val (grown, performed) = tryRandomAddition(t)
        t = grown
        if performed then validateAll(t)
        i += 1
    }
  }
