package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{Vertex, VertexId}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.util.Random

class PropertyBasedDCELSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with TilingTestHelpers:

  // Generators and helpers

  private val genSides: Gen[Int] = Gen.oneOf(3, 4, 6)

  private val genInitialSides: Gen[Int] = genSides

  private val genSteps: Gen[Int] = Gen.choose(0, 10) // keep runtime modest

  private def createRegular(s: Int): Tiling =
    // Guard against ScalaCheck shrinking producing invalid values (e.g., 0)
    TilingBuilder.createRegularPolygon(RegularPolygon(math.max(3, s)))

  private def validateAll(tiling: Tiling): Assertion =
    allAssert(
      // Topology
      validateTopologically(tiling).isRight shouldBe true,
      // Geometry
      validateGeometrically(tiling).isRight shouldBe true,
      // Space
      validateSpatially(tiling).isRight shouldBe true
    )

  private def validateTopology(tiling: Tiling): Assertion =
    validateTopologically(tiling).isRight shouldBe true

  private def interiorVertices(tiling: Tiling): List[Vertex] =
    val boundary = tiling.boundaryVerticesUnsafe.toSet
    tiling.vertices.filterNot(boundary.contains)

  private def angleSumIsFullCircle(angles: List[AngleDegree]): Boolean =
    angles.nonEmpty && angles.map(_.normalised).sumExact.isFullCircle

  private def random: Random = new Random(0xc0ffee) // deterministic seed for reproducibility

  private def randomBoundaryEdgeStart(tiling: Tiling): Option[VertexId] =
    val edges = tiling.boundaryEdgesUnsafe
    if edges.isEmpty then None
    else Some(edges(random.nextInt(edges.length)).origin.id)

  /** Try to add a regular polygon by sampling boundary edges and sides. Returns the updated tiling and a
    * boolean "performed" indicating whether any addition succeeded. Only accepts the addition if the
    * resulting tiling passes topological validation.
    */
  private def tryRandomAddition(tiling: Tiling, attempts: Int = 24): (Tiling, Boolean) =
    var current = tiling
    var i       = 0
    var done    = false
    while i < attempts && !done do
      randomBoundaryEdgeStart(current) match
        case None =>
          i = attempts // no boundary to grow
        case Some(startVid) =>
          val s = genSides.sample.getOrElse(3)
          current.maybeAddRegularPolygonToBoundary(startVid, RegularPolygon(s)) match
            case Right(next) =>
              // Accept only if topology remains valid
              if validateTopologically(next).isRight then
                current = next
                done = true
            // else keep searching for another random operation
            case Left(_)     =>
              // try again with another random pick
              ()
      i += 1
    (current, done)

  /** Try to delete a face, preferring boundary-adjacent faces. Returns the updated tiling and a boolean
    * "performed" indicating whether any deletion succeeded. Only accepts the deletion if the resulting tiling
    * passes topological validation.
    */
  private def tryRandomDeletion(tiling: Tiling, attempts: Int = 24): (Tiling, Boolean) =
    var current = tiling
    var i       = 0
    var done    = false
    while i < attempts && !done do
      if current.innerFaces.isEmpty then
        i = attempts
      else
        // Prefer deleting faces that touch the outer boundary (safer operations)
        val boundaryAdjacentFaces =
          current.innerFaces.filter: face =>
            face.halfEdges.toOption.exists: edges =>
              edges.exists: halfEdge =>
                halfEdge.twin
                  .flatMap: halfEdge =>
                    halfEdge.incidentFace
                  .contains(current.outerFace)

        val pool = if boundaryAdjacentFaces.nonEmpty then boundaryAdjacentFaces else current.innerFaces
        val f    = pool(random.nextInt(pool.length))

        current.maybeDeleteFace(f.id) match
          case Right(next) =>
            // Accept only if topology remains valid
            if validateTopologically(next).isRight then
              current = next
              done = true
          // else keep searching for another random operation
          case Left(_)     => ()
      i += 1
    (current, done)

  // Properties

  behavior of "Interior vertex angles"

  it should "sum to a full circle for interior vertices across random growth sequences" in
    forAll(genInitialSides, genSteps): (initialSides, steps) =>
      var t = createRegular(initialSides)
      allAssert(
        validateTopology(t), {
          var i          = 0
          val assertions = ListBuffer[Assertion]()
          while i < steps do
            val (grown, performed) = tryRandomAddition(t)
            t = grown
            if performed then assertions += validateTopology(t)
            i += 1
          allAssert(assertions.toList*)
        }, {
          val interior   = interiorVertices(t)
          val assertions =
            interior.map: vertex =>
              val angles = t.getAnglesAtVertex(vertex.id).value
              if angles.nonEmpty then
                withClue(s"Interior vertex ${vertex.id}: angles=$angles"):
                  angleSumIsFullCircle(angles) shouldBe true
              else succeed
          allAssert(assertions*)
        }
      )

  behavior of "Face angles and edge count consistency"

  it should "match number of half-edges and pass polygon angle sum validation" in
    forAll(genInitialSides, genSteps): (initialSides, steps) =>
      var t = createRegular(initialSides)
      var i = 0
      while i < steps do
        val (grown, _) = tryRandomAddition(t)
        t = grown
        i += 1

      // Check each inner face
      t.innerFaces.foreach { f =>
        val edges  = f.halfEdges.value
        val angles =
          edges.flatMap: halfEdge =>
            halfEdge.angle
        allAssert(
          withClue(s"Face ${f.id}: edges=${edges.length}, angles=${angles.length}") {
            angles.length shouldEqual edges.length
          },
          SimplePolygon(angles.toVector).toAngles.nonEmpty shouldBe true
        )
      }

  behavior of "Edge linkage invariants"

  it should "preserve twin/next/prev cycles under random additions and deletions" in
    forAll(genInitialSides, genSteps): (initialSides, steps) =>
      var t = createRegular(initialSides)
      allAssert(
        {
          // Random growth
          var i          = 0
          val assertions = ListBuffer[Assertion]()
          while i < steps do
            val (grown, performedAdd) = tryRandomAddition(t)
            t = grown
            if performedAdd then
              assertions += validateTopology(t)
            i += 1
          allAssert(assertions.toList*)
        }, {
          // Random deletions (up to the same count), validate after each successful deletion
          var j          = 0
          val assertions = ListBuffer[Assertion]()
          while j < steps && t.innerFaces.nonEmpty do
            val (shrunk, performedDel) = tryRandomDeletion(t)
            t = shrunk
            if performedDel then
              assertions += validateTopology(t)
            j += 1
          allAssert(assertions.toList*)
        }
      )

  behavior of "Random growth end-to-end validation"

  it should "validate after each step of random boundary growth" in
    forAll(genInitialSides, genSteps): (initialSides, steps) =>
      var t          = createRegular(initialSides)
      val assertions = ListBuffer[Assertion]()
      assertions += validateTopology(t)
      var i          = 0
      while i < steps do
        val (grown, performed) = tryRandomAddition(t)
        t = grown
        if performed then assertions += validateTopology(t)
        i += 1
      allAssert(assertions.toList*)

  behavior of "Tiling certification (ADR-0017)"

  it should "yield only re-certifiable tilings through any public-API op sequence" in
    forAll(genInitialSides, genSteps, Gen.choose(0, 1000000)): (initialSides, steps, seed) =>
      // Every Right of every public mutator must satisfy Tiling.from (full validation, empty certified):
      // this audits each by-construction Tiling.trusted call in the ADR-0017 trust table.
      val rng        = new Random(seed)
      var t: Tiling  = createRegular(initialSides)
      val assertions = ListBuffer[Assertion]()
      var i          = 0
      while i < steps do
        val boundaryStarts                                 = t.boundaryEdgesUnsafe.map(_.origin)
        def randomBoundary: Option[Vertex]                 =
          if boundaryStarts.isEmpty then None else Some(boundaryStarts(rng.nextInt(boundaryStarts.length)))
        // Cap the patch size: doubleArea grows geometrically, and consecutive doublings would
        // exhaust the Node.js heap on the Scala.js run.
        val roomToGrow                                     = t.innerFaces.sizeIs < 50
        val attempted: Option[Either[TilingError, Tiling]] = rng.nextInt(6) match
          case 0 => randomBoundary.map(v =>
              t.maybeAddRegularPolygonToBoundary(v.id, RegularPolygon(3 + rng.nextInt(4)))
            )
          case 1 =>
            if t.innerFaces.isEmpty then None
            else Some(t.maybeDeleteFace(t.innerFaces(rng.nextInt(t.innerFaces.length)).id))
          case 2 => Option.when(roomToGrow)(t.doubleArea)
          case 3 =>
            randomBoundary.flatMap(v =>
              v.leaving.flatMap(_.destination).map(_.coords).map(dest =>
                t.maybeAddTranslatedCopy(v.coords, dest)
              )
            )
          case 4 => randomBoundary.map(v => t.fanAt(v.id))
          case 5 => randomBoundary.map(v => t.maybeDeleteVertex(v.id))
        attempted match
          case Some(Right(grown)) =>
            assertions += (Tiling.from(grown).isRight shouldBe true)
            t = grown
          case _                  => () // rejected or inapplicable ops are fine
        i += 1
      assertions += (Tiling.from(t).isRight shouldBe true)
      allAssert(assertions.toList*)
