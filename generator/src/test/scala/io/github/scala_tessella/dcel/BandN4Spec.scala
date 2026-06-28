package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.ProfileAutomaton as PA
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** De-risk PRECONDITION (test-before-probe): the VALIDATED cut-and-feed machinery (`representFrame` +
  * `cutProfileAt` + `profileCellConsistent`, positive-controlled on n=3 in `CutFeedSpec`) must also be SOUND
  * on n=4 cells before any de-risk classification of n=4 banded cells can be trusted. Invariant: for every
  * realized n=4 cell that `representFrame` accepts, every cut profile is a VALID cut (each vertex fan
  * reconstructs to a cell face). A `representFrame` = None is NOT a failure — it is the de-risk DATUM (band
  * axis not 30°-aligned ⇒ a representation gap), surfaced by `BandN4DerisProbe`.
  */
class BandN4Spec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)
  // n=4 type-sets analogous to the n=3 banded gap families (no dodecagons — those are separately known-hard)
  private val triHex                          = Set("3.3.3.3.3.3", "3.3.3.3.6", "3.3.6.6", "6.6.6").map(sig)
  private val triSq                           = Set("3.3.3.3.3.3", "3.3.3.4.4", "3.3.4.3.4", "4.4.4.4").map(sig)

  it should "produce CELL-CONSISTENT cut profiles for every realized n=4 cell representFrame accepts" in {
    for
      (typeSet, maxV) <- List((triHex, 10), (triSq, 11))
      (_, op)         <- BucketAssembly.enumerateBucket(typeSet, maxV, targetCount = 8).ops
      f               <- PA.representFrame(op) // skip the not-representable cells (de-risk data, not a failure)
    do
      f.profs.foreach: p =>
        withClue(s"non-cell n=4 cut profile (verts=${p.verts.size}): ") {
          PA.profileCellConsistent(p, f.rFaces, f.rv1, f.rv2) shouldBe true
        }
  }
