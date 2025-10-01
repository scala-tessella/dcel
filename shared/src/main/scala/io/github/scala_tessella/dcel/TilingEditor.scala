package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.Polygon.RegularPolygon
import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingEquivalency.* // bring .deepCopy extension into scope

final class TilingEditor private[dcel] (private var working: TilingDCEL):

  // Zero/editable base
  def this() =
    this(TilingDCEL.empty)

  // Example editing operation: add a regular polygon at the boundary.
  // It updates the editor state if successful and keeps all errors as Either.
  def addRegularPolygonToBoundary(
      onEdgeStartingWithVertexId: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, Unit] =
    working.addRegularPolygonToBoundary(onEdgeStartingWithVertexId, polygon).map { updated =>
      // Move the editor state forward
      working = updated
      ()
    }

  // Add more editor commands as needed (deleteFace, addSimplePolygon, merge, split, etc.)
  def deleteFace(faceId: FaceId): Either[TilingError, Unit] =
    working.maybeDeleteFace(faceId).map { updated =>
      working = updated
      ()
    }

  // Commit: validate and emit an immutable snapshot. Freeze ensures isolation from further mutations.
  def commit(): Either[TilingError, TilingDCEL] =
    for
      _ <- TilingDCEL.validateTopologically(working)
      _ <- TilingDCEL.validateGeometrically(working)
      _ <- TilingDCEL.validateSpatially(working)
    yield
      // Freeze by deep-copying into a fresh graph — no external code can mutate this snapshot
      working.deepCopy

object TilingEditor:

  // Public constructors
  def fromSnapshot(snapshot: TilingDCEL): TilingEditor =
    // Work on a detached copy to ensure edits never affect the caller's snapshot
    new TilingEditor(snapshot.deepCopy)

  def empty(): TilingEditor =
    new TilingEditor()
