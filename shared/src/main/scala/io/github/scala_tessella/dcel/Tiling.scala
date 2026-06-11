package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate

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

  /** Certifies an arbitrary [[TilingDCEL]] by running [[TilingValidation.validate]] on it.
    *
    * The structurally empty tiling (no vertices, no half-edges, no inner faces) is certified directly: it is
    * a legitimate blank canvas even though the bare outer face fails per-entity validation.
    *
    * @return
    *   The certified tiling, or the [[TilingError]] reported by validation.
    */
  def from(tilingDCEL: TilingDCEL): Either[TilingError, Tiling] =
    if tilingDCEL.vertices.isEmpty && tilingDCEL.halfEdges.isEmpty && tilingDCEL.innerFaces.isEmpty then
      Right(empty)
    else
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
