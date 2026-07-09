# ADR-0043: Extract the enumeration work into the `uniform-tilings` sibling library

- **Status:** Accepted (user decision 2026-07-09; extraction executed the same day). Final dcel-side ADR of
  the A068600 line — the trail continues in the new repository.
- **Follows:** [[0039-constraint-first-pivot-and-the-12n-bound]] … [[0042-drat-certification]].

## Context and decision

The A068600/uniform-tilings enumeration outgrew its incubator: it is now a research instrument (Delaney–Dress
symbols + SAT assembly, fair type-set derivation, the 12n bound, an SVG atlas, corrections to the published
record) with an audience, dependency profile (SAT4J, JVM-only), release cadence, and citability need all
different from the DCEL data-structure library whose flagship consumer is the Scala.js editor. Measured
coupling: the entire sound pipeline's only DCEL dependency was `VertexTypes`' use of `geometry.AngleDegree` —
replaced in the extraction by exact `Frac` rationals, with the valid-signature list now DERIVED
(`TypeCompatibility.viableFigures`) instead of hardcoded.

**Decision (user):** new library **`uniform-tilings`** under `io.github.scala-tessella`; the audit trail is
GREATLY COMPACTED and copied there with fresh numbering from 0001; the superseded engines (grower, fixed-Λ,
bounded-V, oriented-slice, band/profile automaton) STAY HERE as the archived research record and
cross-validation witnesses.

## What moved (copied; dcel keeps its history)

`Frac`, signatures/normalization, `TypeCompatibility`, `DelaneySymbols` (trimmed of the superseded
oriented-slice/corona generator blocks), `SymbolAssembly`, `SymbolRenderer`, the reference data
(A068600 + the (n,m) table with provenance incl. our corrected row 8), the gate/fixture/property specs, the
gate probes, and the SVG atlas. Module shape: `core` (pure, cross-target-ready) + `solver` (JVM, SAT4J).

## What stays

Everything else: the DCEL library proper, the archived engines and their ADRs 0018–0042 (append-only, per
the repo policy), the probes and cross-engine specs that reference them. `generator/` remains as archive —
new enumeration work happens in `uniform-tilings`.
