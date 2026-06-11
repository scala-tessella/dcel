# ADR-0017: `Tiling` — the certified, validated tiling type

- **Status:** Accepted
- **Date:** 2026-06-11

## Context and problem statement

ADR-0003 established safe/`Unsafe` method pairs and deferred its most attractive
alternative: "a typed 'validated tiling' wrapper — a distinct type for DCELs
that have passed `validate`, carrying the `Unsafe` methods only in that scope."
The convention worked, but it is a convention: nothing stops a caller from
invoking a hidden-throw query on a broken structure, and the editor (the
flagship Scala.js consumer) has no type to hold as state that *proves* its
tiling is valid.

A 2026-06 audit sharpened the picture: only four `Unsafe` methods were public
(zero external usage), ~30 were already `private[dcel]` — but several public
queries hid throws (`gonalityTrees`, `boundarySimplePolygon`,
`Vertex.degree`), and the mutators split into two trust classes: merge/copy/
fan/repeat run the full `validate` internally, while the polygon add/delete
paths maintain validity *by construction* with cheaper inline checks. That
by-construction argument is only sound when the receiver is known valid —
which nothing in the types said.

## Decision

**Introduce `Tiling`, an opaque subtype of `TilingDCEL`, as the primary public
type: constructors return it, all seventeen mutators live on it, and the
validity-dependent queries are total on it.**

```scala
opaque type Tiling <: TilingDCEL = TilingDCEL

object Tiling:
  def from(t: TilingDCEL): Either[TilingError, Tiling] // validates; structural-empty certified directly
  private[dcel] def trusted(t: TilingDCEL): Tiling     // the greppable trust boundary
  val empty: Tiling                                    // certified blank canvas
  extension (tiling: Tiling) ...                       // the 17 mutators + promoted queries
```

- **Opaque subtype**: every raw query, export and analysis extension works on
  a `Tiling` via subsumption, at zero runtime cost (opaque types erase).
  Extensions live in the companion, so call sites need no import.
- **Mutators move, not shadow.** In Scala 3 members beat extensions, so the
  mutators could not be re-typed while remaining members of the case class;
  they are now `Tiling` extensions returning `Either[TilingError, Tiling]`,
  same names and parameters. Raw `TilingDCEL` keeps queries only — it remains
  public as the honest "uncertified DCEL-shaped data" type (input of
  `fromUntrusted`, output of `deepCopy` and `getDcelAtVertex`, hand-built
  validation fixtures).
- **No forgery**: `TilingDCEL`'s constructor is private (hence `copy`/`apply`
  too); the only public raw→`Tiling` path is `Tiling.from`, which validates.
- **The empty quirk, fixed at the root**: `validate` used to reject the
  structurally empty tiling — the bare outer face failed per-entity checks,
  even though the other three validation stages (and the test suite) already
  treated empty as valid, and `SvgMetadata.fromMetadata` carried a workaround.
  `validateCompleteness` now certifies structural emptiness (all components
  empty *and* a bare outer face — tighter than the old workaround, which
  ignored dangling outer wiring); `Tiling.from` and `fromMetadata` are plain
  delegations to `validate`.

### Trust table

Every `trusted` call asserts validity without re-validating. The complete
boundary is auditable with `grep -rn "trusted(" shared generator` (plus
`.map(trusted)` forms); each class below is exercised by the ADR-0017
property test in `PropertyBasedDCELSpec`.

| Trust class | Sites | Justification |
|---|---|---|
| Constructor guarantee | `TilingBuilder.create{SimplePolygon ×2, RegularPolygon, TriangleNet, RhombusNet, HexagonNet, Ring}` | Built valid by construction (ADR-0001/0005 geometry); documented guarantee since 0.1.0. `createHoledTriangleNet` needs no wrap: it folds certified deletions. |
| Post-validate | `TilingDCEL.fromUntrusted`, `Tiling.empty`, `SvgMetadata.fromMetadata` | `validate` (or the empty check) runs immediately before the wrap. |
| Internal validate | `maybeAddCopy` + 4 named variants, `fanAt`, `fanAround`, `repeatAlong` | The delegated machinery runs full `validate` before returning Right (TilingMultiplication). |
| By construction | `maybeAdd{Regular,Simple}Polygon{,ToBoundary}`, `maybeDelete{Vertex,Edge,Face}`, `doubleArea` | Growth/deletion pipelines maintain invariants with inline checks (boundary intersection, angle bookkeeping); full re-validation would re-pay the expensive pipeline per step. Sound only on a valid receiver — which the `Tiling` receiver type now guarantees. |
| Test assertions | `TilingEquivalencySpec` (deepCopy results) | A deep copy of a certified tiling is certified; tests state it explicitly. |

### The honest guarantee

A `Tiling` is *valid at wrap time, immutable from outside the package, and
deep-copied before every internal mutation* (ADR-0002). Two documented caveats:

1. Package-internal code *can* mutate a `Tiling`'s wiring (the structural
   `var`s are `private[dcel]`); ADR-0002's deep-copy discipline is what keeps
   published instances frozen — and gains a second motivation here, since
   in-place mutation would also stale the cached `boundarySimplePolygonUnsafe`.
2. The standard opaque-type erasure hole: a downstream
   `(x: Any) match { case t: Tiling => t }` succeeds at runtime. The same
   caveat applies to every opaque type in the codebase (`VertexId`,
   `RegularPolygon`, ...); `-Werror` keeps it out of this library.

### Surface cleanup carried along

- `gonalityTrees` (hidden `.get`) and `gonalityTreesUnsafe` left the raw type;
  on `Tiling` they are `gonalityTrees` and `gonalityTreesWithPolygons` — the
  `Unsafe` suffix is wrong when the type is the proof.
- `boundarySimplePolygon` became `private[dcel] lazy val
  boundarySimplePolygonUnsafe` (caching preserved) behind a total `Tiling`
  extension.
- `getPathUnsafe`, `maybePathUnsafe`, `adjacencyMapUnsafe` (collection
  extensions that cannot carry provenance) are `private[dcel]`, per ADR-0003's
  own default.
- `Vertex.degree`/`isThread` stay public with documented preconditions: a
  `Vertex` has no provenance, and after this refactor raw tilings in external
  hands come only from `deepCopy`/`getDcelAtVertex`, which are well-wired.

## Consequences

### Positive

- The safe/`Unsafe` convention is now a compiler guarantee on the public
  surface: a consumer holding a `Tiling` cannot reach a throwing query whose
  precondition isn't met, and the by-construction mutators cannot be applied
  to unproven receivers.
- The editor holds `Tiling` as state, starts from `Tiling.empty`, loads via
  `fromMetadata` — its whole pipeline is certified end to end.
- Zero runtime and zero delegation cost; the 764-test suite needed only
  annotation-level churn.
- A new property test re-certifies (`Tiling.from`) the result of every public
  mutator over random op sequences, turning the trust table into an executable
  audit.

### Negative / risks

- Breaking (0.2.x): the seventeen mutators are no longer members of
  `TilingDCEL`; explicit `: TilingDCEL` annotations on mutated values must
  become `Tiling`.
- The name `Tiling` collides conceptually with scala-tessella/tessella's
  `Tiling`. Accepted deliberately: within this library it is the right name
  for the primary type, and the two libraries are not meant to be mixed in
  one namespace.
- Internal discipline is still required at `trusted` call sites — the type
  proves provenance to consumers, not correctness of the by-construction
  pipelines themselves (that burden stays on the test suite, as before).

## Alternatives considered

- **Wrapper class (`final class Tiling(val dcel: TilingDCEL)`).** Rejected:
  loses subtyping, forcing delegation of the ~50-method query surface.
- **iron refinement (`TilingDCEL :| Valid`).** Rejected: a constraint is a
  boolean predicate — the rich `TilingError` diagnostics would collapse to a
  static message, `validate` is far too heavy for a constraint, and iron has
  no package-private `trusted` equivalent.
- **Keep the convention (status quo).** Rejected by this ADR's premise: the
  editor needs the proof in the type, and the receiver-validity soundness gap
  of the by-construction mutators deserved closing.

## Related

- ADR-0002 (deep-copy on mutation — the discipline the guarantee leans on).
- ADR-0003 (safe/`Unsafe` pairs — superseded in part: the public surface now
  uses the type; the convention remains for `private[dcel]` internals).
- ADR-0004 (Either-based errors — unchanged, narrowed success types).
