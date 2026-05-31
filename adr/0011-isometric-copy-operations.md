# ADR-0011: Isometric copy operations (mirror / translate / rotate / glide reflect)

- **Status:** Proposed
- **Date:** 2026-05-30

## Context and problem statement

We want new public operations that grow a tiling by adding a copy of
*itself* under a rigid motion:

1. **Mirrored copy** — reflect across a line.
2. **Translated copy** — slide by a vector.
3. **Rotated copy** — rotate about a centre.
4. **Glide-reflected copy** — reflect across a line, then slide along it
   (added once the shared path was proven; see Consequences).

The user-facing contract for all of them is the same:

> The result is valid only if the vertices and edges of the new copy do not
> conflict with the existing ones — they are either entirely **outside** the
> existing tiling, or they **exactly follow** its composition (coincident
> vertices, collinear shared edges, matching faces). The merged result must
> also be a valid tiling under the existing validity rules
> (`TilingValidation.validate`).

Two facts make this a *generalisation* exercise rather than several new
algorithms:

- **All are isometries** (distance-preserving rigid motions). Translation
  and rotation preserve orientation (det +1); reflection and glide reflection
  reverse it (det −1).
- **Each already exists as a private, specialised internal:**

  | Operation | Existing internal | Location |
  |-----------|-------------------|----------|
  | translate | `rawDouble(origin, repeat, withInversion)` | `TilingAddition.scala:514` |
  | rotate    | `rawFan` / `translatedCopy` + `rotateAround` | `TilingAddition.scala:582-610` |
  | mirror    | `verticallyReflectedCopy` (horizontal axis only) | `TilingEquivalency.scala:185` |

  All three converge on the same two steps:
  `translatedDouble(coordsTransformer, vertexIdTransformer, faceIdTransformer)`
  (`TilingEquivalency.scala:165`) to build a transformed, fresh-id copy, then
  `mergeTilings(base, copy)` (`TilingMerge.scala:18`) to weld it on.

`mergeTilings` already implements the "outside, or exactly follows" contract at
the **vertex and edge** level: it unifies vertices that coincide within
`ACCURACY = 1e-10` (`TilingMerge.scala:26-27`) and collapses seam edges. Partial
conflicts (edges that properly cross, a vertex landing mid-edge, interior angle
sums exceeding 360°) are rejected downstream by `validateSpatially` /
`validateGeometrically`.

## Decision

**Model the operations as one primitive over an `Isometry` ADT, with thin
convenience wrappers.** Parameters are arbitrary points (`BigPoint`), not
vertices — the transforms are pure geometry, decoupled from the existing vertex
set; validity is enforced entirely by `mergeTilings` + `validate`.

```
enum Isometry:
  case Translation(from: BigPoint, to: BigPoint)
  case Rotation(center: BigPoint, degrees: AngleDegree)
  case Reflection(axisP1: BigPoint, axisP2: BigPoint)
  case GlideReflection(axisP1: BigPoint, axisP2: BigPoint)

def maybeAddCopy(isometry: Isometry): Either[TilingError, TilingDCEL]

// convenience wrappers, matching existing `maybeAddRegularPolygon` naming:
def maybeAddTranslatedCopy(from: BigPoint, to: BigPoint): Either[TilingError, TilingDCEL]
def maybeAddRotatedCopy(center: BigPoint, degrees: AngleDegree): Either[TilingError, TilingDCEL]
def maybeAddMirroredCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, TilingDCEL]
def maybeAddGlideReflectedCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, TilingDCEL]
```

The glide reflection's slide vector is the axis itself (`axisP1 → axisP2`): one
reflection across the line plus a translation along it, which is the canonical
parameterisation and keeps the wrapper signature identical to mirror's.

### Parameter contract

| Operation | Parameters | Transform | Exactness |
|-----------|------------|-----------|-----------|
| translate | `from`, `to: BigPoint` | `p => p + (to − from)` | **exact** (BigDecimal subtraction/addition) |
| rotate    | `center: BigPoint`, `degrees: AngleDegree` | `rotateAround(center, …)` | trig float error (~1e-15) |
| mirror    | `axisP1`, `axisP2: BigPoint` | reflect across the line + **winding reversal** | float error for an oblique axis |
| glide     | `axisP1`, `axisP2: BigPoint` | reflect across the line, then translate by `axisP2 − axisP1` + **winding reversal** | float error for an oblique axis |

Points may be derived at the call site from the tiling without special API
support: edge midpoints via `BigLineSegment.midPoint`, face centres via
`centroid`, or a vertex's own `coords`. Midpoints and centroids are exact
BigDecimal averages, so only the rotation trig and oblique-reflection arithmetic
introduce floating error — comfortably under `ACCURACY = 1e-10`.

### Rotation sign convention: clockwise **as rendered**

`degrees` is **positive clockwise, negative counterclockwise, as seen in the
rendered SVG** (i.e. what the downstream editor user sees).

This needs stating because the codebase carries two orientations:

- **Internal coordinates are y-up** (standard math Cartesian). `angleTo` uses
  `atan2(dy, dx)` and `rotateAround` (`TilingAddition.scala:582-591`) uses the
  standard rotation matrix — both **counterclockwise-positive in the internal
  plane**.
- **SVG export flips y** with `.flippedY` (`SvgDsl.scala:86`,
  `TilingSVG.scala:31`) because SVG is y-down.

After the flip, a counterclockwise-positive internal rotation appears
**clockwise on screen**. Therefore a positive `degrees` value maps directly to
the existing `rotateAround` with **no sign change** — the public API converts
degrees to `BigRadian` and forwards. (Had we defined the sign against internal
coordinates instead, we would have to negate; we deliberately do not, because
the editor user's frame is the rendered one.)

### Reflection is the structurally hard case

Translation and rotation are orientation-preserving: a copy is the same DCEL
wiring with transformed coordinates, so it can flow straight through
`translatedDouble`. Reflection reverses orientation: transforming coordinates
alone would leave every inner face winding clockwise and break the
"face-on-the-left" invariant. `verticallyReflectedCopy` already handles this
with explicit winding-reversal wiring — swap `next`↔`prev`, route through twins,
recompute angles (`TilingEquivalency.scala:203-250`) — but is hardcoded to a
horizontal axis at the bounding-box midpoint.

**Plan:** generalise `verticallyReflectedCopy` so the *coordinate* transform is
"reflect across the line through `axisP1`/`axisP2`" while keeping the
axis-independent winding-reversal wiring. The structural part does not depend on
the axis; only the `coordsTransformer` changes.

Glide reflection then needs **no new structural code**: it is the same
orientation-reversing wiring with a `coordsTransformer` that composes the
reflection with a translation along the axis. Because the winding-reversal
machinery is already axis-independent, the only delta from mirror is the
post-reflection slide in the coordinate function.

## Consequences

### Positive

- One conflict-check + merge + validate path, exercised by three transforms.
- Reuses proven internals (`translatedDouble`, `mergeTilings`, `rotateAround`,
  the reflection wiring) rather than introducing parallel geometry code.
- Point-based parameters keep the core free of id plumbing and make the
  operations composable — **glide reflection** is realised exactly this way, as
  reflection ∘ translation reusing the reflection wiring with no new structural
  code.
- The `Isometry` ADT gives a single extension point and a single place to
  document the validity contract.

### Negative / risks

- **Face-level overlap is an open question.** `mergeTilings` unifies vertices
  and collapses seam edges, but keeps **all inner faces from both inputs**
  (`TilingMerge.scala:67-83`). A copy that *fully overlaps* an existing region
  (e.g. mirroring across an axis that is already a symmetry) may yield
  **duplicate coincident faces** rather than one merged face. This must be
  settled in step 1 of the build order (below): either dedupe overlapping faces
  inside `mergeTilings`, or detect-and-reject in the new method. A regression
  test (mirror a symmetric tiling onto itself; assert the face count is
  unchanged, not doubled) pins it down.
- **Rotation's valid input space is narrow and discrete.** For a rotated copy to
  "exactly follow the composition," the angle must be a local symmetry (multiples
  of 60° / 90° / etc. by configuration). Most arbitrary angles are *correctly*
  rejected; this is documented behaviour, not a bug.
- **Trig float error** enters rotation and oblique reflection via
  `Math.cos/sin` on `Double` (the ADR-0010 convention). At ~1e-15 it sits well
  under the 1e-10 coincidence threshold, but compounded rotations or
  deliberately near-miss inputs warrant a precision-stress test.

## Validity contract (enforced, not assumed)

Every operation returns `Either[TilingError, TilingDCEL]` and runs the full
`TilingValidation.validate` after the merge. A `Right` result is guaranteed to
satisfy all existing validity stages (completeness, topology, geometry,
spatial). Inputs that produce a partial overlap, a proper edge crossing, an
out-of-range vertex angle sum, or a degenerate double face return a `Left` with
the relevant `TilingError`.

## Testing strategy

`TilingSymmetry` provides a built-in oracle — known-correct answers for the
idempotent cases:

- **Reflection idempotence** — mirror across an axis reported by
  `reflectionalVertexIds` must reproduce the original (copy lands on itself;
  face count unchanged). This is also the direct test for the face-dedup risk.
- **Rotation idempotence** — rotate by `360° / rotationalSymmetryOrder` must
  reproduce the original.
- **Translation extension** — translate a periodic tiling by a lattice vector
  and merge cleanly (the path `doubleArea` / `rawDouble` already validate).
- **Glide reflection** — no idempotent oracle (a glide is never an involution),
  so it is checked structurally: a glide along a tiling's symmetry axis lands a
  half-offset copy that welds cleanly, and glide = mirror only when the slide
  vector is zero. Covered by `TilingGlideReflectedCopySpec`.
- **Constructions** — mirror a triangle across an edge → rhombus; mirror a
  square strip → grid.
- **Adversarial "must reject" set** — proper edge crossing, a vertex landing
  mid-edge, angle sum > 360°, and a near-miss just *outside* 1e-10.
- **Property-based** (`PropertyBasedDCELSpec`) — any `Right` output passes
  `validate`.
- **Visual regression** — lock results with `.svg` resources, as the existing
  suite does.

## Build order

Smallest-risk-first, on this branch:

1. **Translate** — exact arithmetic; `rawDouble` nearly does it. Lands the
   public merge/validate/conflict-check plumbing **and settles the face-dedup
   question** (affects all three).
2. **Rotate** — adds the degrees→radians wrapper, the clockwise-as-rendered
   convention, and the trig-precision concern.
3. **Mirror** — generalise `verticallyReflectedCopy` to an arbitrary axis,
   keeping the winding-reversal wiring.
4. **Unify** — refactor the operations into `maybeAddCopy(isometry: Isometry)`
   once the shared path is proven.
5. **Glide reflect** — drop-in fourth isometry: reuse the mirror's
   winding-reversal path with an axis-translation added to the coordinate
   transform. No new structural code, only the new `coordsTransformer` and a
   wrapper.

## Alternatives considered

- **Three independent methods, no shared ADT.** Rejected: they would duplicate
  the merge/validate/conflict path three times and drift apart. The orientation
  split (reflection vs the other two) is a transform-builder detail, not a
  reason to fork the whole pipeline.
- **Vertex-id parameters (the original proposal).** Rejected in favour of
  `BigPoint`: arbitrary centres/axes (edge midpoints, face centres) are not
  vertices, and points keep the core decoupled from the vertex set. Callers can
  still pass `vertex.coords`.
- **Defining the rotation sign against internal y-up coordinates.** Rejected:
  the meaningful frame is the rendered editor view. Defining "clockwise" there
  also means the existing `rotateAround` needs no sign change.
- **Including scaling as a fourth operation.** Rejected: scaling breaks the
  unit-edge invariant that construction and validation enforce. The family is
  restricted to isometries by design.
