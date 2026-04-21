# ADR-0004: `Either[TilingError, A]` with a sealed error ADT

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

Tiling operations can fail for reasons that are structurally different
from each other:

- The structure is incomplete (a half-edge has no `twin` yet).
- The structure is locally wrong (a `next`/`prev` cycle doesn't close).
- The geometry is wrong (angles around a vertex exceed 360°; a new
  polygon's boundary would self-intersect).
- The spatial layout is wrong (two faces overlap; a vertex coincides with
  an edge interior).
- A lookup misses (no vertex with the given `VertexId`).
- An input string or user value is malformed.

These categories have different consumers: validation wants to report
each class of failure distinctly; public mutation needs a single
`Either` to flow through a `for`-comprehension; SVG round-trip wants
parse failures to remain distinguishable from topology failures.

Using exceptions for flow control would force every `for` to wrap a
`Try`, discard the category at the catch site, and couple the public API
to JVM semantics (Scala.js exceptions behave differently).

## Decision

**All fallible public operations return `Either[TilingError, A]`, where
`TilingError` is a sealed trait with six final subtypes, each carrying
the minimum information needed to report the failure.**

```scala
sealed trait TilingError:
  def message: String

case class ValidationError(message: String)          extends TilingError
case class IncompleteError(message: String)          extends TilingError
case class TopologyError(message: String)            extends TilingError
case class GeometryError(message: String)            extends TilingError
case class SpatialError(message: String)             extends TilingError
case class NotFoundError(entity: String, id: String) extends TilingError:
  def message: String = s"$entity with ID '$id' not found."
```

(`TilingError.scala`).

Combining multiple messages into a single error of a given category is
done through an `ErrorCategory` enum that bundles a human-readable label
with the per-category factory:

```scala
enum ErrorCategory(val label: String, val build: String => TilingError):
  case Validation extends ErrorCategory("validation",   ValidationError(_))
  case Incomplete extends ErrorCategory("completeness", IncompleteError(_))
  case Topology   extends ErrorCategory("topology",     TopologyError(_))
  case Geometry   extends ErrorCategory("geometry",     GeometryError(_))
  case Spatial    extends ErrorCategory("spatial",      SpatialError(_))

def combineErrors(errors: List[String], category: ErrorCategory): TilingError =
  errors match
    case single :: Nil => category.build(single)
    case many          => category.build(s"Multiple ${category.label} errors: ${many.mkString("; ")}")
```

`NotFoundError` is intentionally excluded from `ErrorCategory` — it
carries two fields and doesn't fit the `String => TilingError` factory
shape.

## Consequences

**Positive**

- Composition: `for { a <- opA; b <- opB(a) } yield …` threads tiling
  errors uniformly.
- Categories line up with the validation pipeline
  (`TilingValidation.validateCompleteness`, `validateTopologically`,
  `validateGeometrically`, `validateSpatially`, `validate`), making the
  diagnostics legible.
- `ErrorCategory` is reusable outside `combineErrors` (logging,
  telemetry, test matchers), which the old discriminator trick prevented.

**Negative / tradeoffs**

- Six categories is a lot if you're writing generic error-handling code.
  In practice callers usually pattern-match on one or two.
- The `message` field is a plain `String`; we don't structure the payload
  beyond the category. For richer diagnostics (offending vertex id, line
  number in the SVG) this is a known limitation — add fields to the
  specific `*Error` case rather than overloading the string.
- No error accumulation by default. `combineErrors` accumulates strings
  at the call site (e.g. in validation) before wrapping, which is
  pragmatic but not compositional. Adopting `cats.data.Validated` would
  fix that but adds a cats dependency for a single use case.

## How to apply

- New fallible public methods return `Either[TilingError, A]`.
- Choose the category that matches the kind of invariant being violated.
  If it's genuinely new, add it in a follow-up ADR, not silently.
- When aggregating a buffer of error strings in a validator, use
  `TilingError.combineErrors(errors, ErrorCategory.<Category>)`.
- For "entity not found" cases, use `NotFoundError(entityLabel, id)`
  directly — don't try to funnel it through `ErrorCategory`.

## Alternatives considered

- **Exceptions.** Rejected: flow control, no type-level safety, Scala.js
  fragility, worse composition.
- **`Try[A]`.** Rejected: widens the error type to `Throwable` and loses
  the category.
- **`cats.data.Validated[TilingError, A]` / `EitherNel`.** Would buy us
  error accumulation without manual `List[String]` aggregation. Deferred
  because the only callers that accumulate today do so in validators
  that already have a buffer. Revisit if more call sites want it.
- **Opaque `type TilingError = String`.** Simpler but discards the
  categorisation that the validation pipeline depends on.

## History

- The previous `combineErrors(errors: List[String], f: String => TilingError)`
  signature used `f.apply("whatever")` to recover the category label —
  fragile. Replaced by the `ErrorCategory` enum; the six single-argument
  factory helpers on `TilingError`'s companion (`validation`,
  `incomplete`, `topology`, `geometry`, `spatial`, `notFound`) were
  deleted at the same time since they existed only to feed that signature.

## Related

- ADR-0003 (safe/`Unsafe` pairs — the `Unsafe` lane skips the `Either`).
