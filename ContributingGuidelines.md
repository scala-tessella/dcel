# Contributing guidelines for DCEL tiling (error handling and “Unsafe” conventions)

This project uses a typed and consistent error-handling strategy, and clearly separates safe/unsafe APIs.

- Error model
  - All fallible public methods return `Either[TilingError, A]`.
  - Use specific error constructors:
    - `ValidationError` for invalid data/parameters (e.g., angles sum mismatch, invalid sides).
    - `TopologyError` for DCEL invariants and connectivity violations.
    - `NotFoundError` for lookups that fail (vertex/edge/face by id).
  - Prefer composing/aggregating errors via helpers (e.g., a `combineErrors` utility) rather than concatenating strings.

- Opaque IDs
  - Use `VertexId` and `FaceId` (opaque types) in all public APIs, not raw `String`.
  - If you receive raw strings at module boundaries, convert ASAP via `VertexId("...")` or `FaceId("...")`.

- Safe vs Unsafe methods
  - “Unsafe” methods may call `.get` on `Option` or assume DCEL invariants, and are intended for internal/trusted contexts or performance-critical code paths.
  - Non-Unsafe (public) methods:
    - Must avoid `.get` or throwing; return `Either[TilingError, A]` where appropriate.
    - Should be total and document preconditions/postconditions if any.
  - Naming:
    - Suffix internal helpers with `Unsafe` consistently (e.g., `faceTraversalUnsafe`, `incidentEdgesUnsafe`).
    - Keep Unsafe methods `private[dcel]` or otherwise non-public where possible.

- Testing
  - Unit tests should validate both API tiers:
    - Safe APIs: check success and error branches.
    - Unsafe APIs: are allowed to assume invariants; tests should set up valid DCEL states before using them.
  - Where possible, include property tests for DCEL invariants (e.g., angle sums, twin/next/prev consistency).

- Documentation hygiene
  - Keep Scaladoc synchronized with signatures. If a method returns `Either[TilingError, A]`, the doc should mention `TilingError` (not `String`).
  - Document side effects and mutation points (e.g., in builders/editors).

- Style & CI
  - Prefer explicit types on public APIs.
  - Use the same error ADT across modules; avoid introducing `Either[String, A]` in main sources.
  - Run tests for both JVM and JS targets; keep formatting/lint rules consistent.

Thank you for contributing!
