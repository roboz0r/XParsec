# `extern` is self-evident — retire `IntrinsicMarkers`

*Prerequisite for both `per-target-manifest-plan.md` and `platform-facts-plan.md`. Small,
self-contained, and behaviour-preserving on the current tree. No `.fsi` edits, no parser
change.*

## The defect

An `extern` type is classified as an intrinsic by whether a `(# "<repr>" #)`-bearing body
**happens to exist on disk for some target**, not by what the contract declares.

`buildProviderWith` (`ReferencedProject.fs:471-511`) walks `everyBody` — the UNION of every
`[targets.<t>] impl` list — and adds each repr-bearing type's short name to
`ctx.IntrinsicMarkers`, while filling `ctx.IntrinsicReprs` only from THIS target's bodies. The
extraction then reads `ctx.IntrinsicMarkers.Contains short` (`VesperLib.fs:1122`) to decide
between publishing `ExternalTypeShape.Intrinsic` and an opaque `Class`.

Two consequences:

- **Target-blind by construction.** `decimal` publishes on JS as `Intrinsic` with
  `IntrinsicPlatform.Unsupported "js"` only because `prim-types-decimal.clr.fs` exists. A
  manifest that cannot see the other target's file list cannot reach that verdict — which is
  exactly what the per-target manifest split does.
- **Silently wrong under deletion.** Remove `prim-types-decimal.clr.fs` and `decimal` degrades
  to an opaque `Class` on *both* targets: `1M` loses its diagnostic everywhere, with no error
  raised anywhere.

## The rule already exists — one layer up

Intrinsic-ness is not a derived fact. It is what `extern` MEANS, and the conformance pass
already enforces it:

- `Conformance.ConformanceError.ExternWithoutIntrinsic` rejects an `extern` in a `.fsi` paired
  with a `.fs` that does not bind `(# … #)` (`ConformanceTests.fs:79-87`).
- Where no body is paired, `ConformancePass.fs:186-188` holds the `.fsi` to owe nothing —
  "it owes a `.fs` unless EVERY declaration is satisfied without one, which an `extern` or
  transparent abbreviation always is."

That is precisely the split the extractor should be making: `extern` declares that the platform
supplies the representation; whether THIS platform supplies it is the separate, per-target
question that `IntrinsicReprs` already answers.

## The change

1. **`isIntrinsic` becomes unconditionally true** for the `TypeSignature.Extern` arm
   (`VesperLib.fs:1114`). The declaration is the fact.
2. **Delete `ctx.IntrinsicMarkers`** (`VesperLib/TyparCapture.fs:136`) and the `everyBody` loop
   that fills it (`ReferencedProject.fs:477-484`). `IntrinsicReprs` stays, filled from THIS
   target's `resolveImpl` only, and picks `IntrinsicPlatform.Repr` vs `Unsupported`
   (`VesperLib.fs:1127-1136`).
3. **Delete the `Class` fallback** (`VesperLib.fs:1178-1180`). It registers a shape for a state
   the conformance pass rejects. If a bare `extern` reaching a non-intrinsic classification is
   worth catching at extraction time, make it the hard error it already is one layer up rather
   than a silently-degraded `Class`.

`extern class` / `extern interface` keep their existing handling. Those tags carry **surface**
information — heritable primitive, capability interface — not intrinsic-ness, so they are
orthogonal and unaffected. Note both already route around `registerIntrinsic` when they carry
members (`VesperLib.fs:1155-1172`, gated on `IntrinsicReprs`, not on markers).

## Behaviour delta: none on the real tree

The only types this moves from `Class` to `Intrinsic + Unsupported` are `extern`s with a repr on
NO target. There are none in `src/Vesper.*` — every `extern` in the shipped contracts is an
intrinsic. Specifically:

- `Attribute` (`prim-types-attr.fsi:10`, `extern class`, no members, no JS body) already lands on
  `Unsupported "js"` via the no-members branch. Unchanged.
- Capabilities (`disposable`, `seq<'T>` …) have members, so they go through
  `extractBodiedClassLike` and never call `registerIntrinsic`. Unchanged.
- The `widget` fixture (`test/…/fixtures/widget/widget.fsi:7`) binds `(# "object" #)` in
  `widget.js.fs`, so it is a marker today and an `extern` tomorrow. Unchanged.
- `ConformanceTests`' inline `type foo = extern` snippets and `HashingTests`' filler contracts
  never typecheck a *use*, so the shape they publish is unobserved.

## Verification

- `UnsupportedOnTargetTests` is the behavioural guard: the `nativeint` family must still be
  refused on JS with `"<name> is not supported on the js target"`. Its premise comment
  (lines 4-6, "what it lacks is a `.js.fs` binding a repr, and that absence is the statement")
  becomes wrong and must be rewritten — the statement is now the `extern` declaration, and the
  absent `.js.fs` only selects `Unsupported` over `Repr`.
- `SignatureExtractorTests` seeds `ctx.IntrinsicMarkers` directly in several tests
  (`:606`, `:628`, `:648`, `:673`, `:697`, `:754`); so does `ExternMemberInlineTests:35`. Those
  seeds all become unnecessary — the `.fsi` text in each already says `extern`.
- Full no-filter test run: `-Filter` implies `--no-build`, so a library edit needs an unfiltered
  pass to be trusted.

## Loose end (not a blocker)

`requireInlineExternMembers` (`VesperLib.fs:1149`) currently runs only where a marker exists. It
validates a TARGET-NEUTRAL contract, so after this change it runs on every target that reads the
`.fsi` — strictly more checking, in the right place. Watch for it newly firing on a JS-side
contract that the CLR-only marker previously exempted.

The real-package conformance sweep is CLR-only (`ConformanceTests.fs:250`), so
`ExternWithoutIntrinsic` is enforced on CLR and unexercised on JS. Not a gap this plan opens —
the JS side has no paired bodies for these types — but running the sweep for both targets is the
check that keeps the contract honest, and the manifest split makes that cheap. Tracked in
`per-target-manifest-plan.md`.

## Anchors (verify before editing)

- Marker fill: `ReferencedProject.fs:471-511` (`targetBodies` / `everyBody` / the two dictionaries).
- Marker read + the three publish arms: `VesperLib.fs:1114-1180`.
- `Class` fallback to delete: `VesperLib.fs:1178-1180`.
- `IntrinsicPlatform` DU + the many-to-one warning on `Platform`: `ExternalSymbols.fs:363-382`.
- Conformance rule that already states the invariant: `ConformancePass.fs:186-188`;
  `ConformanceTests.fs:79-87`.
- Consumers of `Unsupported` (both emit `Kind.UnsupportedOnTarget`, nothing else reads it):
  `PlatformTypes.fs:19-23`, `MemberRegistration.fs:769`.
