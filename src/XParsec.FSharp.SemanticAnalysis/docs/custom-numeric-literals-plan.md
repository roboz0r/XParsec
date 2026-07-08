# Custom numeric literals (`52I` / `52Q` / …)

**Status (2026-07-08): live plan.** Spun out of `contract-sourced-intrinsic-identity-plan.md` —
Stage 5a surfaced it: a bigint LITERAL is a custom numeric literal, not a primitive constant.
`bigint` the TYPE already landed there as a `prim-types` intrinsic (Stage 5a, user-confirmed model
1a); THIS plan is the LITERAL mechanism only. Delete when it lands (`feedback_plan_docs_ephemeral`).

## What the feature is (F# language spec)

The spec is sparse on this feature; the syntactic translation below is the whole of it. Integer
literals with the suffixes `Q`, `R`, `Z`, `I`, `N`, `G` are NOT primitive literals — they are F#'s
user-extensible *custom numeric literals*. The suffix letter selects a `NumericLiteral<suffix>`
module in scope, and the literal is a SYNTACTIC translation to a CONSTRUCTED value (a call), typed
by that module's return type:

```fsgrammar
xxxx<suffix>
    For xxxx = 0                → NumericLiteral<suffix>.FromZero()
    For xxxx = 1                → NumericLiteral<suffix>.FromOne()
    For xxxx in the Int32 range → NumericLiteral<suffix>.FromInt32(xxxx)
    For xxxx in the Int64 range → NumericLiteral<suffix>.FromInt64(xxxx)
    For other numbers           → NumericLiteral<suffix>.FromString("xxxx")
```

No literal syntax is available for numbers outside the range of 32-bit integers unless the module
defines `FromInt64` / `FromString`. Example — defining `NumericLiteralZ` enables `32Z` to build a
32-character string of `Z`:

```fsharp
module NumericLiteralZ =
    let FromZero() = ""
    let FromOne() = "Z"
    let FromInt32 n = String.replicate n "Z"
```

An F# compiler MAY optimise on the assumption that calls to numeric-literal functions always
terminate, are idempotent, and have no observable side effects — so a known-body call may be
constant-folded at compile time.

FSharp.Core (ported here as `math/z.fsi` / `z.fs` in `XParsec.FSharp.Lib`, `namespace
Microsoft.FSharp.Core`) defines ONLY `NumericLiteralI` (→ `bigint`). `Q`/`R`/`Z`/`N`/`G` are reserved
for user/library modules and are an ERROR ("no `NumericLiteral<suffix>` in scope") when undefined.

## Current state — the in-built stand-in

- The lexer emits six distinct `NumBigInteger{Q,R,Z,I,N,G}` tokens (`Token.fs:1112`) — a MISNOMER;
  they are the six suffixes, not six bigints.
- `Passes/Unification/InferLiterals.fs` hardcodes ALL six to `ctx.Intrinsics.BigInt`, bypassing
  `NumericLiteral` resolution and skipping the value construction. So a bigint literal VALUE cannot
  freeze — `Freeze.parseConst` throws "non-representable literal NumBigIntegerI in constant
  position". This over-claims (only `I` is bigint) and is the whole of the analyzer's support today.

## Requirements to elaborate

- **Resolve** `NumericLiteral<suffix>` from the open scope (an `[<AutoOpen>]` module by convention).
  A missing module for the written suffix is a diagnostic, NOT a silent `bigint`.
- **Desugar** the literal to the magnitude-appropriate `From*` call per the spec table (0/1 →
  `FromZero`/`FromOne`; Int32/Int64 range → `FromInt32`/`FromInt64`; else `FromString "xxxx"`). The
  literal's type is the resolved member's return type (`bigint` for `I` via the port). This REPLACES
  the `InferLiterals` `NumBigInteger*` arm. (`feedback_match_fsharp_grammar` — mirror the spec
  translation, don't invent context-sensitivity.)
- **Freeze / codegen** lower the desugared CALL (not a `TConstValue`): CLR through the
  `NumericLiteralI` body (`System.Numerics.BigInteger` construction); JS either through a per-target
  `NumericLiteralI` or directly to a native `bigint` literal (`52n`) — decide the JS lowering when
  implementing. The terminate/idempotent/side-effect-free licence permits constant-folding a
  known-body call.
- **Interplay with `bigint`:** `bigint` the TYPE stays the Stage-5a `prim-types` intrinsic
  (cross-target repr). Confirm the coexistence with the FSharp.Core port's
  `Microsoft.FSharp.Core.bigint = System.Numerics.BigInteger` abbreviation (which identity wins when
  both are in scope).

## Relevant memories

`feedback_fsharpcore_port_transliterate` (`math/z.fs` is a near-literal FSharp.Core port),
`feedback_match_fsharp_grammar` (mirror the spec's syntactic translation), `feedback_plan_docs_ephemeral`
(delete on landing), `feedback_redesign_doc_first` (this doc).
