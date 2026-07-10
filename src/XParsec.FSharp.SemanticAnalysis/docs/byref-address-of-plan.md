# byref producer side: the `~&` address-of operator + `byref<'T>` contract

**Status (2026-07-09): DEFERRED, design-only. No consumer yet** (user, confirmed 2026-07-09 —
no pressing consumer; the goal is eventual full F# feature coverage with more sophisticated
analysis to facilitate future native lowering). Spun out of
`contract-sourced-intrinsic-identity-plan.md` §5 once scouting showed "byref migration"
decomposes into three pieces at different risk. **Piece 1 (type identity `"&"`→`Vesper.byref`)
LANDED there**; this doc holds the two deferred pieces. Delete when they land
(`feedback_plan_docs_ephemeral`). This is a *producer*-side feature — Vesper source cannot yet
*declare or construct* an arbitrary byref; the *consume* side (`&local` into a BCL `out`/byref
param) already works and is NOT in scope to change except where the operator generalisation
subsumes it.

## Premise (user, confirmed 2026-07-09)

The **type** is `byref` (identity `Vesper.byref`, a generic intrinsic `TyConst(byrefKey,[elem])`
— Piece 1); **`&` / `~&` is the OPERATOR** that constructs one from an lvalue. These are
distinct concerns. The long-term aim is full F# feature coverage with real static analysis
(here: an lvalue/addressability judgment) so a future native backend can lower byrefs correctly.

## What already exists (do NOT rebuild)

- **Parser**: `&`/`&&` prefix → `Token.OpAmp`/`OpAmpAmp`; `~&`/`~&&` → compiled names
  `op_AddressOf`/`op_IntegerAddressOf` (`ExpressionParsing.fs` `pOpAddressOfPrefix`, `Token.fs`).
- **Desugar**: `Token.OpAmp` → `DesugaredForm.OpName "op_AddressOf"` (`Passes/Desugar.fs`).
- **Inference**: `InferApp.inferPrefix` **special-cases** `op_AddressOf`, bypassing operator
  resolution ("no Vesper.Core / BCL symbol"), and types `&x` as `TyConst(byrefKey,[operandTy])`.
  This matches a BCL method's byref/`out` param (`Int32.TryParse(string, int&)`).
- **Freeze**: `Freeze/Apply.fs` lowers `op_AddressOf` to a **hardcoded** `ILIntrinsic("ldloca", …)`;
  codegen emits `ldloca` by inspecting the inner `Var`'s slot (no recur, which would `ldloc` the
  value). Only the **mutable-local** lvalue kind is handled.
- **Type consume side**: `MetadataSymbols.tryBuildType` mints `byrefKey` for `t.IsByRef`
  (direction-agnostic — `in`/`out`/`ref` collapse to arity-1 `T&`); `ClrEncoder.encodeType`
  emits `ELEMENT_TYPE_BYREF` at the param/return seam; `inferIndexedLookup` erases a byref return
  to its element at the value position. Regression-guarded by the `Span<char>` indexer +
  `TryParse(s, &r)` CLR tests.

So the operator is an embryonic front-end special-case restricted to one lvalue kind, with no
`byref<'T>` name resolution and no real lvalue analysis.

## Piece 2 — the `byref<'T>` contract + source-writable annotations

Author `prim-types-byref.fsi`/`.fs` under `namespace Vesper`:
`type byref<'T> = (# "!0&" #)` ⇒ identity `Vesper.byref` (verbatim name, arity in args, matching
the Piece-1 `byrefKey`). This makes the *name* `byref` resolvable, which is the precondition for:
(a) declaring byref parameters / returns in Vesper source, and (b) the `~&` operator's return
annotation in Piece 3's impl file.

Open questions to settle before implementing:
- **Direction forms.** F# has `byref<'T>` / `inref<'T>` (readonly, `in`) / `outref<'T>` (`out`).
  MetadataSymbols currently collapses all three to arity-1 direction-agnostic `T&`. Decide whether
  the producer side needs the direction distinction (a second `'Kind` arg / distinct abbrevs) or
  whether direction-agnostic byref suffices for v1. Lean: start direction-agnostic; add `inref`/
  `outref` only when a consumer needs the readonly/write distinction.
- **Who resolves the name.** With no source writing `byref<'T>` today, the contract is inert until
  a producer consumer exists — so Piece 2 lands *with* Piece 3, not before.

## Piece 3 — `~&` as a real operator with lvalue analysis + `ld*a` codegen

Target shape (from §5 sketch):
`let inline (~&) ([<LocatorValue>] x: 'T) : byref<'T> = (# "ld*a" : byref<'T> #)` in the impl file.

Three sub-problems, each a real design decision:

1. **Front-end special-case → resolved contract operator (or not).** Today `op_AddressOf`
   deliberately bypasses operator resolution. Making `(~&)` a resolved contract symbol means
   `inferPrefix`/`translatePrefix` stop special-casing the name and instead resolve it like any
   operator, reading a new `[<LocatorValue>]` attribute off the parameter.
   *Fork:* is the contract-operator route worth it, or is generalising the existing special-case
   (keep `inferPrefix`'s direct byref typing; just replace the hardcoded `ldloca` lowering) the
   lighter, equally-correct path? The special-case exists precisely because `op_AddressOf` has no
   runtime symbol; a contract `(# "ld*a" #)` impl would give it one. Decide by whether
   `[<LocatorValue>]` buys anything the dedicated lvalue check below doesn't.

2. **The lvalue judgment (the actual "sophisticated analysis").** `~&`'s argument must be a real
   **lvalue**: a mutable local, a (mutable) field, an array element, or a byref/`out` argument —
   never a computed value (`&(f x)` is illegal). No such addressability judgment exists today
   (the current code just assumes a mutable local and lets codegen fail otherwise). This is the
   feature's substance: a predicate over the resolved operand expression, run in the semantic
   analyzer (relax-then-reject: type it, then reject a non-lvalue at elaboration). Prior art to
   reuse / align with: `Regions`, `RefCellPromotion`, captured-mutable handling, and how mutable
   locals already carry slot identity.

3. **`ld*a` context selection in codegen.** `"ld*a"` is deliberate — no single IL opcode fits;
   Codegen.Clr must pick `ldloca/ldloca.s/ldflda/ldsflda/ldarga/ldarga.s/ldelema` by the operand's
   lvalue KIND. Key observation (scout): the lvalue kind is **derivable from the operand TExpr
   shape at codegen** (a `Var`→slot ⇒ `ldloca`/`ldarga`; a field access ⇒ `ldflda`/`ldsflda`; an
   index ⇒ `ldelema`) — exactly as `Freeze/Apply` already inspects the inner `Var`'s slot. So the
   clean split is: **semantic layer VALIDATES** addressability (sub-problem 2), **codegen
   CLASSIFIES + EMITS** by shape. `[<LocatorValue>]` alone cannot validate.
   *Fork:* does the existing `ILIntrinsic` node want a context-resolved `"ld*a"` opcode (codegen
   switches on the operand), or is per-shape lowering in `Freeze/Apply` (emit the specific opcode
   there, as today for `ldloca`) cleaner? The latter keeps `ILIntrinsic` opcodes concrete.

## Backend scope

- **CLR only.** byref is a CLR ABI concept. On **JS** the byref intrinsic has no representation
  and `~&` must be rejected (there is no JS byref lowering — a producer-position byref should
  error at elaboration on the JS target, mirroring how other CLR-only constructs are gated).

## Relevant memories

`feedback_redesign_doc_first` (this doc — design before code), `feedback_relax_parser_defer_to_typecheck`
(type `&x` generally, reject a non-lvalue at elaboration), `feedback_freeze_no_backend_knowledge`
(the byref *identity* is asm-blind; `ld*a` opcode selection stays in Codegen.Clr),
`feedback_match_fsharp_grammar` (mirror F#'s byref/inref/outref where the direction distinction is
real), `feedback_prototype_correct_semantics_over_fsharp_parity` (don't reproduce F# quirks for
their own sake).
