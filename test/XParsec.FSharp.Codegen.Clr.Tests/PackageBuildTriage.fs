module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTriage

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Pre-2 / Phase 0 (vesper-lib-test-plan.md): the triage smoke pass. Run the
// `buildPackage` harness against every Candidate / At-risk package and let the
// green/pending split *be* the readiness map — converting the readiness
// hypothesis into a fact and producing the work-list for the B/R suites
// (Phase 2/3). A green row = the package compiles to a BCL-only DLL today; a
// `ptest` (pending) row = it does not yet, with the blocking diagnostic quoted in
// the test name + comment so the gap is legible without re-running. Each pending
// body still calls `buildsBclOnly`, so flipping `ptest`→`test` re-runs the build
// once the gap is closed.
//
// Triage result (2026-05-30, Gap 1 closed):
//   Vesper.Choice  — BUILDS (BCL-only).
//   Vesper.Option  — BUILDS (BCL-only) after the front-end + `unit`-repr fixes.
//   Vesper.Result  — BUILDS (BCL-only) after the `unit`-repr fix.
//   Vesper.Array   — BUILDS (BCL-only) after the `'T[]` intrinsic codegen landed
//                    (`arr.[i]`/`arr.Length`/`newarr` → `ldelem`/`ldlen`/`newarr`).
//   Vesper.Seq     — BUILDS (BCL-only)
//
// Out of scope here:
//   - Vesper.Core / Vesper.List — proven (PackageBuildTests).
//   - Vesper.Comparison — `impl = []` (the ordering operators are signature-only
//     inline bodies, no DLL); exercised at use sites in Phase 2, not built here.
//   - Vesper.Printf — a C# DLL (PrintfHappyPathTests).
//   - Vesper.Set — Blocked on class/interface/object-expr backend features
//     (vesper-set-gaps.md §B); do not attempt B/R.

/// Assert a package builds to a BCL-only DLL (empty `FSharpCoreDependencies`).
let private buildsBclOnly (package: string) : unit =
    let _, artifact = (buildPackage package).Value

    Expect.isEmpty
        artifact.FSharpCoreDependencies
        (sprintf "%s must compile to a BCL-only DLL (FSharp.Core deps: %A)" package artifact.FSharpCoreDependencies)

[<Tests>]
let tests =
    testList
        "PackageBuildTriage"
        [
            // ---- Candidates: no known structural blocker (Core dep only) ------

            // Choice links end-to-end: `Choice<'T1,'T2>` + its constructors compile
            // to a BCL-only DLL. Real assertion — a regression turns this red.
            test "Vesper.Choice builds BCL-only" { buildsBclOnly "Vesper.Choice" }

            // `option.fs` type-checks, compiles end-to-end, AND is now BCL-only.
            // The front-end gaps were fixed earlier (`Some` carries its `Value`
            // field; external-ctor-without-`new` resolution lets `get` /
            // `member Value`'s `raise (InvalidOperationException …)` type-check);
            // Gap 1 (vesper-lib-test-plan.md) then removed the last wall — `iter` /
            // `defaultWith` / `orElseWith` no longer pull `Microsoft.FSharp.Core.Unit`
            // now that `unit` encodes off its `prim-types-min` `System.ValueTuple`
            // binding. Behavioural coverage rides reflection-invoke (OptionTests.fs).
            test "Vesper.Option builds BCL-only" { buildsBclOnly "Vesper.Option" }

            // `result.fs` compiles to a BCL-only DLL — same story as Option: the
            // `unit`→`System.ValueTuple` repr (Gap 1) dropped the lone
            // `Microsoft.FSharp.Core.Unit` dependency the impl's `unit`-typed
            // functions used to pin.
            test "Vesper.Result builds BCL-only" { buildsBclOnly "Vesper.Result" }

            // ---- At-risk: documented feature gap ------------------------------

            // `array.fs` (`zeroCreate` + `fold`) compiles to a BCL-only DLL. The
            // `'T[]` intrinsic codegen landed end-to-end: `arr.[i]` and `arr.Length`
            // lower (via Freeze) to the `ldelem`/`ldlen` IL intrinsics, and
            // `Array.zeroCreate`'s `(# "newarr !0" … #)` to `newarr <elem>` — all
            // three carry their element type on `TExprG.ILIntrinsic.typeOperand`.
            test "Vesper.Array builds BCL-only" { buildsBclOnly "Vesper.Array" }

            // The explicit-enumerator terminals `fold` / `reduce` / `toArray` now
            // compile BCL-only. The metadata provider surfaces interface members
            // through base interfaces (so `source.GetEnumerator()` / `e.MoveNext()` /
            // `e.Current` / the `use` `IDisposable.Dispose` resolve against
            // `IEnumerator`1`/`IEnumerator`/`IDisposable`), `not` is in the Core
            // `Operators` contract, and the supporting backend gaps closed along the
            // way (value restriction on expansive lets, `while`/local-assignment IL,
            // `'T[]` type translation + SZArray encoding + array-return members). The
            // former blocker — `truncate`'s `Enumerable.Take<TSource>(…)`, a
            // *generic external static method* (method-owned typars) — now resolves:
            // the method axis is carried as baked `FTTypar(Method, j)` in the member's
            // two-axis `ExternalSignature` template, instantiated at the call site,
            // and emitted via a `MethodSpec`.
            test "Vesper.Seq builds BCL-only" { buildsBclOnly "Vesper.Seq" }

            // Vesper.Set — the sprint target (vesper-set-sprint-phase-9.md §9.7).
            // Phase 9's *source rewrites* (§9.1-9.6) have landed: `set.fs` no longer
            // references any FSharp.Core idiom (OptimizedClosures / FastGenericComparer
            // / anyToStringShowingNull / SR.GetString are gone), `ValueOption` → the
            // struct `Option`, and the `:? T as x` / `[a; b]` patterns are spelled with
            // the supported `:?`+`:?>` / cons-terminated forms. The build now reaches
            // *Freeze* and stops at the first remaining BACKEND gap (out of Phase 9
            // scope — these are Phase 5/6 follow-ups). Gaps cleared so far:
            //   G4 (CLOSED) — Freeze "InfixApp ... missing DesugaredForm entry" on the
            //   `t.Height = 1` inside `[<Struct>]` SetIterator's `IEnumerator.MoveNext`.
            //   Root cause was *not* type resolution: `Desugar` only walked a type's
            //   own members, never `interface … with member …` bodies, so the `=` node
            //   got no `DesugaredForm.OpName`. Fixed by recursing into `InterfaceImpl`
            //   bodies in `Desugar.walkMemberElems`; gated by the `StructTests` row
            //   "an infix operator inside a struct interface member resolves".
            //   G9 (CLOSED) — Freeze.translatePat "TODO Named (acc [k])" on the
            //   `fun acc k -> add comparer k acc` lambda in `Set.ofArray` (set.fs:747).
            //   Root cause was a PARSER bug, not a Freeze gap: the `fun` parser used
            //   `many1 Pat.parse` (full patterns), so `acc k` parsed as the applied
            //   pattern `Named(acc, [k])` instead of two atomic binders. The F# grammar
            //   is `FUN atomicPatterns RARROW`; fixed by switching the lambda to
            //   `Pat.parseAtomicBindingArgMany1` (the same atomic parser the let/member
            //   binding heads use). Gated by the 5 updated `fun`-parsing golden rows.
            //   G10 (CLOSED) — Freeze.translateApp "expected function type … free
            //   TypeVar" on `SetTree.diff …` inside `Set<'T>`'s `static member (-)`.
            //   Root cause: a static *operator* member's body was never inferred.
            //   `memberNameOf`/`fillTypeMembers` only recognised `Pat.NamedSimple`
            //   heads, so a `Pat.Op` member got no `TypeMemberInfo` and Unification
            //   skipped its body — every application in it stayed a free TyVar. Fix:
            //   register + infer `Pat.Op` members, keyed by `CstKeys.ofPat` of the
            //   head (the `(lParen, PatOp)` key `inferBinding` links under — keying on
            //   the op token left `mInfo.Type` an unlinked placeholder). Also closed
            //   the `Set<'T>.Empty` / `Set<'T>.Singleton x` follow-on: a static member
            //   read on an *explicitly* instantiated generic class parses as
            //   `DotLookup(TypeApp(Set, <'T>), .Member)` — added the inference +
            //   Freeze arms (property → StaticPropertyGet, method → StaticMethodCall).
            //   All gated by `ClassStatic` rows.
            //   G11 (CLOSED) — 34× "Free type parameter 'T is not declared …" — the
            //   class's typars were absent from a *generic member's signature
            //   annotations* (`static member Singleton (x: 'T) : Set<'T>`, the
            //   `Set<'T>` operator params). `inferBinding` minted a fresh typar scope
            //   seeded only by the binding's *own* `<'a>`, dropping the enclosing class
            //   typars `fillTypeMembers` had put in scope. Fixed by a new
            //   `Resolution.EnclosingTypars` slot: `fillTypeMembers` /
            //   `fillSecondaryCtors` set it to the class typar scope and `inferBinding`
            //   seeds its fresh scope from it first. Gated by the SemanticAnalysis
            //   "G11" rows. Cleared 28 of the 34 (every class-typar case).
            //   G12 (CLOSED) — 6× "Free type parameter 'U/'T1/'T2 …" — *implicit*
            //   member-level generic params (`member s.Map f : Set<'U>`,
            //   `s.PartitionWith(p: 'T -> Choice<'T1,'T2>)`). Distinct from G11: these
            //   typars are neither class typars nor explicit `<'a>` on the member.
            //   Fixed by registering them: `MemberRegistration.implicitMemberTypars`
            //   walks the member signature for free typars not in the enclosing-type /
            //   explicit-typar lists and appends them to `MethodTypeParams`;
            //   `fillTypeMembers` keeps the member typars in `EnclosingTypars` across
            //   the body so nested `let`s (`Comparer<'U>.Default`) resolve them too.
            //   Gated by the SemanticAnalysis "G12" rows.
            // Current state (reached after G12): every "Free type parameter" error is
            // gone; the build surfaces the rest of the Phase-5/6-deferred WALL.
            // Dominant categories, in rough size order:
            //   - "Unresolved qualified name: SetTree.*" — the nested `SetTree` module's
            //     functions don't resolve from `Set<'T>` member / `module Set` bodies.
            //   - "for-in: source is not a supported enumerable" (G7), `:>` upcasts
            //     (SetTreeNode→SetTree, →IEnumerator), "Unknown class type
            //     'IEqualityComparer'", and "Unknown operator symbol op_LessThan/…".
            //   - the four Phase-5-deferred interfaces (IComparable /
            //     IStructuralEquatable / ICollection / IReadOnlyCollection), `use`.
            // Flip `ptest`→`test` once the remaining gaps close.
            ptest "Vesper.Set builds BCL-only" { buildsBclOnly "Vesper.Set" }
        ]
