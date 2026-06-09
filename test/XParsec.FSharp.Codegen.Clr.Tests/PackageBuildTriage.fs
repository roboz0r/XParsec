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
            // Current state (after G19 fully closed, 2026-06-07): the wall is down
            // to 44 analysis errors (from 197 → 121 → 72 → 71 → 50 → 44). G20 is
            // CLOSED — class→interface and class→base `:>` upcasts now type-check:
            // `subsumes` walks declared interfaces (local `interface … with` impls +
            // the external provider's frozen interface list) as well as the `inherit`
            // chain, an interface-impl resolution pre-pass stamps `InterfaceImpls.Resolved`
            // before any member body is typed, and `:>`/argument coercion go through
            // `tryCoerceUpcast`, which *unifies* the witness supertype's type args so a
            // generic / wildcard target is pinned (`this :> seq<_>`). G19 is CLOSED:
            // the coercion engine (`unifyArg`) handles in-scope `Comparer<'T>.Default`
            // → `IComparer<'T>` slots, and the *forward-reference* residue (a class
            // member calling a sibling-module function whose `let` is generalised AFTER
            // class members) closed via `prebindModuleFunctionSchemes` — an
            // annotation-derived scheme pre-pass run before `fillClassMembers`, so the
            // forward call instantiates fresh and `unifyArg` upcasts the subtype arg.
            // Both gated by the `ClassCoercion` rows. G21 + G22 are now CLOSED too
            // (wall 44 → 38): the deferred dot-access drain (Engine.fs) resolves an
            // *external* receiver through the provider (G22 — `comparer.Equals` on the
            // `IEqualityComparer` interface param, pinned only by the post-body
            // conformance unify) and normalises its `System.Object` params to `obj`;
            // and `:?>` admits a still-unresolved source TyVar (G21 — `that :?> Set<'T>`
            // on an interface member's unannotated `obj` param). Gated by the
            // `ClassCoercion` "G21"/"G22" rows. G7 (`for-in`) is also CLOSED — but the
            // 2 `op_LeftShift` it was thought to cascade were a MISDIAGNOSIS (wall 38 →
            // 36): the bitwise/shift operator family was simply absent from the
            // *contract surface*. A parenthesised binding head `(<<<)`/`(&&&)` lexes to
            // a *generic* operator token (not the distinct enum), so the contract
            // extractor's `opTokenToCompiled` mapped it to no compiled name and dropped
            // the val — fixed by covering those tokens + their source text. The shift's
            // `int32` param then dealiased to `int` (a primitive *alias* now follows its
            // `type int32 = int` definition through `mkNominal`'s `Abbrev` arm rather
            // than freezing the alias spelling). The `for-in` front end itself always
            // resolved (`Set<'T> : IEnumerable<'T>` → `tryLocalInterfaceEnumerator`);
            // gated by the `ForIn` + `BitwiseOperators` rows. The 36 remaining are NOT
            // for-in cascade — three independent roots + their ~33 TyVar cascade:
            //   - 2 "<unfreezable …>" at `comparer.GetHashCode(x)` (G22-area: the
            //     deferred external dot-access drain freezes an unfreezable template).
            //   - the lone "SetTree`1 vs unit" in `SetTree.compareStacks` (the
            //     `SetTreeNode(...) :> SetTree<'T>` cons-list cast).
            //   - ~33 "ResolvedTypes: … unresolved TyVar" cascading from the above.
            // FRONT END CLOSED (2026-06-08): all of the above analysis roots are fixed
            // — the four G5 deferred interfaces (`IComparable` / `IStructuralEquatable`
            // / `ICollection<'T>` / `IReadOnlyCollection<'T>`) now type-check and
            // `set.fs` analyses to **0 errors** (the last 2 `raise`-area TyVar leaks
            // closed by the `Infer` App/HPApp unification + arg-aware instance-method
            // probe — `inferHighPrecApp` now delegates to `inferApp`, and
            // `tryInferExternalInstanceMethodCall` resolves instance overloads by the
            // call-site arg types; gated by `InferResolutionTests.fs`). The build now
            // sails past the `analysisErrors` gate and through the BACKEND gaps in
            // turn — re-verified end-to-end by flipping this row to `ftest`:
            //   - CLOSED (2026-06-08): the
            //     `Emit: closure parameter destructuring is out of scope: Tuple`
            //     wall — a lambda with a *tupled* parameter (`fun (a, b) -> …`)
            //     captured into a closure. `discoverClosures` now mints a synthetic
            //     tuple `ParamKey` and `buildClosureInvoke` `bindPattern`s the
            //     element bindings out of the `ldarg.1` `ValueTuple`n` (gated by
            //     `TupleTests.fs`). The build now reaches `NominalEmit`.
            //   - CLOSED (2026-06-08): `Emit: no emitted union for match on
            //     'Vesper.Collections.List`1'` (EmitExpr.fs:148) — the consumer
            //     external-list match gap, a `match` against the *external* `List`
            //     cons-union (`[] / ::`), whose cases live in referenced metadata,
            //     not this assembly. The extracted contract keeps the cons-list's
            //     op-form case names (`op_Nil`/`op_ColonColon`), so it never resolves
            //     through the generic external-union path; the provider now mirrors
            //     construction (`TryEmitUnionCons`) with a cons-list special-case in
            //     `ExternalUnionTag` / `ExternalUnionCaseField`, minting the `_tag` +
            //     `Cons_0`/`Cons_1` refs off the known emitted layout (`Empty` tag 0,
            //     `Cons` tag 1). Gated by `ListModuleTests.fs`
            //     "ListExternalMatchRuntime".
            //   - CLOSED (2026-06-08): `ClrProvider: could not recover declaring
            //     type argument 0 (open FTConst("bool", …))` (ClrEncoder.fs:355).
            //     This surfaced as a *codegen* crash but was a **Freeze receiver-
            //     typing** bug: `not this.stack.IsEmpty` (set.fs:597/602) froze
            //     `this.stack` (a `SetTree<'T> list`) as `bool` — the type of the
            //     final `.IsEmpty`. `recoverFieldStepTy`'s `TyClass` arm scanned only
            //     the class's *members*, missing the `val`/ctor-param instance
            //     fields, so an intermediate chain segment that is a field fell back
            //     to the chain's final type. Fixed in `FreezeExpr.recoverFieldStepTy`
            //     (search `InstanceFields` + `CtorParams` before members). Gated by
            //     `StructTests.fs` "a chained property on a struct val field types the
            //     receiver as the field, not the property".
            //   - CLOSED (2026-06-09): `Emit: no binding for variable src@<off>:PatIdent`
            //     (EmitExpr.buildVarLoad, via `Emit.buildStaticCctor`) — a reference to
            //     a module-level value (`SetTree.empty`) from a `.cctor`/member body.
            //     The handoff's "stale generic `static let`" diagnosis was wrong: module
            //     values had no real storage at all (they were `Main` locals, and a
            //     Library has no `Main`). Fixed by emitting module values as `public
            //     static` fields on their module holder, initialised by the holder's
            //     `.cctor`, referenced via `ldsfld` (module-representation-plan). Gated
            //     by `ClassStatic` "module value …" rows.
            //   - CLOSED (2026-06-09): two front-end typar-grounding gaps that each
            //     leaked a bare `TyVar` past `ResolvedTypes`, surfacing at the backend
            //     as `FTUnknown "?ungrounded-operator"`. (1) A nested `let rec loop
            //     (t': SetTree<'T>) acc` (`SetTree.toList`) minted a *fresh* `'T` per
            //     binding scope, so the inner lambda's capture of `t'` leaked — fixed by
            //     inheriting the enclosing binding's typar scope (`inferBinding`, F#
            //     lexical typar scoping). (2) `Set.Add`/`Remove` calling an *earlier*
            //     module fn with an *unannotated* param (`SetTree.add … k …`) was typed
            //     against `prebindModuleFunctionSchemes`' annotation-only stand-in, which
            //     over-generalised `k`; the member's `value` arg bound the free typar and
            //     leaked — fixed by typing bodies in *declaration order*
            //     (`Unification.walkElems`) so the real scheme exists first. Both the
            //     handoff's "SRTP/operator default" guess and its "instance member" framing
            //     were imprecise (the first wall hit was actually a *closure-capture field*).
            //     Gated by `InferResolution` "TyparGroundingAcrossBoundaries".
            //   - CURRENT wall: `Emit: no binding for variable src@<off>:PatIdent`
            //     (EmitExpr.buildVarLoad, via `Emit.buildStaticCctor` / `NominalEmit`) —
            //     `Set<'T>`'s `static let empty = … SetTree.empty` references the *generic*
            //     module value `SetTree.empty` (`let empty = null`), which the
            //     module-representation v1 slice DEFERS (generic values are compiled as
            //     generic methods, not static fields — see module-representation-plan §2.3).
            //     A pure-codegen gap, distinct from the now-closed front-end leaks.
            //     Flip `ptest`→`test` once it (and any following gaps) close.
            ptest "Vesper.Set builds BCL-only" { buildsBclOnly "Vesper.Set" }
        ]
