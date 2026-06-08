module XParsec.FSharp.Codegen.Clr.Tests.InferResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Systematic, source-synthetic coverage of `Infer`'s *application / construction*
// resolution surface — the area the Vesper.Set G5 work surfaced as buggy. Each
// case is a minimal idiomatic F# program fed through the real analysis pipeline
// (`buildContract defaultManifests` → `Pipeline.analyseSem`), asserting on the
// error-severity diagnostics. Because the pipeline runs `ResolvedTypes`, an empty
// error list is a strong claim: the program both type-checks (no `unify` mismatch)
// AND leaves no free `TyVar` in the frozen TAST.
//
// The matrix deliberately crosses the axes that `Infer` currently treats
// inconsistently:
//   * `new T(args)` (Expr.New)  vs  the `new`-less ctor sugar `T(args)` / `T args`
//   * spaced `T (args)` (Expr.App)  vs  non-spaced `T(args)` (Expr.HighPrecedenceApp)
//   * the constructed value used in a *pinning* context (bound / annotated) vs a
//     *non-pinning* one (an argument to a generic-parameter sink like `raise`)
//   * single-arg / multi-arg / nullary / generic constructors
//   * external instance-method chains off a freshly-constructed receiver
//
// Gaps A, B and C are now fixed (see `docs/infer-resolution-gaps-plan.md`): the
// rows below are all `test` and green. They stay as regression gates — each name
// still records which gap it was a symptom of, so a reintroduced divergence
// points straight at the failure mode.

let private errorsOf (src: string) : Diagnostic list =
    let provider = SymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Assert a synthetic program produces no error-severity diagnostics.
let private clean (label: string) (src: string) : unit =
    let errs = errorsOf src
    Expect.isEmpty errs (sprintf "%s — expected clean, got: %A" label (errs |> List.map (fun d -> d.Message)))

[<Tests>]
let tests =
    testList
        "InferResolution"
        [
            // ---- External ctor: `new` keyword (Expr.New → inferNew) -------------
            // The grounded baseline: `new` always routes through `inferNew`, which
            // pins the result to the class type regardless of pinning context.
            testList
                "NewKeyword"
                [
                    test "new, single string arg, bound" { clean "new-bound" "let e = new System.Exception(\"x\")" }
                    test "new, single string arg, under raise (non-pinning sink)" {
                        clean "new-raise" "let f () = raise (new System.Exception(\"x\"))"
                    }
                    test "new, two string args (ArgumentException(message, paramName))" {
                        clean "new-2arg" "let e = new System.ArgumentException(\"m\", \"p\")"
                    }
                    test "new, nullary (StringBuilder())" {
                        clean "new-nullary" "let sb = new System.Text.StringBuilder()"
                    }
                ]

            // ---- External ctor: `new`-less sugar, SPACED (Expr.App) -------------
            // `inferApp` probes `tryInferExternalCtorApp` (Infer.fs), so the spaced
            // form is grounded like `new`.
            testList
                "CtorSugarSpaced"
                [
                    test "spaced ctor `Exn (x)`, bound" { clean "spaced-bound" "let e = System.Exception (\"x\")" }
                    test "spaced ctor under raise" {
                        clean "spaced-raise" "let f () = raise (System.Exception (\"x\"))"
                    }
                ]

            // ---- External ctor: `new`-less sugar, NON-SPACED (HighPrecedenceApp)
            // GAP A (fixed): `inferHighPrecApp` now routes through `inferApp`, so the
            // non-spaced form probes `tryInferExternalCtorApp` exactly like the spaced
            // form and grounds the constructed value to its class type instead of a
            // fresh, unpinned `TyVar`. Before the fix a *pinning* context (bound /
            // annotated) rescued it downstream but a non-pinning sink (`raise`'s
            // fully-generic `'TException`) did not — the var leaked.
            testList
                "CtorSugarNonSpaced"
                [
                    test "non-spaced ctor `Exn(x)`, bound, is grounded [gap A/B]" {
                        clean "nonspaced-bound" "let e = System.Exception(\"x\")"
                    }
                    // The single-ident form via an `open` must ground too — the gap
                    // was not LongIdent-specific.
                    test "non-spaced ctor via `open` + single ident, bound [gap A/B]" {
                        clean "nonspaced-open" "open System\nlet e = Exception(\"x\")"
                    }
                    // The headline G5 failure: a non-pinning sink no longer leaves the
                    // result free.
                    test "non-spaced ctor under raise is grounded [gap A/B]" {
                        clean "nonspaced-raise" "let f () = raise (System.InvalidOperationException(\"x\"))"
                    }
                    test "non-spaced ctor under raise, System.Exception [gap A/B]" {
                        clean "nonspaced-raise-exn" "let f () = raise (System.Exception(\"x\"))"
                    }
                    test "non-spaced NotSupportedException under raise [gap A/B]" {
                        clean "nonspaced-raise-nse" "let f () = raise (System.NotSupportedException(\"x\"))"
                    }
                ]

            // ---- raise / failwith inline external functions ----------------------
            testList
                "RaiseFailwith"
                [
                    test "failwith (body uses `new`, always grounded)" {
                        clean "failwith" "let f () = failwith \"boom\""
                    }
                    test "raise of a pre-ground exn parameter" { clean "raise-ground" "let f (e: exn) = raise e" }
                    test "raise of a `new`-constructed exception" {
                        clean "raise-new" "let f () = raise (new System.Exception(\"x\"))"
                    }
                ]

            // ---- App ≡ HighPrecedenceApp parity ---------------------------------
            // The same construction differing ONLY by a space before `(` must
            // produce identical diagnostics — the parser's associativity choice is
            // not a semantic distinction. GAP A used to make these diverge; the
            // shared `inferApp` body now keeps them identical.
            testList
                "AppHPAppParity"
                [
                    test "raise (Exn (x))  ≡  raise (Exn(x)) [gap A]" {
                        let spaced = errorsOf "let f () = raise (System.Exception (\"x\"))"
                        let nonSpaced = errorsOf "let f () = raise (System.Exception(\"x\"))"

                        Expect.equal
                            (List.length nonSpaced)
                            (List.length spaced)
                            (sprintf "spaced=%d non-spaced=%d must match" (List.length spaced) (List.length nonSpaced))
                    }
                ]

            // ---- External instance-method chains --------------------------------
            // A fluent chain off a freshly-constructed external receiver. Gap A
            // grounds the `StringBuilder()` head eagerly, which on its own tripped
            // gap C: the eager `.Append` overload pick consulted no argument types and
            // grabbed `Append(char[], int, int)` for a single `string` arg
            // ("string vs TyTuple"). `tryInferExternalInstanceMethodCall` now resolves
            // the overload by the call-site arg types, so the whole chain type-checks.
            testList
                "InstanceMethodChain"
                [
                    test "StringBuilder().Append(string).ToString() [gap A/B/C]" {
                        clean "sb-1" "let f () = System.Text.StringBuilder().Append(\"x\").ToString()"
                    }
                    test "StringBuilder().Append.Append.ToString() (longer chain) [gap A/B/C]" {
                        clean "sb-2" "let f () = System.Text.StringBuilder().Append(\"x\").Append(\"y\").ToString()"
                    }
                ]

            // ---- External static method calls -----------------------------------
            testList
                "StaticMethod"
                [
                    test "String.Concat(a, b) non-spaced" {
                        clean "concat-hp" "let s = System.String.Concat(\"a\", \"b\")"
                    }
                    test "String.Concat (a, b) spaced" {
                        clean "concat-app" "let s = System.String.Concat (\"a\", \"b\")"
                    }
                ]

            // ---- Generic external ctor ------------------------------------------
            testList
                "GenericCtor"
                [
                    test "ResizeArray<int>() (explicit type app)" {
                        clean "ra-int" "let xs = System.Collections.Generic.List<int>()"
                    }
                ]

            // ---- Vesper.Set G5 handoff roots 2-4 --------------------------------
            // Regression gates for three of the four inference/extraction roots fixed
            // in the G5 wall-clearing session (`docs/vesper-set-g5-handoff.md`). Each
            // is a leaked-free-`TyVar` (or `unify` mismatch) that `ResolvedTypes` flags
            // on otherwise-clean F#. Root 1 (`objnull` extracts to `obj`) is a
            // contract-extraction gate and lives in SemanticAnalysis.Tests
            // `VesperLibTests`; roots 2-4 are pure front-end resolution and gate here.
            testList
                "SetG5Roots"
                [
                    // Root 2 — `inferNew` secondary-ctor lookup. A `[<Struct>]` with no
                    // primary ctor has `CtorParams = [||]`; the `new T(arg)` keyword
                    // path used to unify the arg against the *primary* params (i.e.
                    // `unit`), clashing with the real arg type. It now falls back to a
                    // matching-arity `SecondaryCtors` entry. Set's `new SetIterator<'T>(s)`
                    // (set.fs:612) was the original site.
                    test "struct, only an explicit ctor, `new T(arg)` [root 2: inferNew secondary-ctor]" {
                        clean
                            "struct-new-monomorphic"
                            (String.concat
                                "\n"
                                [
                                    "[<Struct>]"
                                    "type Boxi ="
                                    "    val Item: int"
                                    "    new (x: int) = { Item = x }"
                                    "let b = new Boxi(5)"
                                ])
                    }
                    test "generic struct, only an explicit ctor, `new T<'a>(arg)` [root 2]" {
                        clean
                            "struct-new-generic"
                            (String.concat
                                "\n"
                                [
                                    "[<Struct>]"
                                    "type Cell<'T> ="
                                    "    val Item: 'T"
                                    "    new (x: 'T) = { Item = x }"
                                    "let make (x: 'T) = new Cell<'T>(x)"
                                ])
                    }

                    // Root 3 — `inferILIntrinsic` pins each `Expr.Null` operand to the
                    // first non-null operand. `isNull` is `inline` with body
                    // `(# "ceq" value null : bool #)`; the `null` leaf used to mint its
                    // own unpinned fresh `TyVar` that rode the spliced body into every
                    // caller.
                    test "`isNull` on a reference operand grounds the `null` leaf [root 3: ILIntrinsic null pinning]" {
                        clean "isnull-string" "let f (s: string) = isNull s"
                    }

                    // Root 4 (the load-bearing one) — `InlineExpansion.expandLocalAt`
                    // now always `deriveInlineTypeArgs`. A *generic* local `let inline`
                    // expanded with zero type args left the callee's generalised typars
                    // (here the `:?> 'T` result typar) free in the caller's frozen TAST,
                    // because beta-reduction binds value params but not typars. Only
                    // triggers when the callee is `inline` AND generic — monomorphic
                    // locals derive `[||]` and were always fine.
                    test
                        "local generic `let inline` cast helper from a non-inline fn [root 4: expandLocalAt derives type args]" {
                        clean
                            "inline-cast"
                            (String.concat
                                "\n"
                                [
                                    "let inline cast (x: obj) : 'T = x :?> 'T"
                                    "let useCast (x: obj) : string = cast x"
                                ])
                    }
                    test "local generic `let inline` returning a generic record, field-accessed [root 4]" {
                        clean
                            "inline-record"
                            (String.concat
                                "\n"
                                [
                                    "type Boxed<'T> = { Unbox: 'T }"
                                    "let inline asBoxed (x: obj) : Boxed<'T> = x :?> Boxed<'T>"
                                    "let useBoxed (x: obj) : int = (asBoxed x).Unbox"
                                ])
                    }
                ]
        ]
