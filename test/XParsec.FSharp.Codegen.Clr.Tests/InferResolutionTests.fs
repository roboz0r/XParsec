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
// The three resolution gaps these rows pin (A: `inferApp` ≢ `inferHighPrecApp`
// parser-form divergence; B: the generic-application fallback leaking an unpinned
// external result; C: eager external instance-method overload mis-pick) are now
// fixed — `inferHighPrecApp` delegates to `inferApp` (one shared probe chain) and
// `tryInferExternalInstanceMethodCall` resolves instance overloads by the call-site
// arg types. The rows below are all `test` and green; they stay as regression gates,
// each name recording the gap it was a symptom of so a reintroduced divergence
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

/// The inferred `SemType` of the program's last top-level `let` — used to assert
/// a precise grounding (not just "no errors": an over-generalised `int -> 'b ->
/// int` is error-free yet wrong).
let private lastLetTy (src: string) : SemType =
    let provider = SymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file

    tast.Decls
    |> EqArray.toList
    |> List.choose (fun d ->
        match d with
        | TDecl.Let(_, _, _, ty) -> Some ty
        | _ -> None
    )
    |> List.tryLast
    |> Option.defaultWith (fun () -> failwithf "no top-level let in: %s" src)

let private groundsTo (label: string) (expected: SemType) (src: string) : unit =
    Expect.equal (lastLetTy src) expected label

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

            // ---- Vesper.Set G5 roots 2-4 ----------------------------------------
            // Regression gates for three of the four inference/extraction roots fixed
            // while clearing the Vesper.Set G5 analysis wall (the four deferred
            // interfaces). Each is a leaked-free-`TyVar` (or `unify` mismatch) that
            // `ResolvedTypes` flags on otherwise-clean F#. Root 1 (`objnull` extracts
            // to `obj`) is a contract-extraction gate and lives in
            // SemanticAnalysis.Tests `VesperLibTests`; roots 2-4 are pure front-end
            // resolution and gate here.
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

            // ---- Arithmetic-operator `default` resolution ----------------------
            // `(+)`'s contract carries a *chain* of `default` constraints ending in
            // `default ^T1 : int` (ops-platform.fsi). When both operands are free
            // (`a + b`, no literal to pin a type), the chain must ground EVERY
            // participating typar to `int`. A prior bug in `applyDefaults`
            // discarded an unfired chained default (`default ^T2 : ^T3`) before the
            // fixpoint could retry it once `^T3` had defaulted, so `a`/result
            // grounded but the second operand leaked (`int -> 'b -> int`) or, in a
            // tuple param, froze to `?ungrounded-operator`. These pin the full
            // grounding so a regression points straight back here.
            // ---- Typar grounding across nesting / module↔class boundaries -------
            // Two `set.fs`-shaped grounding gaps that leak a bare inference `TyVar`
            // past the front end (`ResolvedTypes` misses it) and surface only at
            // codegen as `FTUnknown "?ungrounded-operator"`. Both compile a *library*
            // end-to-end; the assertion is that codegen completes — pre-fix each
            // threw at contract extraction.
            testList
                "TyparGroundingAcrossBoundaries"
                [
                    // A nested `let rec loop (t': Tree<'T>) acc` inside a generic
                    // module function: the inner curried lambda (over `acc`) captures
                    // the outer param `t' : Tree<'T>`. Pre-fix the nested binding minted
                    // a *fresh* `'T` (a new typar scope per binding), generalised `loop`
                    // over it independently, and left the closure-capture occurrence's
                    // `'T` an ungrounded `TyVar`. Fixed by inheriting the enclosing
                    // binding's typar scope (`inferBinding`), so nested `'T` *is* the
                    // function's `'T` (F# lexical typar scoping).
                    test "nested let-rec inner-lambda capture of an enclosing-typar value grounds" {
                        compileSourceTo
                            (ProjectInfo.library "NestedCaptureGrounds")
                            (String.concat
                                "\n"
                                [
                                    "namespace N"
                                    "type Tree<'T>(k: 'T) ="
                                    "    member _.Key = k"
                                    "    member _.Done = true"
                                    "module M ="
                                    "    let toCount (t: Tree<'T>) (seed: int) ="
                                    "        let rec loop (t': Tree<'T>) acc ="
                                    "            if t'.Done then acc"
                                    "            else loop t' (acc + 1)"
                                    "        loop t seed"
                                ])
                        |> ignore
                    }

                    // A class member (`Box.Add`) calls an *earlier* sibling-module
                    // function (`TreeM.add`) that has an *unannotated* parameter
                    // (`k`, grounded `'T` only by its body). Pre-fix the member was
                    // typed against `prebindModuleFunctionSchemes`' annotation-only
                    // stand-in, which over-generalised `k` into a fresh typar; the
                    // member's `value` argument bound it and never grounded, leaking
                    // into the member signature. Fixed by typing bodies in declaration
                    // order, so `TreeM.add`'s real scheme (`k : 'T`) exists when
                    // `Box.Add` types and `value` pins to `'T`.
                    test "class member calling an earlier module fn with an unannotated param grounds" {
                        compileSourceTo
                            (ProjectInfo.library "MemberCallsModuleFn")
                            (String.concat
                                "\n"
                                [
                                    "namespace N"
                                    "open System.Collections.Generic"
                                    "type Tree<'T>(k: 'T) ="
                                    "    member _.Key = k"
                                    "module TreeM ="
                                    "    let add (comparer: IComparer<'T>) k (t: Tree<'T>) : Tree<'T> ="
                                    "        let _ = comparer.Compare(k, t.Key)"
                                    "        t"
                                    "type Box<'T>(comparer: IComparer<'T>, tree: Tree<'T>) ="
                                    "    member s.Comparer = comparer"
                                    "    member s.Tree = tree"
                                    "    member s.Add value : Box<'T> = Box<'T>(s.Comparer, TreeM.add s.Comparer value s.Tree)"
                                ])
                        |> ignore
                    }
                ]

            testList
                "ArithmeticDefaults"
                [
                    let tyInt = BuiltinTypes.tyInt
                    let tyFun a b = SemType.TyFun(a, b)
                    let tyTup xs = SemType.TyTuple(EqArray.ofList xs)

                    test "curried `a + b` grounds both operands and result to int" {
                        groundsTo "curried" (tyFun tyInt (tyFun tyInt tyInt)) "let g a b = a + b"
                    }

                    test "tuple-param `fun (a, b) -> a + b` grounds elements to int" {
                        groundsTo "tuple-lambda" (tyFun (tyTup [ tyInt; tyInt ]) tyInt) "let g = fun (a, b) -> a + b"
                    }

                    test "tuple-param fun-form `let g (a, b) = a + b` grounds to int" {
                        groundsTo "tuple-fun" (tyFun (tyTup [ tyInt; tyInt ]) tyInt) "let g (a, b) = a + b"
                    }

                    test "one literal operand still grounds the free operand (a + 1)" {
                        groundsTo "lit-operand" (tyFun tyInt tyInt) "let g a = a + 1"
                    }

                    test "chained arithmetic `a + b + c` grounds all three to int" {
                        groundsTo
                            "three-operand"
                            (tyFun tyInt (tyFun tyInt (tyFun tyInt tyInt)))
                            "let g a b c = a + b + c"
                    }
                ]
        ]
