module XParsec.FSharp.Codegen.Clr.Tests.InferResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Systematic, source-synthetic coverage of `Infer`'s *application / construction*
// resolution surface. Each case is a minimal idiomatic F# program fed through the
// real analysis pipeline (`buildContract defaultManifests` → `Pipeline.analyseSem`),
// asserting on error-severity diagnostics. Because the pipeline runs `ResolvedTypes`,
// an empty error list is a strong claim: the program both type-checks (no `unify`
// mismatch) AND leaves no free `TyVar` in the frozen TAST.
//
// The matrix crosses the axes that `Infer` must handle consistently:
//   * `new T(args)` (Expr.New)  vs  the `new`-less ctor sugar `T(args)` / `T args`
//   * spaced `T (args)` (Expr.App)  vs  non-spaced `T(args)` (Expr.HighPrecedenceApp)
//   * the constructed value used in a *pinning* context (bound / annotated) vs a
//     *non-pinning* one (an argument to a generic-parameter sink like `raise`)
//   * single-arg / multi-arg / nullary / generic constructors
//   * external instance-method chains off a freshly-constructed object argument

let private errorsOf (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file
    tast.Diagnostics |> Diagnostic.errors

/// Assert a synthetic program produces no error-severity diagnostics.
let private clean (label: string) (src: string) : unit =
    let errs = errorsOf src
    Expect.isEmpty errs (sprintf "%s — expected clean, got: %A" label (errs |> List.map (fun d -> d.Message)))

/// The inferred `SemType` of the program's last top-level `let` — used to assert
/// a precise grounding (not just "no errors": an over-generalised `int -> 'b ->
/// int` is error-free yet wrong).
let private lastLetTy (src: string) : SemType =
    let provider = ClrSymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file

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
            testList
                "CtorSugarSpaced"
                [
                    test "spaced ctor `Exn (x)`, bound" { clean "spaced-bound" "let e = System.Exception (\"x\")" }
                    test "spaced ctor under raise" {
                        clean "spaced-raise" "let f () = raise (System.Exception (\"x\"))"
                    }
                ]

            // ---- External ctor: `new`-less sugar, NON-SPACED (HighPrecedenceApp) ---
            // Non-spaced `T(args)` and spaced `T (args)` must ground identically;
            // a non-pinning sink (`raise`'s fully-generic `'TException`) is the
            // distinguishing case — a leaked `TyVar` here stays free.
            testList
                "CtorSugarNonSpaced"
                [
                    test "non-spaced ctor `Exn(x)`, bound, is grounded [gap A/B]" {
                        clean "nonspaced-bound" "let e = System.Exception(\"x\")"
                    }
                    test "non-spaced ctor via `open` + single ident, bound [gap A/B]" {
                        clean "nonspaced-open" "open System\nlet e = Exception(\"x\")"
                    }
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
            // not a semantic distinction.
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
            // A fluent chain off a freshly-constructed external object argument. The overload
            // pick must consult call-site arg types; otherwise `Append(string)` grabs
            // `Append(char[], int, int)` ("string vs TyTuple").
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

            // ---- SetG5 roots 2-4 ------------------------------------------------
            // Each is a leaked-free-`TyVar` (or `unify` mismatch) that `ResolvedTypes`
            // flags on otherwise-clean F#.
            testList
                "SetG5Roots"
                [
                    // Root 2 — `inferNew` secondary-ctor lookup. A `[<Struct>]` with no
                    // primary ctor has `CtorParams = [||]`; `new T(arg)` must fall back to
                    // a matching-arity `SecondaryCtors` entry rather than unifying against
                    // `unit` primary params.
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

                    // Root 3 — `inferILIntrinsic` must pin each `Expr.Null` operand to the
                    // first non-null operand. `isNull` body is `(# "ceq" value null : bool #)`;
                    // the `null` leaf must not mint an independent fresh `TyVar`.
                    test "`isNull` on a reference operand grounds the `null` leaf [root 3: ILIntrinsic null pinning]" {
                        clean "isnull-string" "let f (s: string) = isNull s"
                    }

                    // Root 4 — resolving a local template must always
                    // `deriveInlineTypeArgs`. A generic `let inline` expanded with zero
                    // type args leaves the callee's generalised typars (e.g. `:?> 'T`
                    // result typar) free in the caller's frozen TAST: beta-reduction
                    // binds value params but not typars. Only triggers when the callee is
                    // `inline` AND generic — monomorphic locals derive `[||]` safely.
                    test
                        "local generic `let inline` cast helper from a non-inline fn [root 4: a local template derives its type args]" {
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

            // ---- Typar grounding across nesting / module↔class boundaries -------
            // Grounding gaps that leak a bare inference `TyVar` past the front end
            // (`ResolvedTypes` misses it) and surface only at codegen. Both compile a
            // *library* end-to-end; the assertion is that codegen completes.
            testList
                "TyparGroundingAcrossBoundaries"
                [
                    // Nested `let rec` inside a generic module function: inner bindings
                    // must inherit the enclosing binding's typar scope (F# lexical typar
                    // scoping), not mint an independent fresh `'T` per binding.
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

                    // A class member calls an earlier sibling-module function with an
                    // unannotated parameter. Bodies must be typed in declaration order so
                    // the sibling's real scheme exists (and `k : 'T` is ground) when the
                    // member types; an annotation-only stand-in over-generalises `k`.
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

            // ---- Arithmetic-operator `default` resolution ----------------------
            // `(+)`'s contract has a chain of `default` constraints ending in
            // `default ^T1 : int`; with both operands free the chain must ground
            // EVERY participating typar. Chained defaults must be retried at fixpoint,
            // not discarded on the first pass.
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
