module XParsec.FSharp.Codegen.Clr.Tests.InferResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Application / construction resolution over minimal synthetic programs. The pipeline
// runs `ResolvedTypes`, so an EMPTY error list is a strong claim: the program both
// type-checks and leaves no free `TyVar` in the frozen TAST.

let private errorsOf (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (LexedFile.ofText lexed) file
    tast.Diagnostics |> Diagnostic.errors

let private clean (label: string) (src: string) : unit =
    let errs = errorsOf src
    Expect.isEmpty errs (sprintf "%s: expected clean, got: %A" label (errs |> List.map (fun d -> d.Message)))

/// The inferred type of the program's last top-level `let`, for asserting a precise
/// grounding: an over-generalised `int -> 'b -> int` is error-free yet wrong.
let private lastLetTy (src: string) : SemType =
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (LexedFile.ofText lexed) file

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
            // The grounded baseline: `new` pins the result to the class type whatever the
            // surrounding context does.
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

            testList
                "CtorSugarSpaced"
                [
                    test "spaced ctor `Exn (x)`, bound" { clean "spaced-bound" "let e = System.Exception (\"x\")" }
                    test "spaced ctor under raise" {
                        clean "spaced-raise" "let f () = raise (System.Exception (\"x\"))"
                    }
                ]

            // Non-spaced `T(args)` and spaced `T (args)` must ground identically. A
            // non-pinning sink (`raise`'s fully-generic parameter) is the distinguishing
            // case, because a `TyVar` that leaks there stays free.
            testList
                "CtorSugarNonSpaced"
                [
                    test "non-spaced ctor `Exn(x)`, bound, is grounded" {
                        clean "nonspaced-bound" "let e = System.Exception(\"x\")"
                    }
                    test "non-spaced ctor via `open` + single ident, bound" {
                        clean "nonspaced-open" "open System\nlet e = Exception(\"x\")"
                    }
                    test "non-spaced ctor under raise is grounded" {
                        clean "nonspaced-raise" "let f () = raise (System.InvalidOperationException(\"x\"))"
                    }
                    test "non-spaced ctor under raise, System.Exception" {
                        clean "nonspaced-raise-exn" "let f () = raise (System.Exception(\"x\"))"
                    }
                    test "non-spaced NotSupportedException under raise" {
                        clean "nonspaced-raise-nse" "let f () = raise (System.NotSupportedException(\"x\"))"
                    }
                ]

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

            // The same construction differing ONLY by a space before `(` must produce
            // identical diagnostics: the parser's associativity choice is not semantic.
            testList
                "AppHPAppParity"
                [
                    test "raise (Exn (x))  ≡  raise (Exn(x))" {
                        let spaced = errorsOf "let f () = raise (System.Exception (\"x\"))"
                        let nonSpaced = errorsOf "let f () = raise (System.Exception(\"x\"))"

                        Expect.equal
                            (List.length nonSpaced)
                            (List.length spaced)
                            (sprintf "spaced=%d non-spaced=%d must match" (List.length spaced) (List.length nonSpaced))
                    }
                ]

            // A fluent chain off a freshly-constructed object argument. The overload pick
            // has to consult call-site arg types, or `Append("x")` grabs the three-argument
            // `Append(char[], int, int)`.
            testList
                "InstanceMethodChain"
                [
                    test "StringBuilder().Append(string).ToString()" {
                        clean "sb-1" "let f () = System.Text.StringBuilder().Append(\"x\").ToString()"
                    }
                    test "StringBuilder().Append.Append.ToString() (longer chain)" {
                        clean "sb-2" "let f () = System.Text.StringBuilder().Append(\"x\").Append(\"y\").ToString()"
                    }
                ]

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

            testList
                "GenericCtor"
                [
                    test "ResizeArray<int>() (explicit type app)" {
                        clean "ra-int" "let xs = System.Collections.Generic.List<int>()"
                    }
                ]

            // Each is a free-`TyVar` leak (or `unify` mismatch) that `ResolvedTypes` flags
            // on otherwise-clean F#.
            testList
                "FreeTyVarLeaks"
                [
                    // A `[<Struct>]` with no primary ctor has empty ctor params, so
                    // `new T(arg)` must fall back to a matching-arity secondary ctor rather
                    // than unify its argument against `unit`.
                    test "struct, only an explicit ctor, `new T(arg)`" {
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
                    test "generic struct, only an explicit ctor, `new T<'a>(arg)`" {
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

                    // `isNull`'s body is `(# "ceq" value null : bool #)`, so the `null` operand
                    // has to be pinned to the other one rather than mint a fresh `TyVar`.
                    test "`isNull` on a nullable operand grounds the `null` operand" {
                        clean "isnull-string" "let f (s: string | null) = isNull s"
                    }

                    // Expanding a generic `let inline` with zero type args leaves the
                    // callee's generalised typars free in the caller's frozen TAST, since
                    // beta-reduction binds value parameters but not typars.
                    test "local generic `let inline` cast helper from a non-inline fn" {
                        clean
                            "inline-cast"
                            (String.concat
                                "\n"
                                [
                                    "let inline cast (x: obj) : 'T = x :?> 'T"
                                    "let useCast (x: obj) : string = cast x"
                                ])
                    }
                    test "local generic `let inline` returning a generic record, field-accessed" {
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

            // `TyVar` leaks that get past `ResolvedTypes` and surface only at codegen, so
            // both compile a library end to end and the assertion is that codegen finishes.
            testList
                "TyparGroundingAcrossBoundaries"
                [
                    // F# typar scope is lexical, so a nested `let rec` inherits the
                    // enclosing binding's `'T` rather than minting a fresh one per binding.
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

                    // Bodies must be typed in declaration order, so that the sibling `add`'s
                    // real scheme exists (and its unannotated `k` is ground) by the time the
                    // member types. An annotation-only stand-in over-generalises `k`.
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

            // `(+)`'s contract chains `default` constraints down to `default ^T1 : int`, so
            // with both operands free every participating typar must ground. Chained
            // defaults are retried at fixpoint rather than discarded on the first pass.
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

            // An `obj`-typed slot ABSORBS its argument rather than unifying with it, so a
            // `null` handed to one is pinned by nothing. It settles at `obj` after the walk;
            // without that it reaches the frozen TAST as a free `TyVar`.
            testList
                "NullLiteral"
                [
                    test "null into an `obj` parameter" { clean "null-obj-param" "let f (o: obj) = 0\nlet n = f null" }

                    test "null into an `obj` record field" {
                        clean "null-obj-field" "type Node = { mutable Next: obj }\nlet n = { Next = null }"
                    }

                    // A typed slot pins it, so the settle must leave these alone.
                    test "null into a `string` record field types as string" {
                        clean "null-string-field" "type Node = { mutable Next: string }\nlet n = { Next = null }"
                    }

                    test "null under an annotation types as the annotation" {
                        groundsTo "null-annotated" BuiltinTypes.tyString "let s: string = null"
                    }
                ]

            // A `.member` access whose object argument is still free parks until the type
            // settles, and the deferred lookup must find everything the immediate one does —
            // an explicit `val` field included.
            testList
                "DeferredMemberAccess"
                [
                    test "`val` field read off a cons-pattern binding" {
                        clean
                            "val-field-deferred"
                            "type Frame =\n    val Kind: int\n    new(kind: int) = { Kind = kind }\n\nlet fs = [ Frame(1) ]\nlet k = match fs with | f :: _ -> f.Kind | [] -> 0"
                    }

                    test "mutable `val` field written through a cons-pattern binding" {
                        clean
                            "val-field-deferred-set"
                            "type Frame =\n    val mutable Kids: int list\n    new() = { Kids = [] }\n\nlet fs = [ Frame() ]\nlet f () = match fs with | top :: _ -> top.Kids <- [ 1 ] | [] -> ()"
                    }
                ]
        ]
