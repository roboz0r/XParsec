module XParsec.FSharp.SemanticAnalysis.Tests.UnificationBasicsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

/// Did `((e) : t)` and `let g : t = e` each accept? Both spellings are one seam, so a
/// disagreement between the two results is a defect, not a policy difference.
let private annotationSeams (decls: string) (e: string) (t: string) =
    let ascribed = analyse (sprintf "%slet f = ((%s) : %s)" decls e t)
    let bound = analyse (sprintf "%slet g : %s = %s" decls t e)
    List.isEmpty (errors ascribed), List.isEmpty (errors bound)

[<Tests>]
let tests =
    testList
        "Unification.Basics"
        [
            test "integer literal types as int" {
                // pat x at offset 4.
                let ctx = analyse "let x = 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "infix `+` types as int -> int -> int -> int (mono operator)" {
                let ctx = analyse "let x = 1 + 2"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "custom infix operator resolves through its composed compiled name" {
                // `++` has no dedicated token: the `op_PlusPlus` spelling is composed
                // from source text and resolved like a well-known operator.
                let intFt = FTConst(RuntimeNames.intKey, EqArray.empty)

                let opProvider =
                    providerOfValues
                        [
                            ExternalSymbols.monoFrozen
                                (SymbolKeyOps.inNamespace "")
                                "op_PlusPlus"
                                (FTFun(intFt, FTFun(intFt, intFt)))
                        ]

                let provider = ExternalSymbolProviders.composite [ opProvider; realProvider.Value ]

                let lexed, file = parseFile "let x = 1 ++ 2"
                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                NameResolution.run ctx file
                Unification.run ctx file

                Expect.isEmpty (errors ctx) "1 ++ 2 resolves and types"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "unknown custom infix operator diagnoses its composed compiled name" {
                let ctx = analyse "let x = 1 >=> 2"

                let hasComposedName =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "op_GreaterEqualsGreater")

                Expect.isTrue
                    hasComposedName
                    (sprintf
                        "diagnostic should carry the composed name; got %A"
                        (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> Seq.toList))
            }

            test "lambda body type propagates to function type" {
                let ctx = analyse "let f = fun x -> x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "function-form let infers parameter type from body" {
                let ctx = analyse "let f x = x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "application instantiates identity to argument type" {
                // Wrap in a named binding so the let-in is unambiguously an
                // expression (else the parser picks a different top-level shape).
                let ctx = analyse "let result = let id = fun x -> x in id 42"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "result : int"
            }

            test "type mismatch on int + bool emits a diagnostic" {
                // Trait verdicts are diagnosed at inline expansion, which this
                // unification-only harness does not reach, so the full pipeline runs here.
                let tast =
                    let lexed, file = parseFile "let x = 1 + true"
                    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

                let hasMismatch =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "The type 'bool' does not support the operator '+'")

                Expect.isTrue hasMismatch "Type mismatch diagnostic emitted"
            }

            // `null`, `undefined` and `unit` each resolve to their own opaque `TyConst`, so
            // no two of them unify — even though a `unit` VALUE also lowers to JS
            // `undefined`.
            test "`undefined` and `unit` resolve to distinct types and do not unify" {
                let ctx = analyse "let f (x: undefined) : unit = x"
                // `x : undefined`, pat at offset 7.
                let patKey = NodeKey.ofSource 7 NodeKind.PatIdent

                Expect.equal (typeOf ctx patKey) (TyConst(RuntimeNames.undefinedKey, EqArray.empty)) "x : undefined"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "returning an `undefined`-typed value as `unit` must mismatch"
            }

            test "`null` and `unit` resolve to distinct types and do not unify" {
                let ctx = analyse "let f (x: unit) : null = x"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "returning a `unit`-typed value as `null` must mismatch"
            }

            test "`null` and `undefined` are distinct types and do not unify" {
                let ctx = analyse "let f (x: null) : undefined = x"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "`null` and `undefined` must not unify"
            }

            test "a function annotation admits a lambda in both spellings" {
                Expect.equal
                    (annotationSeams "" "fun x -> x" "int -> int")
                    (true, true)
                    "`int -> int` accepted, ascribed and bound"
            }

            test "a non-function annotation rejects a lambda in both spellings" {
                Expect.equal
                    (annotationSeams "" "fun x -> x" "string")
                    (false, false)
                    "`string` rejected, ascribed and bound"
            }

            // `subsumes` requires each peeled domain be invariant-`Equal` to its `Fun`
            // argument, so an unpinned `'a` cannot pin THROUGH the nominal: both spellings
            // fall to `unify` and reject. F# admits the analogue, `(fun x -> x) : FSharpFunc<_,_>`.
            test "an unpinned lambda does not pin through the platform function nominal" {
                Expect.equal
                    (annotationSeams "" "fun x -> x" "Fun<int, int>")
                    (false, false)
                    "`Fun<int,int>` rejected, ascribed and bound"
            }

            test "a ground function is admitted the same way in both spellings" {
                let ground = annotationSeams "let h (x: int) : int = x\n" "h" "Fun<int, int>"
                Expect.equal ground (true, true) "`Fun<int,int>` accepted, ascribed and bound"
            }

            test "a strict supertype is admitted the same way in both spellings" {
                let decls =
                    "type Base() =\n    member this.B = 1\ntype Derived() =\n    inherit Base()\n"

                Expect.equal
                    (annotationSeams decls "Derived()" "Base")
                    (true, true)
                    "`Base` accepted, ascribed and bound"
            }

            test "Using a TyUnknown-typed external value emits a use-site diagnostic" {
                // A contract val whose signature named an out-of-scope type bakes an
                // `FTUnknown` into the symbol itself. Referencing it must diagnose once
                // that `FTUnknown` reaches unification, not silently succeed.
                let brokenProvider =
                    providerOfValues
                        [
                            ExternalSymbols.monoFrozen
                                (SymbolKeyOps.inNamespace "")
                                "broken"
                                (FTUnknown(UnknownReason.UndefinedName "Missing.Thing"))
                        ]

                let provider =
                    ExternalSymbolProviders.composite [ brokenProvider; realProvider.Value ]

                let input = "let y = broken"
                let lexed, file = parseFile input

                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                NameResolution.run ctx file
                Unification.run ctx file

                let hasUnknownDiag =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "could not be resolved")

                Expect.isTrue
                    hasUnknownDiag
                    (sprintf
                        "use-site TyUnknown diagnostic expected; diagnostics: %A"
                        (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> Seq.toList))
            }

            // Three uses of a two-position signature meet `unify` six times, and there is one
            // thing to fix. The message must identify the construct, because the sentinel carries
            // no position back from the extraction that minted it.
            test "an unfreezable signature reports once, identifying the construct" {
                let unfreezable = ExternalSignature.unfreezable "'Widget' is a delegate type"

                let brokenProvider =
                    providerOfValues
                        [
                            ExternalSymbols.monoFrozen
                                (SymbolKeyOps.inNamespace "")
                                "broken"
                                (FTFun(unfreezable, unfreezable))
                        ]

                let provider =
                    ExternalSymbolProviders.composite [ brokenProvider; realProvider.Value ]

                let lexed, file = parseFile "let a = broken 1\nlet b = broken 2\nlet c = broken 3"

                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                NameResolution.run ctx file
                Unification.run ctx file

                let msgs = [ for d in ctx.Diagnostics -> d.Message ]

                Expect.hasLength msgs 1 (sprintf "one diagnostic for the one broken contract, got %A" msgs)
                Expect.stringContains msgs.[0] "'Widget' is a delegate type" "the phrase calls out the construct"
                Expect.stringContains msgs.[0] "not yet supported" "a feature gap, not a missing package"
                Expect.isFalse (msgs.[0].Contains "dependency missing") "not attributed to a missing dependency"
            }

            test "ident `true` types as bool via provider" {
                let ctx = analyse "let b = true"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyBool "b : bool"
            }

            test "nested infix: 1 + 2 * 3 types as int" {
                let ctx = analyse "let x = 1 + 2 * 3"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) BuiltinTypes.tyInt "x : int"
            }

            test "record literal infers record type from field set" {
                // pat r at 32: 27-char type decl + "let r = ".
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", EqArray.empty)) "r : TyRecord R"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ambiguous field set requires qualifier" {
                let ctx =
                    analyse "type R = { X: int; Y: int }\ntype S = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let hasAmbig =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "ambiguous")

                Expect.isTrue hasAmbig "ambiguous-field-set diagnostic emitted"
            }

            test "unknown field set diagnoses" {
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Z = 3 }"

                let hasUnknown =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "No record type matches")

                Expect.isTrue hasUnknown "unknown-field-set diagnostic emitted"
            }

            test "record obj field accepts a value initialiser (implicit box)" {
                // `{ X = 5 }` into an `obj` field: F# boxes the int, so the field init coerces.
                // pat r at 24: 19-char type decl + "\n" + "let r = ".
                let ctx = analyse "type R = { X: obj }\nlet r = { X = 5 }"

                let patKey = NodeKey.ofSource 24 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord("R", EqArray.empty)) "r : R"
                Expect.isEmpty ctx.Diagnostics "the value boxes into the obj field — no mismatch"
            }

            test "record field still rejects an unrelated initialiser type" {
                // The coercion is obj / subtype only: a string into an `int` field is not
                // assignable, so it stays a mismatch.
                let ctx = analyse "type R = { X: int }\nlet r = { X = \"s\" }"

                let hasError = ctx.Diagnostics |> Seq.exists Diagnostic.isError

                Expect.isTrue hasError "a string into an int field is still a type error"
            }

            test "field access on annotated parameter types as the field type" {
                let ctx = analyse "type R = { X: int }\nlet f (r: R) = r.X"

                let patKey = NodeKey.ofSource 24 NodeKind.PatIdent
                let expected = TyFun(TyRecord("R", EqArray.empty), BuiltinTypes.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : R -> int"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "field access on free TyVar pinned by use" {
                // The use `f { X = 1 }` pins r to R.
                let ctx = analyse "type R = { X: int }\nlet f r = r.X\nlet u = f { X = 1 }"

                let fatigueFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Cannot resolve field")

                Expect.isFalse fatigueFree "no unresolved-field diagnostic when use pins the object argument"
            }

            test "record clone types as source record" {
                let ctx =
                    analyse "type R = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet q = { p with Y = 5 }"

                // q pat at offset 57.
                let qKey = NodeKey.ofSource 57 NodeKind.PatIdent
                Expect.equal (typeOf ctx qKey) (TyRecord("R", EqArray.empty)) "q : R"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "record clone with unknown field diagnoses" {
                let ctx = analyse "type R = { X: int }\nlet p = { X = 1 }\nlet q = { p with Z = 5 }"

                let hasNoField =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "has no field")

                Expect.isTrue hasNoField "unknown-field diagnostic emitted"
            }

            test "nullary ctor reference types as the union" {
                // pat p at 21: 16-char type decl + "let p = ".
                let ctx = analyse "type S = | Point\nlet p = Point"

                let patKey = NodeKey.ofSource 21 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "p : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "single-arg ctor application types as the union" {
                // pat c at 31: 26-char type decl + "let c = ".
                let ctx = analyse "type S = | Circle of float\nlet c = Circle 1.0"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "c : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "multi-arg ctor application takes a tuple" {
                // pat r at 37: 32-char type decl + "let r = ".
                let ctx = analyse "type S = | Rect of float * float\nlet r = Rect(2.0, 3.0)"

                let patKey = NodeKey.ofSource 37 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("S", EqArray.empty)) "r : S"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "ctor used as a value types as a function" {
                // pat f at 31: 26-char type decl + "let f = ".
                let ctx = analyse "type S = | Circle of float\nlet f = Circle"

                let patKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(BuiltinTypes.tyFloat, TyUnion("S", EqArray.empty))
                Expect.equal (typeOf ctx patKey) expected "f : float -> S"
            }

            test "ctor pattern unifies scrutinee with TyUnion" {
                // Via the `Circle r` arm the scrutinee unifies with TyUnion("S", EqArray.empty),
                // so `area : S -> float`.
                let ctx =
                    analyse "type S = | Circle of float\nlet area s = match s with | Circle r -> r"

                // area pat at 31 (27-char type decl + "let "), s param at 36.
                let areaKey = NodeKey.ofSource 31 NodeKind.PatIdent
                let expected = TyFun(TyUnion("S", EqArray.empty), BuiltinTypes.tyFloat)
                Expect.equal (typeOf ctx areaKey) expected "area : S -> float"
            }

            test "ambiguous ctor name requires qualifier" {
                // Two unions share an `Ok` case.
                let ctx = analyse "type R1 = | Ok of int\ntype R2 = | Ok of float\nlet x = Ok 1"

                let hasAmbig =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Ambiguous constructor")

                Expect.isTrue hasAmbig "ambiguous-ctor diagnostic emitted"
            }

            test "qualified ctor resolves an ambiguous case name" {
                let ctx =
                    analyse "type R1 = | Ok of int\ntype R2 = | Ok of float\nlet x = R2.Ok 1.0"

                // Pat x starts at offset 50: 21 (type R1...) + 1 (\n) + 23 (type R2...) + 1 (\n) + 4 ("let ").
                let patKey = NodeKey.ofSource 50 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyUnion("R2", EqArray.empty)) "x : R2"
            }

            ptest "GAP: a static-optimization clause body is never checked against the declared result" {
                // fsc checks each clause under its constraint substitution (^T := int here), so a
                // clause returning a string where the declared result is bool is FS0001. This
                // compiler types the clause body only to solve its own subtrees
                // (inferLibraryOnlyStaticOptimization), and honest checking means unifying under
                // each clause's substitution, so no diagnostic surfaces today.
                let ctx =
                    analyse "let inline eq (x: ^T) (y: ^T) : bool =\n    true\n    when ^T: int = \"not a bool\""

                Expect.isNonEmpty (errors ctx) "the ill-typed clause body should be diagnosed"
            }
        ]
