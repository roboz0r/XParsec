module XParsec.FSharp.SemanticAnalysis.Tests.UnificationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let private typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.TypeVar.TryGetValue key with
    | ValueSome tv -> Unification.zonk (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key

[<Tests>]
let tests =
    testList
        "Unification"
        [
            test "integer literal types as int" {
                // "let x = 1" — pattern x at offset 4, RHS literal at offset 8.
                let ctx = analyse "let x = 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "infix `+` types as int -> int -> int -> int (mono operator)" {
                // "let x = 1 + 2" — pat x's type should be int.
                let ctx = analyse "let x = 1 + 2"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            test "lambda body type propagates to function type" {
                // "let f = fun x -> x + 1" — f : int -> int.
                let ctx = analyse "let f = fun x -> x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "function-form let infers parameter type from body" {
                // "let f x = x + 1" — f : int -> int.
                let ctx = analyse "let f x = x + 1"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                let expected = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : int -> int"
            }

            test "application instantiates identity to argument type" {
                // "let result = let id = fun x -> x in id 42" — wrap in a named
                // binding so the let-in is unambiguously an expression (avoids the
                // parser disambiguating into a different top-level shape). `result`
                // pat is at offset 4.
                let ctx = analyse "let result = let id = fun x -> x in id 42"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "result : int"
            }

            test "type mismatch on int + bool emits a diagnostic" {
                let ctx = analyse "let x = 1 + true"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "Type mismatch diagnostic emitted"
            }

            test "ident `true` types as bool via provider" {
                let ctx = analyse "let b = true"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyBool "b : bool"
            }

            test "nested infix: 1 + 2 * 3 types as int" {
                let ctx = analyse "let x = 1 + 2 * 3"
                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) MockBuiltins.tyInt "x : int"
            }

            // ---- Records ----

            test "record literal infers record type from field set" {
                // "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"
                //  Newline at 27, "let r = " puts pat r at offset 32.
                let ctx = analyse "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }"

                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                Expect.equal (typeOf ctx patKey) (TyRecord "R") "r : TyRecord R"

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

            test "field access on annotated parameter types as the field type" {
                let ctx = analyse "type R = { X: int }\nlet f (r: R) = r.X"

                let patKey = NodeKey.ofSource 24 NodeKind.PatIdent
                let expected = TyFun(TyRecord "R", MockBuiltins.tyInt)
                Expect.equal (typeOf ctx patKey) expected "f : R -> int"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "field access on free TyVar pinned by use" {
                // `let f r = r.X in let _ = f { X = 1 }` — the call pins r to R.
                let ctx = analyse "type R = { X: int }\nlet f r = r.X\nlet u = f { X = 1 }"

                let fatigueFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Cannot resolve field")

                Expect.isFalse fatigueFree "no unresolved-field diagnostic when use pins receiver"
            }

            test "record clone types as source record" {
                // type R = { X: int; Y: int }
                // let p = { X = 1; Y = 2 }
                // let q = { p with Y = 5 }
                let ctx =
                    analyse "type R = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet q = { p with Y = 5 }"

                // p : R at offset 32, q : R at offset 57.
                let qKey = NodeKey.ofSource 57 NodeKind.PatIdent
                Expect.equal (typeOf ctx qKey) (TyRecord "R") "q : R"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "record clone with unknown field diagnoses" {
                let ctx = analyse "type R = { X: int }\nlet p = { X = 1 }\nlet q = { p with Z = 5 }"

                let hasNoField =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "has no field")

                Expect.isTrue hasNoField "unknown-field diagnostic emitted"
            }
        ]
