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
        ]
