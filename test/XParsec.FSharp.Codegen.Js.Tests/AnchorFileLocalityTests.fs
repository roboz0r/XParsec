module XParsec.FSharp.Codegen.Js.Tests.AnchorFileLocalityTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A frozen file's anchor columns index THAT file's `Lexed`, and nothing else.
//
// The way that stops being true is inline expansion: a body compiled from a library file
// is spliced into a consumer's tree, and a token carried across from it names a position
// in the wrong file — a number still in range, so it resolves to some OTHER token rather
// than faulting. `Inline.spliceAt` is what closes it, by moving every spliced node onto
// the call site; these snippets are one per way a body reaches a consuming tree, and the
// check is the one a token-index anchor column performs when it is dereferenced.

/// Every anchor a frozen file carries, resolved against the file's OWN token stream. A
/// token that came from somewhere else either indexes past the end or disagrees with what
/// is actually at that index.
let private checkAnchors (what: string) (input: string) =
    let lexed, _ = parseFile input
    let pools = frozenOf input

    let check (where: string) (t: SyntaxToken) =
        match t.Index with
        | TokenIndex.Virtual -> ()
        | TokenIndex.Regular i ->
            Expect.isLessThan
                (int i)
                lexed.Tokens.Length
                (sprintf "%s: %s anchor %d is past the end of this file's tokens" what where (int i))

            Expect.equal
                lexed.Tokens[i]
                t.PositionedToken
                (sprintf "%s: %s anchor %d names a different token in this file" what where (int i))

    for t in pools.ExprToks do
        check "expr" t

    for t in pools.PatToks do
        check "pat" t

    for t in pools.BinderToks do
        match t with
        | ValueSome t -> check "binder" t
        | ValueNone -> ()

[<Tests>]
let tests =
    testList
        "Codegen.Js anchor file locality"
        [
            test "an operator's cross-package body is anchored in the consuming file" {
                // `+` and `*` splice `Vesper.Core/ops-platform.js.fs`, whose `int` clauses
                // sit thousands of characters into a file this one has never seen.
                checkAnchors "operators" "let x = 1 + 2\nlet y = x * 3\nprintfn \"%d\" y"
            }

            test "a local `let inline` body is anchored at each of its call sites" {
                checkAnchors
                    "local inline"
                    ("let inline sq x = x * x\n"
                     + "let a = sq 3\n"
                     + "let b = sq 4\n"
                     + "printfn \"%d\" (a + b)")
            }

            test "an inline-first lambda parameter is anchored in the consuming file" {
                checkAnchors
                    "lambda param"
                    ("let inline apply f x = f x\n"
                     + "let v = apply (fun n -> n + 1) 2\n"
                     + "printfn \"%d\" v")
            }

            test "an IL-intrinsic member body is anchored at the member call" {
                checkAnchors "member body" "let a = [| 1; 2; 3 |]\nprintfn \"%d\" a.[1]"
            }

            test "structural equality's cross-package body is anchored in the consuming file" {
                checkAnchors "equality" "let e = 1 = 2\nprintfn \"%b\" e"
            }
        ]
