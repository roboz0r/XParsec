module XParsec.FSharp.Codegen.Js.Tests.AnchorFileLocalityTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A file's own trees are anchored in THAT file's tokens and nothing else. Inline expansion is
// what would break it: an anchor carried in from a library file is still a number in range, so
// it resolves to some OTHER token rather than faulting. Every inline call stays an EDGE.

// The specialization table's entries keep the anchors their body was written at, in its own
// file, so they are a different index space and are asserted in `SpecializationTableTests`.

/// Every anchor the file's own declarations carry, resolved against the file's OWN token
/// stream. A library file is longer than any of these snippets, so an anchor that leaked in
/// indexes past the end of this one, which is what makes the in-range check discriminate.
let private checkAnchors (what: string) (input: string) =
    let lexed, _ = parseFile input
    let pool = TastPoolBuilder.openOver (frozenOf input)

    let check (where: string) (stored: Anchor) =
        match stored.Index with
        | ValueNone -> ()
        | ValueSome i ->
            Expect.isLessThan
                (int i)
                lexed.Tokens.Length
                (sprintf "%s: %s anchor %d is past the end of this file's tokens" what where (int i))

            Expect.notEqual
                lexed.Tokens[i].Token
                Token.EOF
                (sprintf "%s: %s anchor %d names this file's EOF, so it is no node's position" what where (int i))

    let rec checkPat (p: TastAccessor.PatId) =
        check "pat" (TastAccessor.patTok p)

        match TastAccessor.patBoundVar p with
        | ValueSome b -> check "bound variable" (TastPoolBuilder.boundVarTok pool b)
        | ValueNone -> ()

        for k in TastAccessor.patChildren p do
            checkPat k

    let rec checkExpr (e: TastAccessor.ExprId) =
        check "expr" (TastAccessor.exprTok e)

        for p in TastAccessor.exprPatChildren e do
            checkPat p

        for c in TastAccessor.exprChildren e do
            checkExpr c

    for d in TastAccessor.roots pool do
        // Every expression a declaration carries, MEMBER BODIES included: coverage the
        // declaration shape defines, rather than one a walk here could drift from.
        TastAccessor.mapDeclBodies
            (fun e ->
                checkExpr e
                e
            )
            d
        |> ignore

        match d with
        | TastAccessor.DLet l -> checkPat l.Binding.Pattern
        | TastAccessor.DLetGroup g ->
            for m in g.Members do
                checkPat m.Pattern
        | _ -> ()

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
