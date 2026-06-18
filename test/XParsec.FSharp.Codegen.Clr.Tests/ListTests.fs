module XParsec.FSharp.Codegen.Clr.Tests.ListTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// List literals: the cons-chain construction (`Slice4` anchor for the TAST
// shape) and its runtime printing, plus a `[1; 2; 3]` over a program's *own*
// declared `List<'T>` union (`Rung2` anchor). The list type retargets onto the
// Vesper cons-list; `%A` here is the FSharp.Core cold-path printer.

let private lines xs = String.concat "\n" xs

[<Tests>]
let tests =
    testList
        "Lists"
        [
            test "`printfn \"%A\" [1; 2; 3]` analyses clean as a Cons/Nil chain over list<int>" {
                // `%A` of a list lowers to a `Format` node (P3 step 2) — the cons
                // chain rides as the `Structured` hole's argument. This test pins the
                // *list literal*'s construction (Cons/Nil chain typed `list<int>`),
                // now reached through the hole rather than the cold-path App arg.
                let tast = analyse "printfn \"%A\" [1; 2; 3]"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let listTy =
                    SemType.TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton (TyConst("int", EqArray.empty)))

                match tast.Decls with
                | EqList [ TDecl.Expression(TExpr.Format(_, segs, _, _), _) ] ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole,
                                       TExpr.UnionCons("Cons",
                                                       EqList [ TExpr.Const(TConstValue.Int 1, _, _)
                                                                TExpr.UnionCons("Cons",
                                                                                EqList [ TExpr.Const(TConstValue.Int 2,
                                                                                                     _,
                                                                                                     _)
                                                                                         TExpr.UnionCons("Cons",
                                                                                                         EqList [ TExpr.Const(TConstValue.Int 3,
                                                                                                                              _,
                                                                                                                              _)
                                                                                                                  TExpr.UnionCons("Nil",
                                                                                                                                  EqList [],
                                                                                                                                  _,
                                                                                                                                  _) ],
                                                                                                         _,
                                                                                                         _) ],
                                                                                _,
                                                                                _) ],
                                                       outerTy,
                                                       _)) ] ->
                        // printf-shared-core step (d): the hole carries its classified
                        // `Source`; `%A` is a `PercentA` (the old `Structured` kind).
                        let isPercentA =
                            match hole.Source with
                            | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA _) -> true
                            | _ -> false

                        Expect.isTrue isPercentA "%A is a structural (PercentA) hole"
                        Expect.equal outerTy listTy "the hole's arg is the cons chain typed list<int>"
                    | other -> failtestf "unexpected Format segments: %A" other
                | _ -> failtestf "unexpected list TAST: %A" tast.Decls
            }

            test "`printfn \"%A\" [1]` prints [1] (one Cons over Nil + list-typed Invoke)" {
                let _, artifact = compileSource "ListSingle" "printfn \"%A\" [1]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1]" "single-element list constructed and printed"
            }

            test "`printfn \"%A\" [1; 2; 3]` prints [1; 2; 3] (recursive tail + post-order calls)" {
                let _, artifact = compileSource "ListChain" "printfn \"%A\" [1; 2; 3]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "the full cons chain constructed and printed"
            }

            test "`let nums = [1; 2; 3]` / `printfn \"%A\" nums` prints [1; 2; 3] (list-typed local)" {
                let _, artifact =
                    compileSource "ListLocal" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "list stored to a local, reloaded, and printed"
            }

            // A `[1; 2; 3]` literal that binds against the *program's own* declared
            // `List<'T>` union (with the `[]` / `(::)` syntactic constructors), then
            // folds it — no FSharp.Core, no external list.
            test "`[1; 2; 3]` runs against the program's own declared list union (prints 6)" {
                let src =
                    lines
                        [
                            "type List<'T> ="
                            "    | ([]): List<'T>"
                            "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                            "and 'T list = List<'T>"
                            "let rec sum xs ="
                            "    match xs with"
                            "    | Empty -> 0"
                            "    | Cons(h, t) -> h + sum t"
                            "printfn \"%d\" (sum [1; 2; 3])"
                        ]

                let tast, artifact = compileSource "ListLitOwnUnion" src

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "a `[1;2;3]` over our own list + a concrete printf references no FSharp.Core"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "[1; 2; 3] built + folded over the program's own List<'T>"
            }
        ]
