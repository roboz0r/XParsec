module XParsec.FSharp.Codegen.Clr.Tests.ListTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

[<Tests>]
let tests =
    testList
        "Lists"
        [
            test "`printfn \"%A\" [1; 2; 3]` analyses clean as a Cons/Empty chain over List<int>" {
                // `%A` of a list lowers to a `Format` node, and the cons chain rides
                // as the hole's argument rather than a call argument.
                let tast = analyse "printfn \"%A\" [1; 2; 3]"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let listTy =
                    SemType.TyUnion(
                        RuntimeNames.vesperListKey,
                        EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty))
                    )

                match tast.Decls with
                | EqList [ TDecl.Expression(TExpr.Format(_, segs, _, _), _) ] ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole,
                                       TExpr.UnionCons("Cons",
                                                       EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L),
                                                                            _,
                                                                            _)
                                                                TExpr.UnionCons("Cons",
                                                                                EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32,
                                                                                                                          2L),
                                                                                                     _,
                                                                                                     _)
                                                                                         TExpr.UnionCons("Cons",
                                                                                                         EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32,
                                                                                                                                                   3L),
                                                                                                                              _,
                                                                                                                              _)
                                                                                                                  TExpr.UnionCons("Empty",
                                                                                                                                  EqList [],
                                                                                                                                  _,
                                                                                                                                  _) ],
                                                                                                         _,
                                                                                                         _) ],
                                                                                _,
                                                                                _) ],
                                                       outerTy,
                                                       _)) ] ->
                        let isPercentA =
                            match hole.Source with
                            | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA _) -> true
                            | _ -> false

                        Expect.isTrue isPercentA "%A is a structural (PercentA) hole"
                        Expect.equal outerTy listTy "the hole's arg is the cons chain typed List<int>"
                    | other -> failtestf "unexpected Format segments: %A" other
                | _ -> failtestf "unexpected list TAST: %A" tast.Decls
            }

            test "`printfn \"%A\" [1]` prints [1] (one Cons over Empty + list-typed Invoke)" {
                let artifact = compileSource "ListSingle" "printfn \"%A\" [1]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1]" "single-element list constructed and printed"
            }

            test "`printfn \"%A\" [1; 2; 3]` prints [1; 2; 3] (recursive tail + post-order calls)" {
                let artifact = compileSource "ListChain" "printfn \"%A\" [1; 2; 3]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "the full cons chain constructed and printed"
            }

            test "`let nums = [1; 2; 3]` / `printfn \"%A\" nums` prints [1; 2; 3] (list-typed local)" {
                let artifact = compileSource "ListLocal" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "list stored to a local, reloaded, and printed"
            }

            // Without `Vesper.List` there is no cons-list for `[…]` to resolve to. The
            // diagnostic identifies the missing dependency rather than the platform: a package
            // that does not build for the target is already fatal at manifest resolution.
            let analyseWithoutList (src: string) : Diagnostic list =
                let provider = ClrSymbolProviders.buildContract [ vesperCorePackage ]
                let lexed, file = parseFile src

                Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file
                |> fun tast -> tast.Diagnostics |> Diagnostic.errors

            let expectMissingListDep (src: string) =
                match analyseWithoutList src with
                | [] -> failtestf "expected an error for %s with no Vesper.List reference" src
                | errors ->
                    Expect.isTrue
                        (errors
                         |> List.exists (fun d -> d.Kind = Kind.IntrinsicNotInScope Intrinsic.ConsList))
                        (sprintf "the diagnostic names the missing dependency, got %A" (errors |> List.map _.Kind))

            test "`[1; 2; 3]` with no Vesper.List reference identifies the missing dependency" {
                expectMissingListDep "let xs = [1; 2; 3]"
            }

            // A separate path: an empty literal's element is still free at generalisation,
            // so its container is settled there rather than by the whole-file sweep.
            test "`[]` with no Vesper.List reference identifies the missing dependency" {
                expectMissingListDep "let xs = []"
            }

            // The cases are spelled `([])` and `(::)` but match as `Empty` and `Cons`.
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

                let artifact = compileSource "ListLitOwnUnion" src

                expectNoFSharpCore artifact "a `[1;2;3]` over our own list + a concrete printf"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "[1; 2; 3] built + folded over the program's own List<'T>"
            }
        ]
