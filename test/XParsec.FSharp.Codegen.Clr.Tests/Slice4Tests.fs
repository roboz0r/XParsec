module XParsec.FSharp.Codegen.Clr.Tests.Slice4Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Thin-slice #4:
//
//   printfn "%A" [1; 2; 3]
//
// The new thing is list *construction*: building an `FSharpList<int>` at
// runtime from the frozen `UnionCons` cons/nil chain. Each `UnionCons` lowers
// to a static `call` — `FSharpList\`1<int>::Cons(!0, FSharpList\`1<!0>)` for a
// cell, `get_Empty()` for the terminator — so `[1; 2; 3]` emits as
// `ldc 1; ldc 2; ldc 3; call get_Empty; call Cons; call Cons; call Cons`
// (post-order of the chain). The `printfn "%A"` wrapper is the slice-2 path
// unchanged; `%A` only adds a list-typed generic argument, covered by the one
// new `encodeType` case. Work is one provider hook (`TryEmitUnionCons`), one
// `encodeType` case + two recipe builders in `ClrProvider`, and one
// `TExpr.UnionCons` arm in `Emit`.

[<Tests>]
let tests =
    testList
        "Slice4"
        [
            // ---- The sample analyses clean to the cons/nil chain ----

            test "`printfn \"%A\" [1; 2; 3]` analyses clean as a Cons/Nil chain over list<int>" {
                let tast = analyse "printfn \"%A\" [1; 2; 3]"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let listTy = TyRecord("Microsoft.FSharp.Collections.list", [ TyConst "int" ])

                match tast.Decls with
                | [ TDecl.Expression(TExpr.App(TExpr.App(TExpr.External("printfn", _), TExpr.New _, _),
                                               TExpr.UnionCons("Cons",
                                                               [ TExpr.Const(TConstValue.Int 1, _)
                                                                 TExpr.UnionCons("Cons",
                                                                                 [ TExpr.Const(TConstValue.Int 2, _)
                                                                                   TExpr.UnionCons("Cons",
                                                                                                   [ TExpr.Const(TConstValue.Int 3,
                                                                                                                 _)
                                                                                                     TExpr.UnionCons("Nil",
                                                                                                                     [],
                                                                                                                     _) ],
                                                                                                   _) ],
                                                                                 _) ],
                                                               outerTy),
                                               _),
                                     _) ] ->
                    Expect.equal outerTy listTy "the trailing arg is the cons chain typed list<int>"
                | other -> failtestf "unexpected slice-4 TAST: %A" other
            }

            // ---- Milestone 1: single-element list, end-to-end ----

            test "`printfn \"%A\" [1]` prints [1] (one Cons over Nil + list-typed Invoke)" {
                let _, artifact = compileSource "Slice4Single" "printfn \"%A\" [1]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1]" "single-element list constructed and printed"
            }

            // ---- Milestone 2: multi-element chain, end-to-end ----

            test "`printfn \"%A\" [1; 2; 3]` prints [1; 2; 3] (recursive tail + post-order calls)" {
                let _, artifact = compileSource "Slice4Chain" "printfn \"%A\" [1; 2; 3]"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "the full cons chain constructed and printed"
            }

            // ---- Milestone 3: list-typed local, end-to-end ----

            test "`let nums = [1; 2; 3]` / `printfn \"%A\" nums` prints [1; 2; 3] (list-typed local)" {
                let _, artifact =
                    compileSource "Slice4Local" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "[1; 2; 3]" "list stored to a local, reloaded, and printed"
            }
        ]
