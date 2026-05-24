module XParsec.FSharp.SemanticAnalysis.Tests.ResolvedTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private hasResolvedTypesDiag (tast: TastFile) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "ResolvedTypes")

[<Tests>]
let tests =
    testList
        "ResolvedTypes"
        [
            test "monomorphic top-level binding has no ResolvedTypes diagnostic" {
                let tast = analyse "let x = 1"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic"
            }

            test "polymorphic let generalisation is allowed (free TyVars are in the scheme)" {
                // `id`'s TyVar stays free after generalisation, but it's
                // captured in the scheme's Quantified set — the validator
                // must accept it.
                let tast = analyse "let id = fun x -> x"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic on quantified TyVar"
            }

            test "polymorphic id used at two types is clean" {
                let tast = analyse "let r = let id = fun x -> x in id 1, id true"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic"
            }

            test "list literal generalises its element type" {
                // `let xs = []` types as `'a list` — the element TyVar
                // belongs to `xs`'s scheme.
                let tast = analyse "let xs = []"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic"
            }

            test "try-with arm pattern carries a resolved type (regression: free exn TyVar)" {
                // Previously the arm-pattern scrutinee was a fresh TyVar
                // that no use site pinned — wildcard arms shipped a free
                // TyVar into the TAST. Pinning the scrutinee to `TyConst
                // "exn"` resolves it.
                let tast = analyse "let r = try 1 with | _ -> 2"
                Expect.isFalse (hasResolvedTypesDiag tast) "no ResolvedTypes diagnostic"
            }

            test "synthetic TAST with a free TyVar surfaces a diagnostic" {
                // The validator runs against any TastFile, not just one
                // produced by Freeze. Build a minimal pathological TAST by
                // hand: a TDecl.Let whose pattern carries a fresh,
                // unlinked TypeVar with no scheme registered for it.
                let lexed, file = parseFile "let x = 1"

                let ctx, _ =
                    Pipeline.analyseWithContext MockBuiltins.provider "let x = 1" lexed file

                let freeTv = TypeVar()
                let freeTy = TyVar freeTv

                let synthDecls =
                    [
                        TDecl.Let(
                            TPat.NamedSimple(NodeKey(0UL), freeTy),
                            TExpr.Const(TConstValue.Unit, freeTy),
                            false,
                            freeTy
                        )
                    ]

                let synth =
                    {
                        Decls = synthDecls
                        Diagnostics = []
                        IntrinsicReprTypes = Map.empty
                        ModuleMembers = Map.empty
                    }

                let before = ctx.Diagnostics.Count
                ResolvedTypes.run ctx synth

                let added =
                    ctx.Diagnostics
                    |> Seq.skip before
                    |> Seq.filter (fun d -> d.Message.Contains "ResolvedTypes")
                    |> Seq.length

                Expect.isGreaterThan added 0 "synthetic free TyVar fires a diagnostic"
            }

            test "synthetic TAST with a TyVar covered by an outer scheme is silent" {
                // If a synthetic decl's TyVar matches a quantified TyVar
                // from the enclosing decl's scheme, the validator must
                // skip it. Build the scenario via analysing `let id = fun
                // x -> x` and reusing one of id's quantified TyVars.
                let lexed, file = parseFile "let id = fun x -> x"

                let ctx, tast =
                    Pipeline.analyseWithContext MockBuiltins.provider "let id = fun x -> x" lexed file

                let idKey =
                    match tast.Decls with
                    | [ TDecl.Let(TPat.NamedSimple(k, _), _, _, _) ] -> k
                    | other -> failwithf "expected single NamedSimple decl, got %A" other

                let scheme = ctx.Scheme.TryGetValue idKey

                let quantTv =
                    match scheme with
                    | ValueSome s when not (List.isEmpty s.Quantified) -> s.Quantified.[0]
                    | _ -> failwith "expected at least one quantified TyVar on id's scheme"

                let ty = TyVar quantTv

                let synth =
                    {
                        Decls =
                            [
                                TDecl.Let(TPat.NamedSimple(idKey, ty), TExpr.Const(TConstValue.Unit, ty), false, ty)
                            ]
                        Diagnostics = []
                        IntrinsicReprTypes = Map.empty
                        ModuleMembers = Map.empty
                    }

                let before = ctx.Diagnostics.Count
                ResolvedTypes.run ctx synth

                let added =
                    ctx.Diagnostics
                    |> Seq.skip before
                    |> Seq.filter (fun d -> d.Message.Contains "ResolvedTypes")
                    |> Seq.length

                Expect.equal added 0 "TyVar bound by the matching scheme is allowed"
            }
        ]
