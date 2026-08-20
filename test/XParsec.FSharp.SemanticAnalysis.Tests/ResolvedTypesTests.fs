module XParsec.FSharp.SemanticAnalysis.Tests.ResolvedTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (LexedFile.ofText lexed) file

let private isUnresolvedTyVars (d: Diagnostic) =
    match d.Kind with
    | Kind.Internal(InternalBreak.UnresolvedTyVars _) -> true
    | _ -> false

let private hasResolvedTypesDiag (tast: TastFile) =
    tast.Diagnostics |> Seq.exists isUnresolvedTyVars

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
                // `id`'s TyVar stays free but is captured in the scheme's Quantified set,
                // so the validator must accept it.
                let tast = analyse "let id = fun x -> x"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic on quantified TyVar"
            }

            test "polymorphic id used at two types is clean" {
                let tast = analyse "let r = let id = fun x -> x in id 1, id true"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic"
            }

            test "list literal generalises its element type" {
                // `xs` types as `'a list` — the element TyVar belongs to its scheme.
                let tast = analyse "let xs = []"
                Expect.isFalse (hasResolvedTypesDiag tast) "no diagnostic"
            }

            test "try-with arm pattern carries a resolved type (regression: free exn TyVar)" {
                // A wildcard arm has no use site to pin its scrutinee, so the scrutinee is
                // pinned to `TyConst "exn"` rather than left a fresh TyVar.
                let tast = analyse "let r = try 1 with | _ -> 2"
                Expect.isFalse (hasResolvedTypesDiag tast) "no ResolvedTypes diagnostic"
            }

            test "synthetic TAST with a free TyVar surfaces a diagnostic" {
                // The validator runs against any TastFile, not only one Elaborate produced:
                // a hand-built decl carrying an unlinked TypeVar with no scheme must fire.
                let lexed, file = parseFile "let x = 1"

                let ctx, _ =
                    Pipeline.analyseSemWithContext realProvider.Value (LexedFile.ofText lexed) file

                let freeTv = ctx.Store.NewTypeVar()
                let freeTy = TyVar freeTv

                let synthDecls =
                    [
                        TDecl.Let(
                            TPat.NamedSimple(NodeKey(0UL), freeTy, dummyTok),
                            TExpr.Const(TConstValue.Unit, freeTy, dummyTok),
                            false,
                            freeTy
                        )
                    ]

                let synth =
                    {
                        Decls = EqArray.ofList synthDecls
                        InlineBodies = EqArray.empty
                        Specializations = EqArray.empty
                        Diagnostics = []
                        IntrinsicReprKeys = System.Collections.Generic.Dictionary()
                        GlobalValueKeys = System.Collections.Generic.HashSet()
                        ModuleMembers = Map.empty
                        ClosureReprs = Map.empty
                        FunVerdicts = Map.empty
                        GenericFnSchemes = Map.empty
                        Accessibility = System.Collections.Generic.Dictionary()
                        BindingTyparArities = Map.empty
                    }

                let before = ctx.Diagnostics.Count
                ResolvedTypes.run ctx synth

                let added =
                    ctx.Diagnostics
                    |> Seq.skip before
                    |> Seq.filter isUnresolvedTyVars
                    |> Seq.length

                Expect.isGreaterThan added 0 "synthetic free TyVar fires a diagnostic"
            }

            test "synthetic TAST with a TyVar covered by an outer scheme is silent" {
                // A TyVar matching one quantified by the enclosing decl's scheme must be
                // skipped, so the synthetic decl reuses one of `id`'s quantified TyVars.
                let lexed, file = parseFile "let id = fun x -> x"

                let ctx, tast =
                    Pipeline.analyseSemWithContext realProvider.Value (LexedFile.ofText lexed) file

                let idKey =
                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) ] -> k
                    | _ -> failwithf "expected single NamedSimple decl, got %A" tast.Decls

                let scheme = ctx.Bindings.Scheme.TryGetValue idKey

                let quantTv =
                    match scheme with
                    | ValueSome s when not (List.isEmpty s.Quantified) -> s.Quantified.[0]
                    | _ -> failwith "expected at least one quantified TyVar on id's scheme"

                let ty = TyVar quantTv

                let synth =
                    {
                        Decls =
                            EqArray.ofList
                                [
                                    TDecl.Let(
                                        TPat.NamedSimple(idKey, ty, dummyTok),
                                        TExpr.Const(TConstValue.Unit, ty, dummyTok),
                                        false,
                                        ty
                                    )
                                ]
                        InlineBodies = EqArray.empty
                        Specializations = EqArray.empty
                        Diagnostics = []
                        IntrinsicReprKeys = System.Collections.Generic.Dictionary()
                        GlobalValueKeys = System.Collections.Generic.HashSet()
                        ModuleMembers = Map.empty
                        ClosureReprs = Map.empty
                        FunVerdicts = Map.empty
                        GenericFnSchemes = Map.empty
                        Accessibility = System.Collections.Generic.Dictionary()
                        BindingTyparArities = Map.empty
                    }

                let before = ctx.Diagnostics.Count
                ResolvedTypes.run ctx synth

                let added =
                    ctx.Diagnostics
                    |> Seq.skip before
                    |> Seq.filter isUnresolvedTyVars
                    |> Seq.length

                Expect.equal added 0 "TyVar bound by the matching scheme is allowed"
            }

            // A primitive annotation pins through the real `prim-types-*` contract
            // (`ExternalTypeShape.Intrinsic` → `TyConst name`), not a hardcoded arm — the
            // same contract the literal RHS uses, so both agree on `Vesper.int`.
            test "primitive annotations pin to TyConst through the real contract" {
                let bindingTy (src: string) : SemType =
                    let lexed, file = parseFile src

                    let tast = Pipeline.analyseSem realProvider.Value (LexedFile.ofText lexed) file

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(_, ty, _), _, _, _) ] -> ty
                    | other -> failwithf "expected a single annotated let, got %A" other

                Expect.equal
                    (bindingTy "let x : int = 1")
                    (TyConst(RuntimeNames.intKey, EqArray.empty))
                    "int annotation pins to TyConst \"int\""

                Expect.equal
                    (bindingTy "let b : bool = true")
                    (TyConst(RuntimeNames.boolKey, EqArray.empty))
                    "bool annotation pins to TyConst \"bool\""
            }
        ]
