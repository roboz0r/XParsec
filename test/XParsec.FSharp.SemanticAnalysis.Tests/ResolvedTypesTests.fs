module XParsec.FSharp.SemanticAnalysis.Tests.ResolvedTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

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
                // produced by Elaborate. Build a minimal pathological TAST by
                // hand: a TDecl.Let whose pattern carries a fresh,
                // unlinked TypeVar with no scheme registered for it.
                let lexed, file = parseFile "let x = 1"

                let ctx, _ =
                    Pipeline.analyseSemWithContext realProvider.Value "let x = 1" lexed file

                let freeTv = TypeVar()
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
                        Diagnostics = []
                        IntrinsicReprTypes = Map.empty
                        ModuleMembers = Map.empty
                        TopLevelNames = Map.empty
                        ClosureReprs = Map.empty
                        FunVerdicts = Map.empty
                        GenericFnSchemes = Map.empty
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
                    Pipeline.analyseSemWithContext realProvider.Value "let id = fun x -> x" lexed file

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
                        Diagnostics = []
                        IntrinsicReprTypes = Map.empty
                        ModuleMembers = Map.empty
                        TopLevelNames = Map.empty
                        ClosureReprs = Map.empty
                        FunVerdicts = Map.empty
                        GenericFnSchemes = Map.empty
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

            // With the hardcoded `"int" -> BuiltinTypes.tyInt` arms deleted from
            // `translateType`, a primitive type annotation must still pin to
            // `TyConst("int", EqArray.empty)` — now resolved through the real
            // `prim-types-*` contract (`ExternalTypeShape.Intrinsic` → `TyConst
            // name`) rather than a hardcoded arm. Resolves against `realProvider`
            // (the same contract the literal RHS `1`/`true` resolves its intrinsic
            // through), so the annotation and the literal agree on `Vesper.int`.
            test "primitive annotations pin to TyConst through the real contract" {
                let bindingTy (src: string) : SemType =
                    let lexed, file = parseFile src
                    let tast = Pipeline.analyseSem realProvider.Value src lexed file

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
