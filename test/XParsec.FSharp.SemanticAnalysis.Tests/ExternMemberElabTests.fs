module XParsec.FSharp.SemanticAnalysis.Tests.ExternMemberElabTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// W9 Stage 1b-elab — the front end now ELABORATES an inline intrinsic-abbrev
// augmentation `type X = (# "repr" #) with member _.M p = (# … #)` into a
// harvestable `TDecl.Type(Class)` carrying `this`-first member bodies, while KEEPING
// the abbrev's `TyConst` identity (it stays intrinsic at every other use site).

let private analyse (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private widgetSource =
    "module Widgets\n\
     \n\
     type widget =\n\
     \x20   (# \"object\" #)\n\
     \x20   with\n\
     \x20       member _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
     \x20   end\n\
     \n\
     let idW (w: widget) : widget = w\n"

[<Tests>]
let tests =
    testList
        "ExternMemberElab"
        [
            // 2a: the impl elaborates to a `TDecl.Type(Class)` whose `Poke` member is
            // an instance `this`-first member with an `ILIntrinsic` body and one param.
            test "inline intrinsic-abbrev with member elaborates to a Class TDecl.Type" {
                let tast = analyse widgetSource
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let tdecl =
                    EqArray.toList tast.Decls
                    |> List.tryPick (fun d ->
                        match d with
                        | TDecl.Type t -> Some t
                        | _ -> None
                    )

                match tdecl with
                | None -> failtestf "expected a TDecl.Type for widget, decls: %A" tast.Decls
                | Some t ->
                    Expect.equal t.Name "widget" "decl name"

                    match t.Kind with
                    | TTypeKind.Class clsG ->
                        let poke =
                            EqArray.toList clsG.Members
                            |> List.tryFind (fun (m: TTypeMember) -> m.Name = "Poke")

                        match poke with
                        | None -> failtestf "no Poke member, members: %A" (EqArray.toList clsG.Members)
                        | Some m ->
                            Expect.isFalse m.IsStatic "instance member"
                            Expect.isTrue m.ThisKey.IsSome "instance member carries a ThisKey"
                            Expect.equal m.Params.Length 1 "one value param (x)"

                            // Self-type is the INTRINSIC type, not a TyClass.
                            match m.ThisTy with
                            | TyConst("widget", _) -> ()
                            | other -> failtestf "member ThisTy is not `TyConst widget`: %A" other

                            match m.Body with
                            | TExpr.ILIntrinsic _ -> ()
                            | other -> failtestf "member Body is not an ILIntrinsic: %A" other
                    | other -> failtestf "expected TTypeKind.Class, got %A" other
            }

            // Identity: a reference to `widget` as a type elsewhere resolves to
            // `TyConst "widget"` (still intrinsic) — NOT `TyClass`.
            test "a `widget`-typed reference resolves to `TyConst widget`, not TyClass" {
                let tast = analyse widgetSource

                let idWTy =
                    EqArray.toList tast.Decls
                    |> List.tryPick (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, ty) -> Some ty
                        | _ -> None
                    )

                match idWTy with
                | Some(TyFun(TyConst("widget", _), TyConst("widget", _))) -> ()
                | Some other -> failtestf "idW type is not `widget -> widget` over TyConst: %A" other
                | None -> failtestf "no `idW` let decl found, decls: %A" tast.Decls
            }

            // Guardrail: a transparent-alias abbrev with members (non-ILIntrinsic RHS)
            // is rejected with a diagnostic.
            test "a transparent-alias abbrev with members is rejected" {
                let bad =
                    "module Bad\n\
                     \n\
                     type bad =\n\
                     \x20   int\n\
                     \x20   with\n\
                     \x20       member _.M (x: int) : int = (# \"$0\" x : int #)\n\
                     \x20   end\n"

                let tast = analyse bad

                Expect.isNonEmpty tast.Diagnostics "a diagnostic was raised"

                let mentionsAbbrev =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "cannot carry augmentation members")

                Expect.isTrue mentionsAbbrev "the augmentation-member guardrail diagnostic fired"
            }
        ]
