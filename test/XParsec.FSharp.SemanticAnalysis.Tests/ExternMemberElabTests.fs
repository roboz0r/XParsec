module XParsec.FSharp.SemanticAnalysis.Tests.ExternMemberElabTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// An inline intrinsic-abbrev augmentation `type X = (# "…" #) with member _.M p =
// (# … #)` elaborates to a liftable `TDecl.Type(Class)` of `this`-first member bodies,
// while `X` keeps its `TyConst` identity at every other use site.

let private analyse (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

let private widgetSource =
    "module Widgets\n\
     \n\
     type widget =\n\
     \x20   (# \"object\" #)\n\
     \x20   with\n\
     \x20       member inline _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
     \x20   end\n\
     \n\
     let idW (w: widget) : widget = w\n"

[<Tests>]
let tests =
    testList
        "ExternMemberElab"
        [
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
                            | TyConst(key, _) when SymbolKeyOps.typeSimpleName key = DisplayName "widget" -> ()
                            | other -> failtestf "member ThisTy is not `TyConst widget`: %A" other

                            match m.Body with
                            | TExpr.ILIntrinsic _ -> ()
                            | other -> failtestf "member Body is not an ILIntrinsic: %A" other
                    | other -> failtestf "expected TTypeKind.Class, got %A" other
            }

            // `widget` is declared in `module Widgets`, so its key is `Widgets.widget`; a
            // self-type minted as `Vesper.widget` would split the identity in two. Full-key
            // equality, not `simpleName`, is what catches that.
            test "a `widget`-typed reference resolves to the member self-type key, not a Vesper twin" {
                let tast = analyse widgetSource

                let idWTy =
                    EqArray.toList tast.Decls
                    |> List.tryPick (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, ty) -> Some ty
                        | _ -> None
                    )

                let selfKey =
                    EqArray.toList tast.Decls
                    |> List.tryPick (fun d ->
                        match d with
                        | TDecl.Type t when t.Name = "widget" ->
                            match t.Kind with
                            | TTypeKind.Class clsG ->
                                EqArray.toList clsG.Members
                                |> List.tryPick (fun (m: TTypeMember) ->
                                    match m.ThisTy with
                                    | TyConst(k, _) -> Some k
                                    | _ -> None
                                )
                            | _ -> None
                        | _ -> None
                    )

                match selfKey with
                | None -> failtestf "no widget member self-type key found, decls: %A" tast.Decls
                | Some sk ->
                    Expect.notEqual
                        (SymbolKeyOps.typeMetaName sk)
                        "Vesper.widget"
                        "self-type key is not the Vesper twin"

                    match idWTy with
                    | Some(TyFun(TyConst(k1, _), TyConst(k2, _))) ->
                        Expect.equal k1 sk "idW param key == member self-type key (no split-brain)"
                        Expect.equal k2 sk "idW return key == member self-type key (no split-brain)"
                    | Some other -> failtestf "idW type is not `widget -> widget` over TyConst: %A" other
                    | None -> failtestf "no `idW` let decl found, decls: %A" tast.Decls
            }

            // `widget` is not a numeric primitive, so operator-name synthesis declines it:
            // `a + b` can only resolve through the operator declared on the type.
            test "an operator declared on an intrinsic satisfies the SRTP bound" {
                let source =
                    "module Widgets\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       static member inline (+) (x: widget, y: widget) : widget = (# \"$0 + $1\" x y : widget #)\n\
                     \x20   end\n\
                     \n\
                     let addW (a: widget) (b: widget) : widget = a + b\n"

                let tast = analyse source
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let addWTy =
                    EqArray.toList tast.Decls
                    |> List.tryPick (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, ty) -> Some ty
                        | _ -> None
                    )

                match addWTy with
                | Some(TyFun(TyConst _, TyFun(TyConst _, TyConst(k, _)))) ->
                    Expect.equal (SymbolKeyOps.typeSimpleName k) (DisplayName "widget") "addW returns widget"
                | Some other -> failtestf "addW is not `widget -> widget -> widget`: %A" other
                | None -> failtestf "no `addW` let decl found, decls: %A" tast.Decls
            }

            test "the trait-dispatched call site keys the member body the file collects" {
                // The unifier satisfies the SRTP bound off `IntrinsicAbbrevHost`; the inline
                // trait dispatcher must mint the SAME key the member body is collected under,
                // or the backend has no body to splice at the site.
                let source =
                    "module Widgets\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       static member inline (+) (x: widget, y: widget) : widget = (# \"$0 + $1\" x y : widget #)\n\
                     \x20   end\n\
                     \n\
                     let addW (a: widget) (b: widget) : widget = a + b\n"

                let lexed, file = parseFile source
                let origin = LexedFile.ofText lexed

                let ctx, tast =
                    Pipeline.analyseSemWithContextFor testCompiling realProvider.Value origin file

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let bodies = InlineBodies.collect origin (Freeze.run ctx tast)

                let callKeys =
                    [
                        for s in tast.Specializations do
                            yield!
                                TastWalk.chooseExpr
                                    (function
                                    | TExpr.StaticMethodCall(k, _, _, _, _) -> ValueSome k
                                    | _ -> ValueNone)
                                    s.Value
                    ]

                Expect.isNonEmpty callKeys "the trait call dispatched to a StaticMethodCall"

                for k in callKeys do
                    Expect.isTrue
                        (bodies.Members |> List.exists (fun mb -> mb.Key = k))
                        (sprintf "no collected member body under the call-site key %A" k)
            }

            test "a member on an intrinsic host must be declared inline" {
                let bad =
                    "module Widgets\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       member _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
                     \x20   end\n"

                let tast = analyse bad

                let diagnosed =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "must be declared 'inline'")

                Expect.isTrue diagnosed (sprintf "the member-inline diagnostic fired; got %A" tast.Diagnostics)
            }

            test "an override on an intrinsic host is rejected outright, not asked for inline" {
                let bad =
                    "module Widgets\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       override _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
                     \x20   end\n"

                let tast = analyse bad
                let messages = [ for d in tast.Diagnostics -> d.Message ]

                Expect.isTrue
                    (messages
                     |> List.exists (fun m -> m.Contains "cannot declare an 'override' or 'default' member"))
                    (sprintf "the override diagnostic fired; got %A" messages)

                Expect.isFalse
                    (messages |> List.exists (fun m -> m.Contains "must be declared 'inline'"))
                    "an override is not asked to be inline — the parser gives it no inline token"
            }

            test "a secondary constructor on an intrinsic host is rejected" {
                let bad =
                    "module Widgets\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       new (x: int) = x\n\
                     \x20   end\n"

                let tast = analyse bad

                let diagnosed =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "cannot declare a constructor with a body")

                Expect.isTrue diagnosed (sprintf "the constructor diagnostic fired; got %A" tast.Diagnostics)
            }

            test "an `interface … with` block on an intrinsic host is rejected" {
                let bad =
                    "module Widgets\n\
                     \n\
                     type IPoke =\n\
                     \x20   abstract member Poke: int -> int\n\
                     \n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       interface IPoke with\n\
                     \x20           member _.Poke (x: int) : int = x\n\
                     \x20   end\n"

                let tast = analyse bad

                let diagnosed =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "cannot declare an 'interface")

                Expect.isTrue diagnosed (sprintf "the interface-impl diagnostic fired; got %A" tast.Diagnostics)
            }

            // `type bad = int` is a transparent alias: an ILIntrinsic RHS is what admits members.
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
