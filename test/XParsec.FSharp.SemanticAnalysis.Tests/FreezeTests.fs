module XParsec.FSharp.SemanticAnalysis.Tests.FreezeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | [ TDecl.Let(_, _, _, ty) ] -> ty
    | other -> failwithf "expected single TDecl.Let, got %A" other

[<Tests>]
let tests =
    testList
        "Freeze"
        [
            test "`let x = 1` -> single TDecl.Let with TConst Int 1" {
                let tast = analyse "let x = 1"
                Expect.equal tast.Decls.Length 1 "one decl"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Int 1, ty), _, letTy) ->
                    Expect.equal ty MockBuiltins.tyInt "value type"
                    Expect.equal letTy MockBuiltins.tyInt "binding type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let b = true` -> TConst Bool true" {
                let tast = analyse "let b = true"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Bool true, ty), _, _) ->
                    Expect.equal ty MockBuiltins.tyBool "value type bool"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let f = fun x -> x + 1` -> Lambda over App chain with External operator" {
                let tast = analyse "let f = fun x -> x + 1"
                let intTy = MockBuiltins.tyInt
                let intToInt = TyFun(intTy, intTy)

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> (v1 + 1)" "TAST shape"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Lambda(_, _, lamTy), _, declTy) ->
                    Expect.equal lamTy intToInt "lambda type int -> int"
                    Expect.equal declTy intToInt "decl type int -> int"
                | other -> failtestf "unexpected decl: %A" other
            }

            test "function-form `let f x = x + 1` produces same shape as fun-form" {
                let tast = analyse "let f x = x + 1"
                let intToInt = TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = fun v1 -> (v1 + 1)"
                    "TAST shape matches fun-form"

                match tast.Decls.[0] with
                | TDecl.Let(_, _, _, declTy) -> Expect.equal declTy intToInt "decl type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let result = let id = fun x -> x in id 42` -> nested Let with App" {
                let tast = analyse "let result = let id = fun x -> x in id 42"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = let v1 = fun v2 -> v2 in (v1 42)"
                    "TAST shape"

                Expect.equal (declType tast) MockBuiltins.tyInt "result : int"
            }

            test "TDecl.Let binding NodeKey matches headPat NodeKey" {
                let tast = analyse "let x = 1"
                let expected = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls.[0] with
                | TDecl.Let(TPat.NamedSimple(bindingKey, _), _, _, _) -> Expect.equal bindingKey expected "binding key"
                | other -> failtestf "unexpected: %A" other
            }

            test "TVar references the original headPat NodeKey" {
                // "let x = 1\nlet y = x" — y's RHS references x's binding key.
                let tast = analyse "let x = 1\nlet y = x"
                let xKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls with
                | [ _; TDecl.Let(_, TExpr.Var(refKey, _), _, _) ] -> Expect.equal refKey xKey "y refs x"
                | _ -> failtestf "unexpected decls: %A" tast.Decls
            }

            test "diagnostics propagate from earlier passes" {
                let tast = analyse "let x = undefined"

                Expect.isGreaterThanOrEqual tast.Diagnostics.Length 1 "unresolved diagnostic reaches the TAST"
            }

            test "`let xs = [1; 2; 3]` freezes as nested Cons / Nil over `list<int>`" {
                let tast = analyse "let xs = [1; 2; 3]"
                let intTy = MockBuiltins.tyInt
                let listTy = TyRecord("Microsoft.FSharp.Collections.list", [ intTy ])

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) listTy "xs : list<int>"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.UnionCons("Cons",
                                            [ TExpr.Const(TConstValue.Int 1, _)
                                              TExpr.UnionCons("Cons",
                                                              [ TExpr.Const(TConstValue.Int 2, _)
                                                                TExpr.UnionCons("Cons",
                                                                                [ TExpr.Const(TConstValue.Int 3, _)
                                                                                  TExpr.UnionCons("Nil", [], _) ],
                                                                                _) ],
                                                              _) ],
                                            outerTy),
                            _,
                            _) -> Expect.equal outerTy listTy "outer UnionCons ty"
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = []` freezes as empty Nil with a free element type" {
                let tast = analyse "let xs = []"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.UnionCons("Nil", [], ty), _, _) ->
                    match ty with
                    | TyRecord("Microsoft.FSharp.Collections.list", [ _ ]) -> ()
                    | _ -> failtestf "expected list<_> Nil, got %A" ty
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = [|1; 2|]` wraps the Cons chain in Array.ofList" {
                let tast = analyse "let xs = [|1; 2|]"
                let intTy = MockBuiltins.tyInt
                let listTy = TyRecord("Microsoft.FSharp.Collections.list", [ intTy ])
                let arrayTy = TyRecord("Microsoft.FSharp.Core.[]", [ intTy ])

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) arrayTy "xs : int[]"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.App(TExpr.External("Microsoft.FSharp.Collections.ArrayModule.OfList", opTy),
                                      TExpr.UnionCons("Cons", _, innerTy),
                                      outerTy),
                            _,
                            _) ->
                    Expect.equal opTy (TyFun(listTy, arrayTy)) "Array.ofList: list -> array"
                    Expect.equal innerTy listTy "inner list type"
                    Expect.equal outerTy arrayTy "outer array type"
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "all elements unify to a single element type" {
                // Mixing int and bool in a list literal must surface a
                // unification diagnostic via the shared element TyVar.
                let tast = analyse "let xs = [1; true]"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "type-mismatch diagnostic emitted"
            }

        ]

// G1 (docs/selfhost-handoff.md): every pass — not just Freeze — now walks
// `namespace`-headed files, so declarations under a `namespace` are fully
// analysed (name-resolved, inferred, frozen) exactly like a module file.
// Before G1, Desugar / NameResolution / Unification / Regions / Validation
// dropped namespace files, so a `let` under a `namespace` silently froze to
// an untyped / unresolved TAST.
[<Tests>]
let namespaceTests =
    testList
        "NamespacePipeline"
        [
            test "`namespace Foo` + `let x = 1` freezes to a typed TDecl.Let" {
                let tast = analyse "namespace Foo\n\nlet x = 1"

                Expect.equal tast.Decls.Length 1 "one decl"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Int 1, ty), _, letTy) ->
                    Expect.equal ty MockBuiltins.tyInt "value type int"
                    Expect.equal letTy MockBuiltins.tyInt "binding type int"
                | other -> failtestf "unexpected: %A" other
            }

            test "namespace `let` is analysed identically to the module-level form" {
                let nsForm = analyse "namespace Foo\n\nlet f x = x + 1"
                let modForm = analyse "let f x = x + 1"

                Expect.isEmpty nsForm.Diagnostics "no diagnostics (namespace form)"
                Expect.equal nsForm.Decls.Length modForm.Decls.Length "same decl count"

                Expect.equal
                    (TastShape.prettyDecl nsForm.Decls.[0])
                    (TastShape.prettyDecl modForm.Decls.[0])
                    "namespace and module forms freeze to the same TAST"
            }

            test "name resolution + inference run across namespace elements" {
                // `y`'s body references the earlier `x`; it only resolves and
                // types if NameResolution and Unification actually walked the
                // namespace (both were no-ops here before G1). Asserting the
                // namespace form matches the module form locks that in without
                // hard-coding the freshly-named TAST rendering.
                let nsForm = analyse "namespace Foo\n\nlet x = 1\nlet y = x + 1"
                let modForm = analyse "let x = 1\nlet y = x + 1"

                Expect.equal nsForm.Decls.Length 2 "two decls"
                Expect.isEmpty nsForm.Diagnostics "x resolves inside y — no unresolved-ident diagnostic"

                Expect.equal
                    (nsForm.Decls |> List.map TastShape.prettyDecl)
                    (modForm.Decls |> List.map TastShape.prettyDecl)
                    "cross-referencing namespace bindings freeze identically to the module form"

                match nsForm.Decls.[1] with
                | TDecl.Let(_, _, _, declTy) -> Expect.equal declTy MockBuiltins.tyInt "y : int"
                | other -> failtestf "unexpected: %A" other
            }
        ]

// G3 (docs/selfhost-handoff.md): an interface-shaped `TypeDefn.Anon` surfaces as
// `TDecl.Type` whose method signatures are read from the *resolved* member types
// in `ctx.ClassTypes` (NameResolution registers the abstract member; Unification
// fills its signature), with the declaring typars remapped to the `TyConst "'A"`
// markers the backend consumes. Freeze no longer re-translates the CST signature.
[<Tests>]
let interfaceTests =
    testList
        "InterfaceFreeze"
        [
            test "`Fun` interface freezes to TDecl.Type with the resolved Invoke signature" {
                let tast =
                    analyse "namespace Vesper\n\ntype Fun<'A, 'B> =\n    abstract member Invoke: arg: 'A -> 'B"

                // Registering the abstract member (rather than rejecting it) means
                // a clean analysis — the G1-era "member kind not supported" error
                // is gone.
                Expect.isEmpty tast.Diagnostics "no diagnostics for an abstract member"

                match tast.Decls with
                | [ TDecl.Type td ] ->
                    Expect.equal td.Name "Fun" "type name"
                    Expect.equal td.Namespace (Some "Vesper") "namespace"
                    Expect.equal td.TypeParams [ "'A"; "'B" ] "declared typars"

                    match td.Kind with
                    | TTypeKind.Interface [ m ] ->
                        Expect.equal m.Name "Invoke" "method name"
                        Expect.isEmpty m.MethodTypeParams "Invoke has no method typars"
                        // 'A -> 'B, declaring typars as TyConst markers.
                        Expect.equal m.Signature (TyFun(TyConst "'A", TyConst "'B")) "Invoke signature"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }

            // G4 item 5 (docs/selfhost-handoff.md): an abstract method may carry
            // its *own* generic parameters (`abstract Map<'B> : 'A -> 'B`). Its `'B`
            // is no longer diagnosed as a free typar; it surfaces on the method as
            // `MethodTypeParams` and rides the signature as a `TyConst "'B"` marker,
            // distinct from the declaring type's `'A`.
            test "generic abstract method surfaces its own typars distinct from the declaring type's" {
                let tast =
                    analyse "namespace Vesper\n\ntype Mapper<'A> =\n    abstract member Map<'B> : arg: 'A -> 'B"

                Expect.isEmpty tast.Diagnostics "the method typar 'B is declared, not free"

                match tast.Decls with
                | [ TDecl.Type td ] ->
                    Expect.equal td.Name "Mapper" "type name"
                    Expect.equal td.TypeParams [ "'A" ] "declaring typar 'A only"

                    match td.Kind with
                    | TTypeKind.Interface [ m ] ->
                        Expect.equal m.Name "Map" "method name"
                        Expect.equal m.MethodTypeParams [ "'B" ] "method's own typar 'B"
                        // 'A is the declaring typar, 'B the method's own — both markers.
                        Expect.equal m.Signature (TyFun(TyConst "'A", TyConst "'B")) "Map signature 'A -> 'B"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }
        ]
