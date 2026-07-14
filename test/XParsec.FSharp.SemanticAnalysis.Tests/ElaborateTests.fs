module XParsec.FSharp.SemanticAnalysis.Tests.ElaborateTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

[<Tests>]
let tests =
    testList
        "Elaborate"
        [
            test "`let x = 1` -> single TDecl.Let with TConst Int 1" {
                let tast = analyse "let x = 1"
                Expect.equal tast.Decls.Length 1 "one decl"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), ty, _), _, letTy) ->
                    Expect.equal ty BuiltinTypes.tyInt "value type"
                    Expect.equal letTy BuiltinTypes.tyInt "binding type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let b = true` -> TConst Bool true" {
                let tast = analyse "let b = true"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Bool true, ty, _), _, _) ->
                    Expect.equal ty BuiltinTypes.tyBool "value type bool"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let f = fun x -> x + 1` -> Lambda over App chain with External operator" {
                let tast = analyse "let f = fun x -> x + 1"
                let intTy = BuiltinTypes.tyInt
                let intToInt = TyFun(intTy, intTy)

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> (v1 + 1)" "TAST shape"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Lambda(_, _, lamTy, _), _, declTy) ->
                    Expect.equal lamTy intToInt "lambda type int -> int"
                    Expect.equal declTy intToInt "decl type int -> int"
                | other -> failtestf "unexpected decl: %A" other
            }

            test "function-form `let f x = x + 1` produces same shape as fun-form" {
                let tast = analyse "let f x = x + 1"
                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)

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

                Expect.equal (declType tast) BuiltinTypes.tyInt "result : int"
            }

            test "TDecl.Let binding NodeKey matches headPat NodeKey" {
                let tast = analyse "let x = 1"
                let expected = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls.[0] with
                | TDecl.Let(TPat.NamedSimple(bindingKey, _, _), _, _, _) ->
                    Expect.equal bindingKey expected "binding key"
                | other -> failtestf "unexpected: %A" other
            }

            test "TVar references the original headPat NodeKey" {
                // y's RHS references x's binding key.
                let tast = analyse "let x = 1\nlet y = x"
                let xKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls with
                | EqList [ _; TDecl.Let(_, TExpr.Var(refKey, _, _), _, _) ] -> Expect.equal refKey xKey "y refs x"
                | _ -> failtestf "unexpected decls: %A" tast.Decls
            }

            test "diagnostics propagate from earlier passes" {
                let tast = analyse "let x = undefined"

                Expect.isGreaterThanOrEqual tast.Diagnostics.Length 1 "unresolved diagnostic reaches the TAST"
            }

            test "`let xs = [1; 2; 3]` freezes as nested Cons / Nil over `list<int>`" {
                let tast = analyse "let xs = [1; 2; 3]"
                let intTy = BuiltinTypes.tyInt

                let listTy =
                    SemType.TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton intTy)

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) listTy "xs : list<int>"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.UnionCons("Cons",
                                            EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _)
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
                                                                                                       TExpr.UnionCons("Nil",
                                                                                                                       EqList [],
                                                                                                                       _,
                                                                                                                       _) ],
                                                                                              _,
                                                                                              _) ],
                                                                     _,
                                                                     _) ],
                                            outerTy,
                                            _),
                            _,
                            _) -> Expect.equal outerTy listTy "outer UnionCons ty"
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = []` freezes as empty Nil with a free element type" {
                let tast = analyse "let xs = []"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.UnionCons("Nil", EqList [], ty, _), _, _) ->
                    match ty with
                    | TyRecord("Microsoft.FSharp.Collections.list`1", args) when args.Length = 1 -> ()
                    | _ -> failtestf "expected list<_> Nil, got %A" ty
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = [|1; 2|]` wraps the Cons chain in Array.ofList" {
                let tast = analyse "let xs = [|1; 2|]"
                let intTy = BuiltinTypes.tyInt

                let listTy =
                    SemType.TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton intTy)

                let arrayTy = TyConst(RuntimeNames.arrayKey 1, EqArray.singleton intTy)

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) arrayTy "xs : int[]"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.App(TExpr.External("Microsoft.FSharp.Collections.ArrayModule.OfList", _, opTy, _),
                                      TExpr.UnionCons("Cons", _, innerTy, _),
                                      outerTy,
                                      _),
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

// Every pass — not just Elaborate — walks `namespace`-headed files, so
// declarations under a `namespace` are fully analysed (name-resolved,
// inferred, frozen) exactly like a module file. Earlier, Desugar /
// NameResolution / Unification / Regions / Validation dropped namespace
// files, so a `let` under a `namespace` silently froze to an untyped /
// unresolved TAST.
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
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), ty, _), _, letTy) ->
                    Expect.equal ty BuiltinTypes.tyInt "value type int"
                    Expect.equal letTy BuiltinTypes.tyInt "binding type int"
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
                    (nsForm.Decls |> EqArray.map TastShape.prettyDecl |> EqArray.toList)
                    (modForm.Decls |> EqArray.map TastShape.prettyDecl |> EqArray.toList)
                    "cross-referencing namespace bindings freeze identically to the module form"

                match nsForm.Decls.[1] with
                | TDecl.Let(_, _, _, declTy) -> Expect.equal declTy BuiltinTypes.tyInt "y : int"
                | other -> failtestf "unexpected: %A" other
            }
        ]

// Every pass + Elaborate descend into a nested `module Foo = …`. Its body is
// flattened to the enclosing scope (v1 has no module-scoped types), the same
// simplification `CstWalk.implFileElems` applies to namespace groups. Earlier
// the analysis passes never descended into `ModuleElem.Module` (Validation
// `failwith`'d on it) and Elaborate dropped the body, so a `let` inside a nested
// module silently vanished.
[<Tests>]
let nestedModuleTests =
    testList
        "NestedModulePipeline"
        [
            test "a nested module's `let` surfaces flat alongside top-level decls" {
                let tast = analyse "let top = 0\nmodule Inner =\n    let x = 1"

                Expect.equal tast.Decls.Length 2 "top-level binding + the nested binding"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[1] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), ty, _), _, letTy) ->
                    Expect.equal ty BuiltinTypes.tyInt "value type int"
                    Expect.equal letTy BuiltinTypes.tyInt "binding type int"
                | other -> failtestf "unexpected: %A" other
            }

            test "name resolution + inference run inside the nested module body" {
                // `y`'s body references the outer `top`; it only resolves and
                // types if NameResolution and Unification actually descended into
                // the nested module (both dropped it before this slice). Inferring
                // `y : int` (not a free TyVar) proves Unification walked the body.
                let tast = analyse "let top = 1\nmodule Inner =\n    let y = top + 1"

                Expect.equal tast.Decls.Length 2 "top + the nested binding"
                Expect.isEmpty tast.Diagnostics "top resolves inside the nested module"

                match tast.Decls.[1] with
                | TDecl.Let(_, _, _, declTy) -> Expect.equal declTy BuiltinTypes.tyInt "y : int"
                | other -> failtestf "unexpected: %A" other
            }

            test "arbitrarily deep module nesting flattens" {
                let tast = analyse "let top = 0\nmodule A =\n    module B =\n        let x = 1"

                Expect.equal tast.Decls.Length 2 "the doubly-nested binding flattens to top level"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[1] with
                | TDecl.Let(_, _, _, declTy) -> Expect.equal declTy BuiltinTypes.tyInt "x : int"
                | other -> failtestf "unexpected: %A" other
            }

            // The DECL flattens; the CONTAINMENT does not. A binding's holder is the whole
            // chain of modules it is written in — the same chain a type declared there gets
            // (`SymbolKeyTests`, "a NESTED module produces a nested InModule chain"), because
            // both read `ModuleRules.holderChain`. Dropping the outer module here would give
            // one source location two containments depending on what was declared in it.
            test "a binding in a nested module is held by the WHOLE module chain" {
                let tast =
                    analyse "namespace N\n\nmodule A =\n    module B =\n        let f (x: int) = x + 1"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let info =
                    match tast.ModuleMembers |> Map.toList |> List.map snd with
                    | [ info ] -> info
                    | other -> failtestf "expected exactly one module member, got %A" other

                Expect.equal info.Name "f" "the binding's compiled name"
                Expect.equal info.Holder.Name "B" "held by the INNERMOST module"

                match info.Holder.Holder with
                | ModuleHolder.InModule a ->
                    Expect.equal a.Name "A" "which is itself held by the outer module"

                    Expect.equal
                        (List.ofSeq a.Namespace.Path.Underlying)
                        [ "N" ]
                        "and the outer module by the namespace — neither module is a namespace segment"
                | other -> failtestf "expected B's holder to be module A, got %A" other

                Expect.equal
                    (SymbolKeyOps.qualifiedName info.Key)
                    "N.A.B.f"
                    "the binding's key qualifies through the whole chain"
            }

            test "a nested module binding freezes identically to the module-level form" {
                let nested = analyse "let top = 0\nmodule Inner =\n    let f x = x + 1"
                let flat = analyse "let f x = x + 1"

                Expect.isEmpty nested.Diagnostics "no diagnostics (nested form)"

                Expect.equal
                    (TastShape.prettyDecl nested.Decls.[1])
                    (TastShape.prettyDecl flat.Decls.[0])
                    "the nested `f` freezes to the same TAST as the top-level `f`"
            }
        ]

// An interface-shaped `TypeDefn.Anon` surfaces as
// `TDecl.Type` whose method signatures are read from the *resolved* member types
// in `ctx.Types.Class` (NameResolution registers the abstract member; Unification
// fills its signature), with the declaring typars remapped to the `TyConst("'A", EqArray.empty)`
// markers the backend consumes. Elaborate no longer re-translates the CST signature.
[<Tests>]
let interfaceTests =
    testList
        "InterfaceFreeze"
        [
            test "`Fun` interface freezes to TDecl.Type with the resolved Invoke signature" {
                // `Vesper.Fun` is Vesper.Core's OWN type, and the provider mounts Vesper.Core's
                // contract — so this unit must be compiled AS Vesper.Core. A unit is allowed to
                // declare the types its own contract publishes (that is what compiling it means);
                // any other assembly name here is the CS0433 analogue
                // (`claimTypeIdentity`'s external-claim diagnostic).
                let src =
                    "namespace Vesper\n\ntype Fun<'A, 'B> =\n    abstract member Invoke: arg: 'A -> 'B"

                let lexed, file = parseFile src
                let tast = Pipeline.analyseSemFor "Vesper.Core" realProvider.Value src lexed file

                // Registering the abstract member (rather than rejecting it) means
                // a clean analysis — the G1-era "member kind not supported" error
                // is gone.
                Expect.isEmpty tast.Diagnostics "no diagnostics for an abstract member"

                match tast.Decls with
                | EqList [ TDecl.Type td ] ->
                    Expect.equal td.Name "Fun" "type name"
                    Expect.equal td.Namespace (Some "Vesper") "namespace"
                    Expect.equal (EqArray.toList td.TypeParams) [ "'A"; "'B" ] "declared typars"

                    match td.Kind with
                    | TTypeKind.Interface(EqList [ m ]) ->
                        Expect.equal m.Name "Invoke" "method name"
                        Expect.isTrue m.MethodTypeParams.IsEmpty "Invoke has no method typars"
                        // 'A -> 'B, declaring typars as frozen `TyTypar(Declaring, i)`.
                        Expect.equal
                            m.Signature
                            (TyFun(TyTypar(TyparAxis.Declaring, 0), TyTypar(TyparAxis.Declaring, 1)))
                            "Invoke signature"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }

            // An abstract method may carry its *own* generic parameters
            // (`abstract Map<'B> : 'A -> 'B`). Its `'B`
            // is no longer diagnosed as a free typar; it surfaces on the method as
            // `MethodTypeParams` and rides the signature as a `TyConst("'B", EqArray.empty)` marker,
            // distinct from the declaring type's `'A`.
            test "generic abstract method surfaces its own typars distinct from the declaring type's" {
                let tast =
                    analyse "namespace Vesper\n\ntype Mapper<'A> =\n    abstract member Map<'B> : arg: 'A -> 'B"

                Expect.isEmpty tast.Diagnostics "the method typar 'B is declared, not free"

                match tast.Decls with
                | EqList [ TDecl.Type td ] ->
                    Expect.equal td.Name "Mapper" "type name"
                    Expect.equal (EqArray.toList td.TypeParams) [ "'A" ] "declaring typar 'A only"

                    match td.Kind with
                    | TTypeKind.Interface(EqList [ m ]) ->
                        Expect.equal m.Name "Map" "method name"
                        Expect.equal (EqArray.toList m.MethodTypeParams) [ "'B" ] "method's own typar 'B"
                        // 'A is the declaring typar (Declaring 0), 'B the method's own (Method 0).
                        Expect.equal
                            m.Signature
                            (TyFun(TyTypar(TyparAxis.Declaring, 0), TyTypar(TyparAxis.Method, 0)))
                            "Map signature 'A -> 'B"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }

            // A module free function orders its method typars by the F# rule:
            // explicitly-declared `<'b,'a>` first IN DECLARATION ORDER, not by
            // first-appearance. So `'b` is `Method 0` and `'a` is `Method 1` even
            // though `'a` appears first in the signature (`x: 'a`). The frozen
            // `declTy` is `'a -> 'b -> ('a * 'b)` =
            // `!!1 -> !!0 -> (!!1 * !!0)`.
            test "free function honours declared `<'b,'a>` typar order over appearance" {
                let tast = analyse "let f<'b,'a> (x: 'a) (y: 'b) = (x, y)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(xTy, TyFun(yTy, TyTuple(EqList [ rx; ry ]))) ->
                    // x : 'a -> declared second -> Method 1
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 1)) "x : 'a is Method 1 (declared second)"
                    // y : 'b -> declared first -> Method 0
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 0)) "y : 'b is Method 0 (declared first)"
                    Expect.equal rx (TyTypar(TyparAxis.Method, 1)) "tuple .0 is 'a (Method 1)"
                    Expect.equal ry (TyTypar(TyparAxis.Method, 0)) "tuple .1 is 'b (Method 0)"
                | other -> failtestf "expected 'a -> 'b -> ('a * 'b), got %A" other
            }

            // Control: when declared order matches appearance order, the result is
            // unchanged — `'a` (declared first, appears first) is `Method 0`.
            test "free function declared order == appearance order is unchanged" {
                let tast = analyse "let g<'a,'b> (x: 'a) (y: 'b) = (x, y)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(xTy, TyFun(yTy, _)) ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 0)) "x : 'a is Method 0"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 1)) "y : 'b is Method 1"
                | other -> failtestf "expected 'a -> 'b -> _, got %A" other
            }
        ]

// A project-local generic *member* orders its method typars by the F# rule
// (`GeneralizedTypars.canonical`): explicitly-declared `<'C>` typars first in
// source order, then ALL other typars (annotation-derived AND body-inferred) by a
// single first-left-to-right-appearance walk over the final member type. This
// replaced the old 3-tier `explicit @ annotation @ body` append, which diverged
// from F# when an annotated param followed an unannotated (body-inferred) one.
[<Tests>]
let memberTyparOrderTests =
    let classMember (input: string) =
        let tast = analyse input
        Expect.isEmpty tast.Diagnostics "no diagnostics"

        let typeDecl =
            tast.Decls
            |> EqArray.toList
            |> List.tryPick (
                function
                | TDecl.Type t -> Some t
                | _ -> None
            )
            |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

        match typeDecl.Kind with
        | TTypeKind.Class c -> c.Members.[0]
        | other -> failwithf "expected TTypeKind.Class, got %A" other

    testList
        "MemberTyparOrder"
        [
            // Annotated-after-unannotated: `x` is body-inferred (no annotation), `y`
            // is annotated `'a`. The F# rule walks the final member type
            // `'x -> 'a -> ('x * 'a)`, so `'x` (first appearance) is `Method 0` and
            // `'a` is `Method 1` — NOT `'a` first (the old append put the annotation
            // typar ahead of the body-inferred one). The body-inferred typar gets a
            // synthetic `M0` name; the annotation typar keeps its source name `'a`.
            test "member orders annotated-after-unannotated by appearance, not annotation-first" {
                let m = classMember "type C() =\n    member this.M x (y: 'a) = (x, y)"

                // x (body-inferred) appears first ⇒ Method 0; y : 'a ⇒ Method 1.
                match EqArray.toList m.Params with
                | [ (_, xTy); (_, yTy) ] ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 0)) "x (body-inferred) is Method 0"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 1)) "y : 'a is Method 1"
                | other -> failtestf "expected two params, got %A" other

                Expect.equal
                    m.ReturnTy
                    (TyTuple(EqArray.ofList [ TyTypar(TyparAxis.Method, 0); TyTypar(TyparAxis.Method, 1) ]))
                    "returns (x * y) = (Method 0 * Method 1)"

                // Name preservation: the annotation typar keeps `'a`; the body-
                // inferred one gets the synthetic `M0`.
                Expect.equal
                    (GeneralizedTypars.names m.MethodTypeParams |> List.ofArray)
                    [ "M0"; "'a" ]
                    "names: synthetic body typar, preserved 'a"
            }

            // Control: an explicit `<'a>` member still orders explicit-first. `'a`
            // is declared, `y` is body-inferred — so `'a` is `Method 0` (declared)
            // even though `y` could appear first by some walks; here `x : 'a`
            // appears first anyway, but the declared rule pins it regardless.
            test "member with explicit `<'a>` orders the declared typar first" {
                let m = classMember "type C() =\n    member this.M<'a> (x: 'a) y = (x, y)"

                match EqArray.toList m.Params with
                | [ (_, xTy); (_, yTy) ] ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 0)) "x : 'a (declared) is Method 0"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 1)) "y (body-inferred) is Method 1"
                | other -> failtestf "expected two params, got %A" other

                Expect.equal
                    (GeneralizedTypars.names m.MethodTypeParams |> List.ofArray)
                    [ "'a"; "M0" ]
                    "names: declared 'a first, synthetic body typar"
            }
        ]

// The front-end union *shape* needed to compile `Vesper.Collections.List`
// verbatim — operator-named cases (`([])` → Empty, `(::)` → Cons) and the
// explicit-return (GADT-syntax) case forms FSharp.Core's list uses
// (`| ([]) : 'T list`, `| (::) : Head: 'T * Tail: 'T list -> 'T list`).
// Earlier `inspectCaseData` returned `""` for any operator head (the
// case was dropped) and `GadtNary`/`GadtNullary` were diagnosed "not supported".
module private UnionCaseSyntaxHelpers =
    let union (tast: TastFile) =
        let acc = ResizeArray<TTypeDecl * EqArray<TUnionCase>>()

        for d in tast.Decls do
            match d with
            | TDecl.Type td ->
                match td.Kind with
                | TTypeKind.Union(cs, _, _) -> acc.Add(td, cs)
                | _ -> ()
            | _ -> ()

        List.ofSeq acc

[<Tests>]
let unionCaseSyntaxTests =
    let union = UnionCaseSyntaxHelpers.union

    testList
        "UnionCaseSyntax"
        [
            // Operator-named cases in the plain (non-GADT) forms: `([])` is the
            // empty case (named `Empty`), `(::)` the cons case (named `Cons`).
            test "operator-named cases `([])` / `(::)` surface as Empty / Cons" {
                let tast = analyse "type Ops =\n    | ([])\n    | (::) of int * Ops"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match union tast with
                | [ (td, EqList [ c0; c1 ]) ] ->
                    Expect.equal td.Name "Ops" "type name"
                    Expect.equal c0.Name "Empty" "`([])` is named Empty"
                    Expect.isTrue c0.Fields.IsEmpty "Empty is nullary"
                    Expect.equal c1.Name "Cons" "`(::)` is named Cons"
                    Expect.equal c1.Fields.Length 2 "Cons has two fields"
                | other -> failtestf "unexpected unions: %A" other
            }

            // The verbatim FSharp.Core list shape: operator cases written with the
            // explicit-return (GADT) syntax, generic over the element type, the
            // tail referencing the declaring union recursively.
            test "explicit-return list cases surface with names, arity, and field names" {
                let tast =
                    analyse "type List<'T> =\n    | ([]): List<'T>\n    | (::): Head: 'T * Tail: List<'T> -> List<'T>"

                Expect.isEmpty tast.Diagnostics "no diagnostics for the GADT-syntax cases"

                match union tast with
                | [ (td, EqList [ empty; cons ]) ] ->
                    Expect.equal td.Name "List" "type name"
                    Expect.equal (EqArray.toList td.TypeParams) [ "'T" ] "one declared typar"

                    Expect.equal empty.Name "Empty" "`([])` is named Empty"
                    Expect.isTrue empty.Fields.IsEmpty "Empty is nullary"

                    Expect.equal cons.Name "Cons" "`(::)` is named Cons"

                    match cons.Fields with
                    | EqList [ (hn, ht); (tn, tt) ] ->
                        Expect.equal hn (ValueSome "Head") "first field named Head"
                        // The element typar surfaces as the backend marker.
                        Expect.equal ht (TyTypar(TyparAxis.Declaring, 0)) "Head : 'T"
                        Expect.equal tn (ValueSome "Tail") "second field named Tail"
                        // Tail refers back to the declaring union, applied to 'T.
                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TyTypar(TyparAxis.Declaring, 0))))
                            "Tail : List<'T>"
                    | other -> failtestf "expected two named Cons fields, got %A" other
                | other -> failtestf "unexpected unions: %A" other
            }
        ]

// The `and 'T list = List<'T>` recursive abbreviation retargets `[…]` list
// literals onto a program-declared list union (the self-host shape) instead
// of FSharp.Core's `FSharpList`. Additive:
// a normal program declares no `list` abbreviation, so its list literals keep
// the `Microsoft.FSharp.Collections.list` nominal + `Cons`/`Nil` case names.
// The generic-union *backend emission* that makes `[1;2;3]` runnable against
// our own list is the next slice; this slice is the front-end resolution.
[<Tests>]
let listAbbrevTests =
    let listSrc =
        String.concat
            "\n"
            [
                "type List<'T> ="
                "    | ([]): List<'T>"
                "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                "and 'T list = List<'T>"
            ]

    testList
        "ListAbbrev"
        [
            // The verbatim `list.fs` shape: the abbreviation's RHS references
            // the union it shares an `and` group with, and the union's `Tail`
            // field references back through the `'T list` abbreviation.
            test "`and 'T list = List<'T>` type-checks with the verbatim list.fs case shape" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type List<'T> ="
                            "    | ([]): 'T list"
                            "    | (::): Head: 'T * Tail: 'T list -> 'T list"
                            "and 'T list = List<'T>"
                        ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics for the recursive abbrev + GADT cases"

                match UnionCaseSyntaxHelpers.union tast with
                | [ (td, EqList [ empty; cons ]) ] ->
                    Expect.equal td.Name "List" "type name"
                    Expect.equal empty.Name "Empty" "`([])` is Empty"

                    match cons.Fields with
                    | EqList [ (_, ht); (_, tt) ] ->
                        Expect.equal ht (TyTypar(TyparAxis.Declaring, 0)) "Head : 'T"
                        // `'T list` resolved through the abbrev back to the union.
                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TyTypar(TyparAxis.Declaring, 0))))
                            "Tail : List<'T> via the abbrev"
                    | other -> failtestf "expected two Cons fields, got %A" other
                | other -> failtestf "unexpected unions: %A" other
            }

            // The headline: a `[1; 2; 3]` literal in a program that declares the
            // list union + abbrev types as that union and freezes to a Cons chain
            // terminated by the union's own empty case (`Empty`, not `Nil`).
            test "`[1; 2; 3]` resolves to the declared list union and freezes a Cons/Empty chain" {
                let src = listSrc + "\nlet xs = [1; 2; 3]"
                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let xs =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, _) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, v, _, ty) -> v, ty
                        | _ -> failwith "unreachable"
                    )

                match xs with
                | ValueSome(value, ty) ->
                    Expect.equal
                        ty
                        (TyUnion("List", EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty))))
                        "xs : List<int> (the declared union)"

                    Expect.equal
                        (TastShape.prettyExpr value)
                        "Cons(1, Cons(2, Cons(3, Empty)))"
                        "Cons chain terminated by the union's Empty case"
                | ValueNone -> failtest "no `let xs` binding surfaced"
            }

            // The empty literal `[]` resolves to the union's nullary case too.
            test "`[]` resolves to the declared union's empty case" {
                let src = listSrc + "\nlet e : int list = []"
                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let e =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, _) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, v, _, ty) -> v, ty
                        | _ -> failwith "unreachable"
                    )

                match e with
                | ValueSome(value, ty) ->
                    Expect.equal
                        ty
                        (TyUnion("List", EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty))))
                        "e : List<int>"

                    Expect.equal (TastShape.prettyExpr value) "Empty" "the bare `[]` is the union's Empty case"
                | ValueNone -> failtest "no `let e` binding surfaced"
            }

            // Regression: with no `list` abbreviation in scope, a list literal
            // stays the FSharp.Core nominal with `Cons`/`Nil` (additive — Slice4
            // and every existing list-bearing program are untouched).
            test "a list literal with no `list` abbrev keeps the FSharp.Core nominal" {
                let tast = analyse "let xs = [1; 2; 3]"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let xs =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, _, _, _) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, v, _, ty) -> v, ty
                        | _ -> failwith "unreachable"
                    )

                match xs with
                | ValueSome(value, ty) ->
                    Expect.equal
                        ty
                        (SemType.TyRecord(
                            RuntimeNames.fsharpCoreListKey,
                            EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty))
                        ))
                        "xs : Microsoft.FSharp.Collections.list<int> (the FSharp.Core default)"

                    Expect.equal
                        (TastShape.prettyExpr value)
                        "Cons(1, Cons(2, Cons(3, Nil)))"
                        "the default FSharp.Core Cons/Nil chain"
                | ValueNone -> failtest "no `let xs` binding surfaced"
            }
        ]

[<Tests>]
let unionMemberTests =
    // P3d.3: union augmentation members (`with member …` / `static member …`)
    // type-check through the whole pipeline and surface on `TTypeKind.Union`
    // with their lowered bodies.
    let memberSrc =
        String.concat
            "\n"
            [
                "type Lst ="
                "    | Nil"
                "    | Cons of int * Lst"
                ""
                "    member this.IsEmpty ="
                "        match this with"
                "        | Nil -> true"
                "        | Cons(_, _) -> false"
                ""
                "    member this.Head ="
                "        match this with"
                "        | Cons(h, _) -> h"
                "        | Nil -> failwith \"empty\""
                ""
                "    static member Empty = Nil"
                "    static member Single x = Cons(x, Nil)"
            ]

    testList
        "UnionMembers"
        [
            test "augmentation members surface with kind, static-ness, types, and a this binder" {
                let tast = analyse memberSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let members =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union _ } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union(_, ms, _) } -> ValueSome(EqArray.toList ms)
                        | _ -> ValueNone
                    )

                match members with
                | ValueSome ms ->
                    let find n =
                        ms |> List.find (fun (m: TTypeMember) -> m.Name = n)

                    let isEmpty = find "IsEmpty"
                    Expect.isFalse isEmpty.IsStatic "IsEmpty is an instance member"
                    Expect.equal isEmpty.Kind TMemberKind.Property "IsEmpty is a property"

                    Expect.equal isEmpty.ReturnTy (TyConst(RuntimeNames.boolKey, EqArray.empty)) "IsEmpty : bool"

                    Expect.isTrue (ValueOption.isSome isEmpty.ThisKey) "an instance member carries a `this` binder"

                    Expect.equal (find "Head").ReturnTy (TyConst(RuntimeNames.intKey, EqArray.empty)) "Head : int"

                    let empty = find "Empty"
                    Expect.isTrue empty.IsStatic "Empty is static"
                    Expect.equal empty.ReturnTy (TyUnion("Lst", EqArray.empty)) "Empty : Lst"
                    Expect.isTrue (ValueOption.isNone empty.ThisKey) "a static member has no `this` binder"

                    let single = find "Single"
                    Expect.isTrue single.IsStatic "Single is static"
                    Expect.equal single.Kind TMemberKind.Method "Single is a method"
                    Expect.equal single.Params.Length 1 "Single takes one parameter"
                    Expect.equal single.ReturnTy (TyUnion("Lst", EqArray.empty)) "Single : int -> Lst"
                | ValueNone -> failtest "no union surfaced"
            }

            test "instance + static member access type-checks against the union's members" {
                let tast =
                    analyse (
                        memberSrc
                        + "\nlet xs = Cons(1, Nil)\nlet h = xs.Head\nlet e = Lst.Empty\nlet s = Lst.Single 2"
                    )

                Expect.isEmpty tast.Diagnostics "no diagnostics for xs.Head / Lst.Empty / Lst.Single"
            }
        ]

[<Tests>]
let unionInterfaceImplTests =
    // A union implementing an interface (`interface IFace with member …`) now
    // CARRIES the impl in its frozen representation
    // (`TTypeKind.Union(cases, members, interfaces)`) instead of being silently
    // dropped for lack of one. Front-end only this slice — the impl resolves +
    // conformance-checks and surfaces on the frozen union; codegen emits nothing
    // for it yet. A project-local interface is used so the resolution does not lean
    // on the external provider knowing any BCL interface.
    let src =
        String.concat
            "\n"
            [
                "type IDescribe ="
                "    abstract member Describe : unit -> int"
                ""
                "type U ="
                "    | A"
                "    | B"
                ""
                "    interface IDescribe with"
                "        member this.Describe() = 1"
            ]

    testList
        "UnionInterfaceImpl"
        [
            test "a union implementing a local interface surfaces the impl on TTypeKind.Union.interfaces" {
                let tast = analyse src

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                // Pre-slice this froze as `Union(cases, members)` with the impl gone;
                // post-slice the third positional `interfaces` carries it.
                let interfaces =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type { Name = "U"; Kind = TTypeKind.Union _ } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union(_, _, ifaces) } -> ValueSome ifaces
                        | _ -> ValueNone
                    )

                match interfaces with
                | ValueSome ifaces ->
                    Expect.equal ifaces.Length 1 "exactly one interface impl is carried on the frozen union"

                    let (ifaceTy, members) = ifaces.[0]

                    match ifaceTy with
                    | TyClass(name, _) ->
                        Expect.stringContains name "IDescribe" "the impl heads the IDescribe interface"
                    | other -> failtestf "interface head is not a TyClass: %A" other

                    Expect.equal members.Length 1 "the Describe member body is carried with the impl"
                    Expect.equal members.[0].Name "Describe" "the carried member is Describe"
                | ValueNone -> failtest "no union U carrying interface impls surfaced"
            }

            // A union whose ONLY member is an interface impl whose body READS `this`
            // (via `match this`). Before the NameResolution guard was relaxed, a
            // members-empty union never had its impl bodies name-resolved, so `this`
            // (and the case-payload binders) resolved to an unbound `External` →
            // "unsupported external value". This is the gating fix for a union (e.g.
            // `List`) implementing `seq` whose `GetEnumerator` must reference `this`.
            test "a union interface-impl body can read `this` (match self) without an unbound-external error" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IRank ="
                            "    abstract member Rank : unit -> int"
                            ""
                            "type V ="
                            "    | Lo"
                            "    | Hi of int"
                            ""
                            "    interface IRank with"
                            "        member this.Rank() ="
                            "            match this with"
                            "            | Lo -> 0"
                            "            | Hi n -> n"
                        ]

                let tast = analyse src

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                Expect.isEmpty
                    errors
                    (sprintf "no front-end errors — `this`/payload resolve in the impl body (%A)" errors)

                // And the impl still freezes onto the union (the body typed cleanly).
                let carried =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type { Name = "V"; Kind = TTypeKind.Union _ } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union(_, _, ifaces) } -> ValueSome ifaces
                        | _ -> ValueNone
                    )

                match carried with
                | ValueSome ifaces -> Expect.equal ifaces.Length 1 "the IRank impl is carried on the frozen union"
                | ValueNone -> failtest "no union V carrying interface impls surfaced"
            }
        ]

[<Tests>]
let recordInterfaceImplTests =
    // §14.6 slice 5 (front-end): a record implementing a local interface CARRIES
    // the impl in its frozen representation (`TTypeKind.Record(fields, members,
    // interfaces)`) — the same machinery as the union slice. The impl resolves +
    // conformance-checks and surfaces on the frozen record. A project-local
    // interface keeps the resolution off the external provider.
    let src =
        String.concat
            "\n"
            [
                "type IRank ="
                "    abstract member Rank : unit -> int"
                ""
                "type R ="
                "    { N: int }"
                ""
                "    interface IRank with"
                "        member this.Rank() = this.N"
            ]

    testList
        "RecordInterfaceImpl"
        [
            test "a record implementing a local interface surfaces the impl on TTypeKind.Record.interfaces" {
                let tast = analyse src

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                let interfaces =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type {
                                         Name = "R"
                                         Kind = TTypeKind.Record _
                                     } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type {
                                         Kind = TTypeKind.Record(_, _, ifaces)
                                     } -> ValueSome ifaces
                        | _ -> ValueNone
                    )

                match interfaces with
                | ValueSome ifaces ->
                    Expect.equal ifaces.Length 1 "exactly one interface impl is carried on the frozen record"

                    let (ifaceTy, members) = ifaces.[0]

                    match ifaceTy with
                    | TyClass(name, _) -> Expect.stringContains name "IRank" "the impl heads the IRank interface"
                    | other -> failtestf "interface head is not a TyClass: %A" other

                    Expect.equal members.Length 1 "the Rank member body is carried with the impl"
                    Expect.equal members.[0].Name "Rank" "the carried member is Rank"
                | ValueNone -> failtest "no record R carrying interface impls surfaced"
            }
        ]
