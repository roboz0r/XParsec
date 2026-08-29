module XParsec.FSharp.SemanticAnalysis.Tests.ElaborateTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

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
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), ty, _), _, letTy) ->
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

            test "`let f = fun x -> x + 1` -> Lambda over an InlineCall of the served operator" {
                let tast = analyse "let f = fun x -> x + 1"
                let intTy = BuiltinTypes.tyInt
                let intToInt = TyFun(intTy, intTy)

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = fun v1 -> spec#0(v1, 1)" "TAST shape"

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
                    "let v0 = fun v1 -> spec#0(v1, 1)"
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

            test "TDecl.Let binding NodeKey matches binding-pattern NodeKey" {
                let tast = analyse "let x = 1"
                let expected = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls.[0] with
                | TDecl.Let(TPat.NamedSimple(bindingKey, _, _), _, _, _) ->
                    Expect.equal bindingKey expected "binding key"
                | other -> failtestf "unexpected: %A" other
            }

            test "TVar references the original binding-pattern NodeKey" {
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

            test "`let xs = [1; 2; 3]` freezes as nested Cons / Empty over `List<int>`" {
                let tast = analyse "let xs = [1; 2; 3]"
                let intTy = BuiltinTypes.tyInt

                let listTy = SemType.TyUnion(RuntimeNames.vesperListKey, EqArray.singleton intTy)

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) listTy "xs : List<int>"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.UnionCons("Cons",
                                            EqList [ TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), _, _)
                                                     TExpr.UnionCons("Cons",
                                                                     EqList [ TExpr.Const(TConstValue.Integral(IntKind.Int32,
                                                                                                               2L),
                                                                                          _,
                                                                                          _)
                                                                              TExpr.UnionCons("Cons",
                                                                                              EqList [ TExpr.Const(TConstValue.Integral(IntKind.Int32,
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
                                            _),
                            _,
                            _) -> Expect.equal outerTy listTy "outer UnionCons ty"
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = []` freezes as empty Empty with a free element type" {
                let tast = analyse "let xs = []"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.UnionCons("Empty", EqList [], ty, _), _, _) ->
                    match ty with
                    | TyUnion("Vesper.Collections.List`1", args) when args.Length = 1 -> ()
                    | _ -> failtestf "expected List<_> Empty, got %A" ty
                | other -> failtestf "unexpected TAST shape: %A" other
            }

            test "`let xs = [|1; 2|]` freezes as an array literal over its elements" {
                let tast = analyse "let xs = [|1; 2|]"
                let intTy = BuiltinTypes.tyInt
                let arrayTy = TyConst(RuntimeNames.arrayKey 1, EqArray.singleton intTy)

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) arrayTy "xs : int[]"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.ArrayLit(EqList [ TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), _, _)
                                                    TExpr.Const(TConstValue.Integral(IntKind.Int32, 2L), _, _) ],
                                           outerTy,
                                           _),
                            _,
                            _) -> Expect.equal outerTy arrayTy "array literal ty"
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

// Every pass walks `namespace`-rooted files, so a `let` under a `namespace` is
// name-resolved, inferred and frozen exactly as in a module file.
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
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), ty, _), _, letTy) ->
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
                // `y`'s body references the earlier `x`, so it types only if the passes walked
                // the namespace. Comparing against the module form avoids hard-coding the
                // freshly-named TAST rendering.
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

// A nested `module Foo = …` is descended into and its body flattened to the enclosing
// scope, the same simplification applied to namespace groups.
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
                | TDecl.Let(_, TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), ty, _), _, letTy) ->
                    Expect.equal ty BuiltinTypes.tyInt "value type int"
                    Expect.equal letTy BuiltinTypes.tyInt "binding type int"
                | other -> failtestf "unexpected: %A" other
            }

            test "name resolution + inference run inside the nested module body" {
                // `y`'s body references the outer `top`, and inferring `y : int` rather than a
                // free TyVar is what shows the passes descended into the nested module.
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

            // The DECL flattens; the CONTAINMENT does not. A binding's container is the whole
            // chain of modules it is written in, the same chain a type declared there gets —
            // otherwise one source location would have two containments.
            test "a binding in a nested module is held by the WHOLE module chain" {
                let tast =
                    analyse "namespace N\n\nmodule A =\n    module B =\n        let f (x: int) = x + 1"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let info =
                    match tast.ModuleMembers |> Map.toList |> List.map snd with
                    | [ info ] -> info
                    | other -> failtestf "expected exactly one module member, got %A" other

                Expect.equal info.Name "f" "the binding's compiled name"

                match info.DeclaringModule with
                | ValueNone -> failtest "expected a declaring module"
                | ValueSome b ->
                    Expect.equal b.Name "B" "held by the INNERMOST module"

                    match b.Container with
                    | ModuleContainer.InModule a ->
                        Expect.equal a.Name "A" "which is itself held by the outer module"

                        Expect.equal
                            (List.ofSeq a.Namespace.Path.Underlying)
                            [ "N" ]
                            "and the outer module by the namespace — neither module is a namespace segment"
                    | other -> failtestf "expected B's container to be module A, got %A" other

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

// An interface-shaped type surfaces as a `TDecl.Type` whose method signatures come from the
// RESOLVED member types, not from a re-translation of the CST signature.
[<Tests>]
let interfaceTests =
    testList
        "InterfaceFreeze"
        [
            test "`Fun` interface freezes to TDecl.Type with the resolved Invoke signature" {
                // `Vesper.Fun` is Vesper.Core's OWN type and the provider mounts Vesper.Core's
                // contract, so this file must be compiled AS Vesper.Core: only the declaring
                // assembly may declare the types its contract publishes.
                let src =
                    "namespace Vesper\n\ntype Fun<'A, 'B> =\n    abstract member Invoke: arg: 'A -> 'B"

                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseSemFor
                        {
                            Name = AssemblyName "Vesper.Core"
                            Target = "clr"
                        }
                        realProvider.Value
                        (LexedFile.ofText lexed)
                        file

                Expect.isEmpty tast.Diagnostics "no diagnostics for an abstract member"

                match tast.Decls with
                | EqList [ TDecl.Type td ] ->
                    Expect.equal td.Name "Fun" "type name"
                    Expect.equal td.TypeKey.Namespace.Dotted "Vesper" "namespace"
                    Expect.equal (EqArray.toList td.TypeParams) [ "'A"; "'B" ] "declared typars"

                    match td.Kind with
                    | TTypeKind.Interface(EqList [ m ]) ->
                        Expect.equal m.Name "Invoke" "method name"
                        Expect.isTrue m.MethodTypeParams.IsEmpty "Invoke has no method typars"

                        Expect.equal
                            m.Signature
                            (TyFun(TyTypar(TyparAxis.Declaring, 0), TyTypar(TyparAxis.Declaring, 1)))
                            "Invoke signature"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }

            // An abstract method may carry its OWN generic parameters (`abstract Map<'B> : 'A ->
            // 'B`). `'B` is not a free typar: it surfaces in `MethodTypeParams` and appears in
            // the signature on the `Method` axis, distinct from the declaring type's `'A`.
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

                        Expect.equal
                            m.Signature
                            (TyFun(TyTypar(TyparAxis.Declaring, 0), TyTypar(TyparAxis.Method, 0)))
                            "Map signature 'A -> 'B"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
            }

            // Explicitly-declared typars are ordered by DECLARATION order, not first appearance:
            // in `f<'b,'a> (x: 'a) (y: 'b)`, `'b` is `Method 0` and `'a` is `Method 1` even
            // though `'a` appears first in the signature.
            test "free function honours declared `<'b,'a>` typar order over appearance" {
                let tast = analyse "let f<'b,'a> (x: 'a) (y: 'b) = (x, y)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match declType tast with
                | TyFun(xTy, TyFun(yTy, TyTuple(EqList [ rx; ry ]))) ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 1)) "x : 'a is Method 1 (declared second)"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 0)) "y : 'b is Method 0 (declared first)"
                    Expect.equal rx (TyTypar(TyparAxis.Method, 1)) "tuple .0 is 'a (Method 1)"
                    Expect.equal ry (TyTypar(TyparAxis.Method, 0)) "tuple .1 is 'b (Method 0)"
                | other -> failtestf "expected 'a -> 'b -> ('a * 'b), got %A" other
            }

            // Control: declared order matching appearance order leaves `'a` at `Method 0`.
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

// A generic member orders its method typars by the F# rule: explicitly-declared ones first in
// source order, then all others — annotation-derived and body-inferred alike — by a single
// left-to-right appearance walk over the final member type.
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
            // Annotated-after-unannotated: in `M x (y: 'a)` the walk over `'x -> 'a -> ('x * 'a)`
            // puts body-inferred `'x` at `Method 0` and annotated `'a` at `Method 1`. The
            // body-inferred one is named `M0`; the annotation keeps its source name `'a`.
            test "member orders annotated-after-unannotated by appearance, not annotation-first" {
                let m = classMember "type C() =\n    member this.M x (y: 'a) = (x, y)"

                match EqArray.toList m.Params with
                | [ (_, xTy); (_, yTy) ] ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 0)) "x (body-inferred) is Method 0"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 1)) "y : 'a is Method 1"
                | other -> failtestf "expected two params, got %A" other

                Expect.equal
                    m.ReturnTy
                    (TyTuple(EqArray.ofList [ TyTypar(TyparAxis.Method, 0); TyTypar(TyparAxis.Method, 1) ]))
                    "returns (x * y) = (Method 0 * Method 1)"

                Expect.equal
                    [ for (n, _) in m.MethodTypeParams -> n ]
                    [ "M0"; "'a" ]
                    "names: synthetic body typar, preserved 'a"
            }

            // Control: an explicit `<'a>` is `Method 0` because it is declared, ahead of the
            // body-inferred `y`.
            test "member with explicit `<'a>` orders the declared typar first" {
                let m = classMember "type C() =\n    member this.M<'a> (x: 'a) y = (x, y)"

                match EqArray.toList m.Params with
                | [ (_, xTy); (_, yTy) ] ->
                    Expect.equal xTy (TyTypar(TyparAxis.Method, 0)) "x : 'a (declared) is Method 0"
                    Expect.equal yTy (TyTypar(TyparAxis.Method, 1)) "y (body-inferred) is Method 1"
                | other -> failtestf "expected two params, got %A" other

                Expect.equal
                    [ for (n, _) in m.MethodTypeParams -> n ]
                    [ "'a"; "M0" ]
                    "names: declared 'a first, synthetic body typar"
            }
        ]

// The union shape a verbatim `list` declaration needs: operator-named cases (`([])` → Empty,
// `(::)` → Cons), in the plain form and in the explicit-return one
// (`| (::) : Head: 'T * Tail: 'T list -> 'T list`).
module private UnionCaseSyntaxHelpers =
    let union (tast: TastFile) =
        let acc = ResizeArray<TTypeDecl * EqArray<TUnionCase>>()

        for d in tast.Decls do
            match d with
            | TDecl.Type td ->
                match td.Kind with
                | TTypeKind.Union u -> acc.Add(td, u.Cases)
                | _ -> ()
            | _ -> ()

        List.ofSeq acc

[<Tests>]
let unionCaseSyntaxTests =
    let union = UnionCaseSyntaxHelpers.union

    testList
        "UnionCaseSyntax"
        [
            // In the plain form, `([])` is named `Empty` and `(::)` is named `Cons`.
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

            // Operator cases in explicit-return syntax, generic over the element type, with the
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
                        Expect.equal ht (TyTypar(TyparAxis.Declaring, 0)) "Head : 'T"
                        Expect.equal tn (ValueSome "Tail") "second field named Tail"

                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TyTypar(TyparAxis.Declaring, 0))))
                            "Tail : List<'T>"
                    | other -> failtestf "expected two named Cons fields, got %A" other
                | other -> failtestf "unexpected unions: %A" other
            }
        ]

// An `and 'T list = List<'T>` abbreviation retargets `[…]` literals onto the program's own
// list union; without one they stay on `Vesper.Collections.List`.
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
            // The abbreviation's RHS references the union it shares an `and` group with, and the
            // union's `Tail` field references back through the `'T list` abbreviation.
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

                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TyTypar(TyparAxis.Declaring, 0))))
                            "Tail : List<'T> via the abbrev"
                    | other -> failtestf "expected two Cons fields, got %A" other
                | other -> failtestf "unexpected unions: %A" other
            }

            // The literal resolves to the program's OWN union, not the external cons-list,
            // and freezes to a chain of its case names.
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

            // With no `list` abbreviation in scope the literal stays the default cons-list.
            test "a list literal with no `list` abbrev keeps the Vesper nominal" {
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
                        (SemType.TyUnion(
                            RuntimeNames.vesperListKey,
                            EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty))
                        ))
                        "xs : Vesper.Collections.List<int> (the default cons-list)"

                    Expect.equal
                        (TastShape.prettyExpr value)
                        "Cons(1, Cons(2, Cons(3, Empty)))"
                        "the default Cons/Empty chain"
                | ValueNone -> failtest "no `let xs` binding surfaced"
            }
        ]

[<Tests>]
let unionMemberTests =
    // Union augmentation members type-check through the whole pipeline and surface on
    // `TTypeKind.Union` with their lowered bodies.
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
            test "augmentation members surface with kind, static-ness, types, and a this bound variable" {
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
                        | TDecl.Type { Kind = TTypeKind.Union u } -> ValueSome(EqArray.toList u.Members)
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

                    Expect.isTrue
                        (ValueOption.isSome isEmpty.ThisKey)
                        "an instance member carries a `this` bound variable"

                    Expect.equal (find "Head").ReturnTy (TyConst(RuntimeNames.intKey, EqArray.empty)) "Head : int"

                    let empty = find "Empty"
                    Expect.isTrue empty.IsStatic "Empty is static"
                    Expect.equal empty.ReturnTy (TyUnion("Lst", EqArray.empty)) "Empty : Lst"
                    Expect.isTrue (ValueOption.isNone empty.ThisKey) "a static member has no `this` bound variable"

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
    // A union implementing an interface carries the impl in its frozen representation, the
    // `Interfaces` slot of `TTypeKind.Union`. The interface is
    // project-local so resolution does not lean on the provider knowing a BCL one.
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

                let errors = tast.Diagnostics |> Diagnostic.errors
                Expect.isEmpty errors (sprintf "no front-end errors (%A)" errors)

                let interfaces =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type { Name = "U"; Kind = TTypeKind.Union _ } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union u } -> ValueSome u.Interfaces
                        | _ -> ValueNone
                    )

                match interfaces with
                | ValueSome ifaces ->
                    Expect.equal ifaces.Length 1 "exactly one interface impl is carried on the frozen union"

                    let (ifaceTy, members) = ifaces.[0]

                    match ifaceTy with
                    | TyClass(name, _) ->
                        Expect.stringContains name "IDescribe" "the impl names the IDescribe interface"
                    | other -> failtestf "the interface type is not a TyClass: %A" other

                    Expect.equal members.Length 1 "the Describe member body is carried with the impl"
                    Expect.equal members.[0].Name "Describe" "the carried member is Describe"
                | ValueNone -> failtest "no union U carrying interface impls surfaced"
            }

            // A union whose ONLY member is an interface impl: its impl bodies must still be
            // name-resolved, or `this` and the case-payload bound variables resolve to an
            // unbound `External`. `List` implementing `seq` is the case that needs it.
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

                let errors = tast.Diagnostics |> Diagnostic.errors

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
                        | TDecl.Type { Kind = TTypeKind.Union u } -> ValueSome u.Interfaces
                        | _ -> ValueNone
                    )

                match carried with
                | ValueSome ifaces -> Expect.equal ifaces.Length 1 "the IRank impl is carried on the frozen union"
                | ValueNone -> failtest "no union V carrying interface impls surfaced"
            }
        ]

[<Tests>]
let recordInterfaceImplTests =
    // A record implementing a local interface carries the impl in its frozen representation,
    // `TTypeKind.Record` — the union case's machinery.
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

                let errors = tast.Diagnostics |> Diagnostic.errors
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
                        | TDecl.Type { Kind = TTypeKind.Record r } -> ValueSome r.Interfaces
                        | _ -> ValueNone
                    )

                match interfaces with
                | ValueSome ifaces ->
                    Expect.equal ifaces.Length 1 "exactly one interface impl is carried on the frozen record"

                    let (ifaceTy, members) = ifaces.[0]

                    match ifaceTy with
                    | TyClass(name, _) -> Expect.stringContains name "IRank" "the impl names the IRank interface"
                    | other -> failtestf "the interface type is not a TyClass: %A" other

                    Expect.equal members.Length 1 "the Rank member body is carried with the impl"
                    Expect.equal members.[0].Name "Rank" "the carried member is Rank"
                | ValueNone -> failtest "no record R carrying interface impls surfaced"
            }
        ]

// `[<Global>]` DECLARES a module value to BE a target global, recording its symbol so no
// definition is emitted. Checked both ways: a marked binding must be a bare intrinsic
// template, and an unmarked one may not restate its own emitted name.
[<Tests>]
let globalAttributeTests =
    let errorsOf (input: string) =
        (analyse input).Diagnostics
        |> Diagnostic.errors
        |> List.map (fun d -> d.Message)

    testList
        "Elaborate [<Global>]"
        [
            test "a [<Global>] binding is recorded by symbol and emits no diagnostic" {
                let tast = analyse "module M\n\n[<Global>]\nlet undefined = (# \"undefined\" #)\n"

                Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) "a well-formed global is accepted"

                match tast.Decls with
                | EqList [ TDecl.Let _ ] -> ()
                | other -> failtestf "expected a single TDecl.Let, got %A" other

                Expect.equal
                    (tast.GlobalValueKeys |> EqSet.toList |> List.map SymbolKeyOps.simpleName)
                    [ DisplayName "undefined" ]
                    "the binding's own identity carries the declaration"
            }

            test "an unmarked binding records no global" {
                let tast = analyse "module M\n\nlet emptyDocs = (# \"[]\" #)\n"
                Expect.isTrue tast.GlobalValueKeys.IsEmpty "nothing declared, nothing recorded"
            }

            test "[<Global>] on a body that is not a bare intrinsic is an error citing the binding" {
                match errorsOf "module M\n\n[<Global>]\nlet total = 42\n" with
                | [ msg ] ->
                    Expect.stringContains msg "'total'" "the diagnostic refers to the binding"
                    Expect.stringContains msg "[<Global>]" "and the attribute it is about"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "a binding restating its own target global without [<Global>] is an error" {
                match errorsOf "module M\n\nlet undefined = (# \"undefined\" #)\n" with
                | [ msg ] ->
                    Expect.stringContains msg "'undefined'" "the diagnostic refers to the binding"
                    Expect.stringContains msg "[<Global>]" "and says how to declare it"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            // The name is NOT the template text, so this defines a value rather than
            // restating a global.
            test "a nullary intrinsic whose name differs from its template is an ordinary binding" {
                Expect.isEmpty (errorsOf "module M\n\nlet emptyDocs = (# \"[]\" #)\n") "no diagnostic"
            }

            test "[<Global>] on a pattern with no single bound name is an error, not a silent no-op" {
                // The declaration is filed under the value's identity, so a wildcard pattern
                // has nowhere to carry it — and honouring it silently would emit the very
                // definition the attribute exists to suppress.
                let tast = analyse "module M\n\n[<Global>]\nlet _ = (# \"undefined\" #)\n"

                Expect.isTrue tast.GlobalValueKeys.IsEmpty "nothing was recorded"

                match tast.Diagnostics |> Diagnostic.errors |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "this binding has no single name"
                        "the diagnostic is reported at the pattern"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "a same-named user type does NOT take [<Global>]'s meaning" {
                // `Global` here reaches the user's own class — a local claim beats the contract
                // — so the error is reported at the binding, for restating a global rather than
                // declaring one.
                let src =
                    String.concat
                        "\n"
                        [
                            "namespace Mine"
                            ""
                            "type GlobalAttribute() ="
                            "    member this.M () = 1"
                            ""
                            "module Ambient ="
                            "    [<Global>]"
                            "    let undefined = (# \"undefined\" #)"
                        ]

                let tast = analyse src

                Expect.isTrue tast.GlobalValueKeys.IsEmpty "the user's GlobalAttribute cannot hijack the compiler's"

                match tast.Diagnostics |> Diagnostic.errors |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "restates the target global" "so the binding reads as unmarked"
                | other -> failtestf "expected exactly one error, got %A" other
            }
        ]

[<Tests>]
let propertySetterTests =
    // Every `set` accessor lowers to a `set_P` METHOD, so a write to a declared property is a
    // call; only a real field write reaches `TExpr.FieldSet`.
    let classDecl =
        String.concat
            "\n"
            [
                "type C() ="
                "    let mutable q = 0"
                "    member this.Q with get () = q and set (w: int) = q <- w"
            ]

    /// The member names called and the field names written, across every top-level value and
    /// class member body.
    let writes (tast: TastFile) =
        let calls = ResizeArray<string>()
        let fieldSets = ResizeArray<string>()

        let collect =
            { TastWalk.identityIter with
                VisitExpr =
                    fun _ e ->
                        match e with
                        | TExpr.MethodCall(key = key) ->
                            let (DisplayName name) = SymbolKeyOps.simpleName key
                            calls.Add name
                        | TExpr.FieldSet(_, name, _, _, _) -> fieldSets.Add name
                        | _ -> ()

                        true
            }

        for d in EqArray.toList tast.Decls do
            match d with
            | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr collect value
            | TDecl.Expression(e, _) -> TastWalk.iterExpr collect e
            | TDecl.Type td ->
                match td.Kind with
                | TTypeKind.Class c ->
                    for m in EqArray.toList c.Members do
                        TastWalk.iterExpr collect m.Body
                | _ -> ()

        {|
            Calls = List.ofSeq calls
            FieldSets = List.ofSeq fieldSets
        |}

    testList
        "PropertySetterElaboration"
        [
            test "`c.Q <- v` on a declared setter lowers to a set_Q call" {
                let tast = analyse (classDecl + "\nlet s (c: C) = c.Q <- 1")
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let w = writes tast
                Expect.contains w.Calls "set_Q" "the write dispatches through the accessor method"
                Expect.isFalse (w.FieldSets |> List.contains "Q") "C declares no field Q to write"
            }

            // A computed object argument parses as `Expr.DotLookup`, the other of the two
            // shapes an assignment LHS folds through.
            test "`(mk ()).Q <- v` lowers to a set_Q call" {
                let src = classDecl + "\nlet mk () = C()\nlet s () = (mk ()).Q <- 1"

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let w = writes tast
                Expect.contains w.Calls "set_Q" "the write dispatches through the accessor method"
                Expect.isFalse (w.FieldSets |> List.contains "Q") "C declares no field Q to write"
            }

            test "a mutable record field write stays a FieldSet" {
                let tast = analyse "type R = { mutable X: int }\nlet s (r: R) = r.X <- 1"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let w = writes tast
                Expect.contains w.FieldSets "X" "a real field takes the field-write path"
                Expect.isEmpty w.Calls "no accessor call is minted for a field"
            }

            // A write-only property declares `set_Q` and no `Q`, so the LHS has no readable
            // half: the RHS is checked against the setter's declared value instead.
            test "a write-only property's assignment type-checks against set_Q's parameter" {
                let writeOnly =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    let mutable q = 0"
                            "    member this.Q with set (w: int) = q <- w"
                            "    member this.Write(n: int) = this.Q <- n"
                        ]

                let tast = analyse writeOnly
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.contains (writes tast).Calls "set_Q" "the write dispatches through the accessor method"
            }

            test "a write-only property rejects a value of the wrong type" {
                let bad =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    let mutable q = 0"
                            "    member this.Q with set (w: int) = q <- w"
                            "    member this.Write(n: string) = this.Q <- n"
                        ]

                match (analyse bad).Diagnostics |> Diagnostic.errors |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "int"
                        "the setter's declared int parameter is what the string is checked against"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            // The type-check and the lowering classify the LHS through one `AssignTarget`, so
            // every object-argument shape the lowering writes through `set_Q` also types
            // through it. These two shapes reach no binding to read a type off.
            test "a write-only property takes a computed object argument" {
                let probe =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    let mutable q = 0"
                            "    member this.Q with set (w: int) = q <- w"
                            "let mk () = C()"
                            "let s () = (mk ()).Q <- 1"
                        ]

                let tast = analyse probe
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.contains (writes tast).Calls "set_Q" "the write dispatches through the accessor method"
            }

            test "a write-only property takes a three-segment object argument" {
                let probe =
                    String.concat
                        "\n"
                        [
                            "type C() ="
                            "    let mutable q = 0"
                            "    member this.Q with set (w: int) = q <- w"
                            "type D() ="
                            "    member val Inner = C() with get"
                            "let s (d: D) = d.Inner.Q <- 1"
                        ]

                let tast = analyse probe
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.contains (writes tast).Calls "set_Q" "the write dispatches through the accessor method"
            }

            // A range has no first-class value, so its result type is a sentinel that unifies
            // with the binding's expected type. The sentinel must not be mistaken for a type
            // name a contract failed to resolve.
            test "a range in value position reports only that a range has no first-class value" {
                let tast = analyse "let x = 1..10"
                let kinds = tast.Diagnostics |> Seq.map (fun d -> d.Kind) |> Seq.toList
                Expect.equal kinds [ Kind.RangeNotFirstClassValue ] "one diagnostic, the range one"
            }
        ]

[<Tests>]
let stringEscapeTests =
    testList
        "StringEscapes"
        [
            test "ordinary escapes round-trip through a string literal" {
                let tast = analyse "let s = \"a\\tb\\u0041\\x41\\065\\U0001F600\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\tbAAA\U0001F600" "escapes decode, the astral \\U to a surrogate pair"
                | other -> failtestf "expected a string const let, got %A" other
            }

            test "an interpolated string with no hole decodes its escapes" {
                let tast = analyse "let s = $\"a\\tb\\u0041\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\tbA" "the interpolated path decodes as the plain one does"
                | other -> failtestf "expected a string const let, got %A" other
            }

            test "an interpolated string with a hole decodes the escapes in its literal runs" {
                let tast = analyse "let x = 42\nlet s = $\"a\\tb{x}c\\nd\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls |> EqArray.last with
                | TDecl.Let(_, TExpr.Format(_, segs, _, _), _, _) ->
                    let lits =
                        EqArray.toList segs
                        |> List.choose (
                            function
                            | FormatSeg.Lit text -> Some text
                            | _ -> None
                        )

                    Expect.equal lits [ "a\tb"; "c\nd" ] "each literal run carries decoded text"
                | other -> failtestf "expected a Format let, got %A" other
            }

            test "a verbatim interpolated string keeps its backslashes" {
                let tast = analyse "let s = $@\"a\\tb\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\\tb" "a verbatim string decodes no escape"
                | other -> failtestf "expected a string const let, got %A" other
            }

            test "a triple-quoted interpolated string keeps its backslashes" {
                let tast = analyse "let s = $\"\"\"a\\tb\"\"\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\\tb" "a triple-quoted string decodes no escape"
                | other -> failtestf "expected a string const let, got %A" other
            }

            // fsi: `@"a""b"` is `a"b`, `$@"a""b"` is `a"b`, `$@"a""b{x}c"` is `a"b1c`.
            test "a verbatim string collapses its doubled quote" {
                let tast = analyse "let s = @\"a\"\"b\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\"b" "the doubled quote is one quote"
                | other -> failtestf "expected a string const let, got %A" other
            }

            // `%%` is a printf escape, not a string escape: fsi renders `"100%%"` as
            // `100%%`, and the runtime format engine is what collapses it in a format body.
            test "a plain string keeps both characters of `%%`" {
                let tast = analyse "let s = @\"a\"\"b%%c\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\"b%%c" "the doubled quote collapses, `%%` does not"
                | other -> failtestf "expected a string const let, got %A" other
            }

            test "a verbatim interpolated string with no hole collapses its doubled quote" {
                let tast = analyse "let s = $@\"a\"\"b\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "a\"b" "the doubled quote is one quote"
                | other -> failtestf "expected a string const let, got %A" other
            }

            test "a verbatim interpolated string collapses the doubled quote in its literal runs" {
                let tast = analyse "let x = 1\nlet s = $@\"a\"\"b{x}c\"\"d\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls |> EqArray.last with
                | TDecl.Let(_, TExpr.Format(_, segs, _, _), _, _) ->
                    let lits =
                        EqArray.toList segs
                        |> List.choose (
                            function
                            | FormatSeg.Lit text -> Some text
                            | _ -> None
                        )

                    Expect.equal lits [ "a\"b"; "c\"d" ] "each literal run collapses its doubled quote"
                | other -> failtestf "expected a Format let, got %A" other
            }

            test "an unknown and a truncated escape stay verbatim, no diagnostics" {
                let tast = analyse "let s = \"\\q \\u12\""
                Expect.isEmpty tast.Diagnostics "fsc keeps both verbatim without a warning"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.String s, _, _), _, _) ->
                    Expect.equal s "\\q \\u12" "raw text, backslashes included"
                | other -> failtestf "expected a string const let, got %A" other
            }

            // An explicitly written unit argument is a VALUE, so it reaches a constructor as one
            // argument: fsc reports FS0501 ("takes 0 argument(s) but is here given 1") for
            // `P2 (())` and `P2((()))` against `new()`. `Expr.App` and `HighPrecedenceApp` peel
            // through the same function, so the two call shapes give the same arity.
            test "an explicit unit argument reaches a constructor as one argument, on both call shapes" {
                let newArgCount (call: string) =
                    let src = sprintf "type P(u: unit) =\n    member this.X = 0\nlet a = %s" call

                    match (analyse src).Decls |> EqArray.last with
                    | TDecl.Let(_, TExpr.New(args = args), _, _) -> args.Length
                    | other -> failtestf "expected a trailing `let _ = New(…)`, got %A" other

                // `P (())` shapes as `Expr.App`; `P((()))` as `HighPrecedenceApp`, whose own
                // parentheses the parser has already taken into `lParen`/`rParen`.
                Expect.equal (newArgCount "P (())") 1 "`P (())` is one unit argument"
                Expect.equal (newArgCount "P((()))") 1 "`P((()))` is one unit argument"
            }

            test "`P()` still reaches a nullary constructor with no arguments" {
                for call in [ "P()"; "P ()" ] do
                    let tast = analyse (sprintf "type P() =\n    member this.X = 0\nlet a = %s" call)

                    Expect.isEmpty (tast.Diagnostics |> List.filter Diagnostic.isError) "nullary call is admitted"

                    match tast.Decls |> EqArray.last with
                    | TDecl.Let(_, TExpr.New(args = args), _, _) ->
                        Expect.isEmpty args (sprintf "`%s` peels to no arguments" call)
                    | other -> failtestf "expected a trailing `let _ = New(…)`, got %A" other
            }

            test "a decimal trigraph above 255 is a hard ERROR" {
                let tast = analyse "let s = \"\\256\""
                let kinds = tast.Diagnostics |> Seq.map (fun d -> d.Kind) |> Seq.toList

                Expect.equal kinds [ Kind.EscapeTrigraphOutOfRange "\\256" ] "one diagnostic, the trigraph one"
            }

            test "a \\U beyond the Unicode scalar range is a hard ERROR" {
                let tast = analyse "let s = \"\\U00110000\""
                let kinds = tast.Diagnostics |> Seq.map (fun d -> d.Kind) |> Seq.toList

                Expect.equal kinds [ Kind.EscapeNotUnicodeScalar "\\U00110000" ] "one diagnostic, fsc's FS1245"
            }

            test "a trigraph above 255 in an enum case value reports at the escape token" {
                let tast = analyse "type E = | A = \"\\256\""

                let hasTrigraph =
                    tast.Diagnostics
                    |> List.exists (fun d -> d.Kind = Kind.EscapeTrigraphOutOfRange "\\256")

                Expect.isTrue hasTrigraph "the enum-case value projection surfaces the verdict"
            }
        ]

// Every surfaced `TDecl.Type` records its declaration's `access` token in `Accessibility`,
// where an absent key reads as `Public`.
[<Tests>]
let typeAccessibilityTests =
    /// The `Accessibility` entry recorded for the surfaced type named `name`.
    let accessOf (src: string) (name: string) : Accessibility voption =
        let tast = analyse src

        let typeKey =
            tast.Decls
            |> EqArray.toList
            |> List.tryPick (
                function
                | TDecl.Type td when td.Name = name -> Some td.TypeKey
                | _ -> None
            )

        match typeKey with
        | None -> failtestf "no TDecl.Type named '%s'; decls: %A" name tast.Decls
        | Some key -> EqDict.tryFind (SymbolKey.Type key) tast.Accessibility

    testList
        "TypeAccessibility"
        [
            test "`type private R = { … }` records Private" {
                Expect.equal
                    (accessOf "type private R = { x: int }" "R")
                    (ValueSome Accessibility.Private)
                    "the record's access token reaches the table"
            }

            test "`type private E = | A = 1` records Private" {
                Expect.equal
                    (accessOf "type private E = | A = 1" "E")
                    (ValueSome Accessibility.Private)
                    "the enum's access token reaches the table"
            }

            ptest "GAP: `type D = delegate of int -> int` surfaces no decl, so `private` on it records Private" {
                Expect.equal
                    (accessOf "type private D = delegate of int -> int" "D")
                    (ValueSome Accessibility.Private)
                    "the delegate's access token reaches the table"
            }

            ptest "GAP: `type S = struct … end` surfaces no decl, so `private` on it records Private" {
                Expect.equal
                    (accessOf "type private S =\n    struct\n        val mutable x: int\n    end" "S")
                    (ValueSome Accessibility.Private)
                    "the struct's access token reaches the table"
            }
        ]

[<Tests>]
let tastFileEqualityTests =
    // Populates all four collection fields: an intrinsic abbreviation, a `[<Global>]` binding,
    // a nested module and a `private` binding.
    let src =
        "module M\n\ntype nat = (# \"number\" #)\n\n[<Global>]\nlet undefined = (# \"undefined\" #)\n\nlet private hidden = 1\n\nmodule Inner =\n    let y = 2\n"

    testList
        "TastFile equality"
        [
            test "two elaborations of the same source are `=`" {
                let a = analyse src
                let b = analyse src

                Expect.isFalse a.IntrinsicBindings.IsEmpty "the source declares an intrinsic"
                Expect.isFalse a.GlobalValueKeys.IsEmpty "the source declares a global"
                Expect.isFalse a.ModuleSourcePaths.IsEmpty "the source declares a module"
                Expect.isFalse a.Accessibility.IsEmpty "the source declares a private binding"

                Expect.equal a b "separately built files with the same content are equal"
            }

            test "a dropped collection entry makes two files unequal" {
                let a = analyse src

                Expect.notEqual
                    a
                    { a with GlobalValueKeys = EqSet.empty }
                    "the collection fields reach the derived equality"
            }
        ]
