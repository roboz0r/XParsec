module XParsec.FSharp.SemanticAnalysis.Tests.FreezeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

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
                    Expect.equal ty BuiltinTypes.tyInt "value type"
                    Expect.equal letTy BuiltinTypes.tyInt "binding type"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let b = true` -> TConst Bool true" {
                let tast = analyse "let b = true"

                match tast.Decls.[0] with
                | TDecl.Let(_, TExpr.Const(TConstValue.Bool true, ty), _, _) ->
                    Expect.equal ty BuiltinTypes.tyBool "value type bool"
                | other -> failtestf "unexpected: %A" other
            }

            test "`let f = fun x -> x + 1` -> Lambda over App chain with External operator" {
                let tast = analyse "let f = fun x -> x + 1"
                let intTy = BuiltinTypes.tyInt
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
                | TDecl.Let(TPat.NamedSimple(bindingKey, _), _, _, _) -> Expect.equal bindingKey expected "binding key"
                | other -> failtestf "unexpected: %A" other
            }

            test "TVar references the original headPat NodeKey" {
                // y's RHS references x's binding key.
                let tast = analyse "let x = 1\nlet y = x"
                let xKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match tast.Decls with
                | EqList [ _; TDecl.Let(_, TExpr.Var(refKey, _), _, _) ] -> Expect.equal refKey xKey "y refs x"
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
                                            EqList [ TExpr.Const(TConstValue.Int 1, _)
                                                     TExpr.UnionCons("Cons",
                                                                     EqList [ TExpr.Const(TConstValue.Int 2, _)
                                                                              TExpr.UnionCons("Cons",
                                                                                              EqList [ TExpr.Const(TConstValue.Int 3,
                                                                                                                   _)
                                                                                                       TExpr.UnionCons("Nil",
                                                                                                                       EqList [],
                                                                                                                       _) ],
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
                | TDecl.Let(_, TExpr.UnionCons("Nil", EqList [], ty), _, _) ->
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

                let arrayTy = TyConst(RuntimeNames.arrayName 1, EqArray.singleton intTy)

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) arrayTy "xs : int[]"

                match tast.Decls.[0] with
                | TDecl.Let(_,
                            TExpr.App(TExpr.External("Microsoft.FSharp.Collections.ArrayModule.OfList", _, opTy),
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

// Every pass — not just Freeze — walks `namespace`-headed files, so
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
                | TDecl.Let(_, TExpr.Const(TConstValue.Int 1, ty), _, letTy) ->
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

// Every pass + Freeze descend into a nested `module Foo = …`. Its body is
// flattened to the enclosing scope (v1 has no module-scoped types), the same
// simplification `CstWalk.implFileElems` applies to namespace groups. Earlier
// the analysis passes never descended into `ModuleElem.Module` (Validation
// `failwith`'d on it) and Freeze dropped the body, so a `let` inside a nested
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
                | TDecl.Let(_, TExpr.Const(TConstValue.Int 1, ty), _, letTy) ->
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
                | EqList [ TDecl.Type td ] ->
                    Expect.equal td.Name "Fun" "type name"
                    Expect.equal td.Namespace (Some "Vesper") "namespace"
                    Expect.equal (EqArray.toList td.TypeParams) [ "'A"; "'B" ] "declared typars"

                    match td.Kind with
                    | TTypeKind.Interface(EqList [ m ]) ->
                        Expect.equal m.Name "Invoke" "method name"
                        Expect.isTrue m.MethodTypeParams.IsEmpty "Invoke has no method typars"
                        // 'A -> 'B, declaring typars as frozen `TempTypar(Declaring, i)`.
                        Expect.equal
                            m.Signature
                            (TyFun(TempTypar(TyparAxis.Declaring, 0), TempTypar(TyparAxis.Declaring, 1)))
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
                            (TyFun(TempTypar(TyparAxis.Declaring, 0), TempTypar(TyparAxis.Method, 0)))
                            "Map signature 'A -> 'B"
                    | other -> failtestf "expected one interface method, got %A" other
                | other -> failtestf "expected single TDecl.Type, got %A" other
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
                | TTypeKind.Union(cs, _) -> acc.Add(td, cs)
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
                        Expect.equal ht (TempTypar(TyparAxis.Declaring, 0)) "Head : 'T"
                        Expect.equal tn (ValueSome "Tail") "second field named Tail"
                        // Tail refers back to the declaring union, applied to 'T.
                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TempTypar(TyparAxis.Declaring, 0))))
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
                        Expect.equal ht (TempTypar(TyparAxis.Declaring, 0)) "Head : 'T"
                        // `'T list` resolved through the abbrev back to the union.
                        Expect.equal
                            tt
                            (TyUnion("List", EqArray.singleton (TempTypar(TyparAxis.Declaring, 0))))
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
                        (TyUnion("List", EqArray.singleton (TyConst("int", EqArray.empty))))
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
                    Expect.equal ty (TyUnion("List", EqArray.singleton (TyConst("int", EqArray.empty)))) "e : List<int>"
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
                            EqArray.singleton (TyConst("int", EqArray.empty))
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
                        | TDecl.Type { Kind = TTypeKind.Union(_, ms) } -> ValueSome(EqArray.toList ms)
                        | _ -> ValueNone
                    )

                match members with
                | ValueSome ms ->
                    let find n =
                        ms |> List.find (fun (m: TTypeMember) -> m.Name = n)

                    let isEmpty = find "IsEmpty"
                    Expect.isFalse isEmpty.IsStatic "IsEmpty is an instance member"
                    Expect.equal isEmpty.Kind TMemberKind.Property "IsEmpty is a property"
                    Expect.equal isEmpty.ReturnTy (TyConst("bool", EqArray.empty)) "IsEmpty : bool"
                    Expect.isTrue (ValueOption.isSome isEmpty.ThisKey) "an instance member carries a `this` binder"

                    Expect.equal (find "Head").ReturnTy (TyConst("int", EqArray.empty)) "Head : int"

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
