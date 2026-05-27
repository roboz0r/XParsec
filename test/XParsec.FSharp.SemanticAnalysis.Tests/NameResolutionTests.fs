module XParsec.FSharp.SemanticAnalysis.Tests.NameResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx

[<Tests>]
let tests =
    testList
        "NameResolution"
        [
            test "let-in: body ident resolves to local binding" {
                // body x at offset 13, binding x at offset 4.
                let ctx = analyse "let x = 1 in x"
                let bodyKey = NodeKey.ofSource 13 NodeKind.ExprIdent
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite bindingKey "binding site"
                | ValueNone -> failtest "body ident not resolved"
            }

            test "lambda parameter is in scope for body" {
                // param x at offset 13, body x at offset 18.
                let ctx = analyse "let id = fun x -> x"
                let bodyKey = NodeKey.ofSource 18 NodeKind.ExprIdent
                let paramKey = NodeKey.ofSource 13 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite paramKey "binding site"
                | ValueNone -> failtest "lambda body ident not resolved"
            }

            test "function-form let: argument is in scope for body" {
                // arg x at offset 6, body x at offset 10.
                let ctx = analyse "let f x = x"
                let bodyKey = NodeKey.ofSource 10 NodeKind.ExprIdent
                let argKey = NodeKey.ofSource 6 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite argKey "binding site"
                | ValueNone -> failtest "fn-form arg not resolved"
            }

            test "shadowing: inner binding wins" {
                // inner binding x at offset 17 (body uses it, not the outer).
                let ctx = analyse "let x = 1 in let x = 2 in x"
                let bodyKey = NodeKey.ofSource 26 NodeKind.ExprIdent
                let innerBindingKey = NodeKey.ofSource 17 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite innerBindingKey "resolves to inner x"
                | ValueNone -> failtest "body x not resolved"
            }

            test "module-level let visible to next module element" {
                let ctx = analyse "let x = 1\nlet y = x"
                // use x at 18: 10-char first line + "let y = ".
                let useKey = NodeKey.ofSource 18 NodeKind.ExprIdent
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue useKey with
                | ValueSome rb -> Expect.equal rb.BindingSite bindingKey "binding site"
                | ValueNone -> failtest "use not resolved"
            }

            test "unresolved name emits a diagnostic" {
                let ctx = analyse "let x = undefined"
                Expect.isGreaterThanOrEqual ctx.Diagnostics.Count 1 "at least one diagnostic"

                let hasUnresolved =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

                Expect.isTrue hasUnresolved "Unresolved diagnostic emitted"
            }

            test "external symbol from provider does not emit diagnostic" {
                // `true` resolves through the provider → no diagnostic, no Binding entry (external).
                let ctx = analyse "let x = true"
                let useKey = NodeKey.ofSource 8 NodeKind.ExprIdent
                Expect.equal ctx.Diagnostics.Count 0 "no diagnostics for known external"

                Expect.isTrue
                    (ctx.Bindings.Binding.TryGetValue useKey = ValueNone)
                    "no Binding entry for external symbol"
            }

            test "let mutable: binding-site IsMutable is true" {
                // pat n at offset 12.
                let ctx = analyse "let mutable n = 0"
                let bindingKey = NodeKey.ofSource 12 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bindingKey with
                | ValueSome rb -> Expect.isTrue rb.IsMutable "binding-site entry has IsMutable = true"
                | ValueNone -> failtest "no binding-site self-entry for `n`"
            }

            test "let (no mutable): binding-site IsMutable is false" {
                let ctx = analyse "let n = 0"
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Bindings.Binding.TryGetValue bindingKey with
                | ValueSome rb -> Expect.isFalse rb.IsMutable "binding-site entry has IsMutable = false"
                | ValueNone -> failtest "no binding-site self-entry for `n`"
            }

            test "use of a mutable binding: use-site IsMutable mirrors binding" {
                // use n at offset 21.
                let ctx = analyse "let mutable n = 0 in n"
                let useKey = NodeKey.ofSource 21 NodeKind.ExprIdent

                match ctx.Bindings.Binding.TryGetValue useKey with
                | ValueSome rb -> Expect.isTrue rb.IsMutable "use-site IsMutable propagated from binding"
                | ValueNone -> failtest "use of `n` not resolved"
            }

            test "record type definition registers in ctx.Types.Record" {
                let ctx = analyse "type R = { X: int; Y: int }"

                match ctx.Types.Record.TryGetValue "R" with
                | true, info ->
                    Expect.equal info.Fields.Length 2 "two fields"
                    Expect.equal info.Fields.[0].Name "X" "first field is X"
                    Expect.equal info.Fields.[1].Name "Y" "second field is Y"
                | false, _ -> failtest "record type R not registered"
            }

            test "record field index is built" {
                let ctx = analyse "type R = { X: int; Y: int }"

                match ctx.Types.FieldIndex.TryGetValue "X" with
                | true, infos -> Expect.equal infos.Length 1 "X referenced by exactly one type"
                | false, _ -> failtest "X not in FieldIndex"

                match ctx.Types.FieldIndex.TryGetValue "Y" with
                | true, infos -> Expect.equal infos.Length 1 "Y referenced by exactly one type"
                | false, _ -> failtest "Y not in FieldIndex"
            }

            test "duplicate record type name diagnoses" {
                let ctx = analyse "type R = { X: int }\ntype R = { Y: int }"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate type definition")

                Expect.isTrue hasDup "duplicate-type diagnostic emitted"
            }

            test "mutable field IsMutable is true" {
                let ctx = analyse "type P = { X: int; mutable Y: int }"

                match ctx.Types.Record.TryGetValue "P" with
                | true, info ->
                    Expect.isFalse info.Fields.[0].IsMutable "X is immutable"
                    Expect.isTrue info.Fields.[1].IsMutable "Y is mutable"
                | false, _ -> failtest "record type P not registered"
            }

            test "union type definition registers in ctx.Types.Union" {
                let ctx =
                    analyse "type S =\n    | Circle of float\n    | Rectangle of float * float\n    | Point"

                match ctx.Types.Union.TryGetValue "S" with
                | true, info ->
                    Expect.equal info.Cases.Length 3 "three cases"
                    Expect.equal info.Cases.[0].Name "Circle" "Circle case"
                    Expect.equal info.Cases.[1].Name "Rectangle" "Rectangle case"
                    Expect.equal info.Cases.[2].Name "Point" "Point nullary case"
                    Expect.equal info.Cases.[0].Fields.Length 1 "Circle: 1 field"
                    Expect.equal info.Cases.[1].Fields.Length 2 "Rectangle: 2 fields"
                    Expect.equal info.Cases.[2].Fields.Length 0 "Point: nullary"
                | false, _ -> failtest "union type S not registered"
            }

            test "duplicate union type name diagnoses" {
                let ctx = analyse "type S = | A\ntype S = | B"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate type definition")

                Expect.isTrue hasDup "duplicate-type diagnostic emitted"
            }

            test "CtorIndex maps ctor name to declaring union" {
                let ctx = analyse "type S =\n    | Circle of float\n    | Point"

                match ctx.Types.CtorIndex.TryGetValue "Circle" with
                | true, infos ->
                    Expect.equal infos.Length 1 "Circle declared by exactly one union"
                    Expect.equal infos.Head.UnionName "S" "Circle belongs to S"
                | false, _ -> failtest "Circle not in CtorIndex"

                match ctx.Types.CtorIndex.TryGetValue "Point" with
                | true, infos -> Expect.equal infos.Length 1 "Point declared by exactly one union"
                | false, _ -> failtest "Point not in CtorIndex"
            }

            test "nullary ctor in pattern binds nothing" {
                // `Point` is a known ctor, not a binder, so its pattern ident must
                // have no self-binding entry.
                let ctx = analyse "type S = | Point\nmatch 0 with | Point -> 0 | _ -> 0"

                // Point ident at 32: 17-char type decl + "match 0 with | ".
                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                let hasBinding = ctx.Bindings.Binding.ContainsKey patKey
                Expect.isFalse hasBinding "Point pattern should not be a binding site"
            }

            test "bare ctor reference does not emit Unresolved diagnostic" {
                let ctx = analyse "type S = | Point\nlet p = Point"

                let hasUnresolved =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

                Expect.isFalse hasUnresolved "bare ctor name not flagged as unresolved"
            }

            test "generic record registers single TypeParam" {
                let ctx = analyse "type Box<'a> = { Value: 'a }"

                match ctx.Types.Record.TryGetValue "Box" with
                | true, info ->
                    Expect.equal (List.length info.TypeParams) 1 "one typar"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "name is 'a"
                | false, _ -> failtest "Box not registered"
            }

            test "generic record's field type shares typar identity" {
                // `Value : 'a` — after Unification fills field types, the
                // field's placeholder TyVar links onto the same TyVar root
                // that's stored in TypeParams.
                let input = "type Box<'a> = { Value: 'a }"
                let lexed, file = parseFile input
                let ctx = PassContext(MockBuiltins.provider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                match ctx.Types.Record.TryGetValue "Box" with
                | true, info ->
                    let _, tparTv = info.TypeParams.[0]
                    let tparRoot = UnionFind.find tparTv
                    let fieldTy = Unification.zonk info.Fields.[0].Type

                    match fieldTy with
                    | TyVar fieldTv ->
                        let fieldRoot = UnionFind.find fieldTv
                        Expect.isTrue (System.Object.ReferenceEquals(tparRoot, fieldRoot)) "field shares typar root"
                    | other -> failtestf "expected TyVar, got %A" other
                | false, _ -> failtest "Box not registered"
            }

            test "generic record keeps declaration order" {
                let ctx = analyse "type Pair<'a, 'b> = { First: 'a; Second: 'b }"

                match ctx.Types.Record.TryGetValue "Pair" with
                | true, info ->
                    Expect.equal (List.length info.TypeParams) 2 "two typars"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "first is 'a"
                    Expect.equal (fst info.TypeParams.[1]) "'b" "second is 'b"
                | false, _ -> failtest "Pair not registered"
            }

            test "generic union registers TypeParams" {
                let ctx = analyse "type Option<'a> = | Some of 'a | None"

                match ctx.Types.Union.TryGetValue "Option" with
                | true, info ->
                    Expect.equal (List.length info.TypeParams) 1 "one typar"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "name is 'a"
                | false, _ -> failtest "Option not registered"
            }

            test "implicit free typar in type-def diagnoses" {
                // `type Bad = { X: 'a }` with no `<'a>` defn — Unification's
                // strict-mode walk over field types should fire a
                // "Free type parameter" diagnostic.
                let lexed, file = parseFile "type Bad = { X: 'a }"
                let ctx = PassContext(MockBuiltins.provider, "type Bad = { X: 'a }", lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let hasFree =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Free type parameter")

                Expect.isTrue hasFree "implicit free typar diagnosed"
            }

            test "monomorphic abbreviation registers with no TypeParams" {
                let ctx = analyse "type Name = string"

                match ctx.Types.Abbreviation.TryGetValue "Name" with
                | true, info -> Expect.isTrue info.TypeParams.IsEmpty "no typars"
                | false, _ -> failtest "abbreviation Name not registered"
            }

            test "generic abbreviation keeps declaration order" {
                let ctx = analyse "type Pair<'a, 'b> = 'a * 'b"

                match ctx.Types.Abbreviation.TryGetValue "Pair" with
                | true, info ->
                    Expect.equal (List.length info.TypeParams) 2 "two typars"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "first is 'a"
                    Expect.equal (fst info.TypeParams.[1]) "'b" "second is 'b"
                | false, _ -> failtest "abbreviation Pair not registered"
            }

            test "duplicate abbreviation name diagnoses" {
                let ctx = analyse "type Foo = int\ntype Foo = bool"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate type definition")

                Expect.isTrue hasDup "duplicate-type diagnostic emitted"
            }

            test "abbreviation vs record same name diagnoses" {
                let ctx = analyse "type R = { X: int }\ntype R = int"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate type definition")

                Expect.isTrue hasDup "duplicate-type diagnostic emitted"
            }

            test "class type registers in ctx.Types.Class with ctor params and members" {
                let ctx = analyse "type C(x: int) =\n    member this.X = x"

                match ctx.Types.Class.TryGetValue "C" with
                | true, info ->
                    Expect.equal info.CtorParams.Length 1 "one ctor param"
                    Expect.equal info.CtorParams.[0].Name "x" "ctor param named x"
                    Expect.equal info.Members.Length 1 "one member"
                    Expect.equal info.Members.[0].Name "X" "member named X"
                    Expect.equal info.Members.[0].Kind ClassMemberKind.Property "member is a property"
                | false, _ -> failtest "class type C not registered"
            }

            test "class duplicate type name diagnoses against record" {
                let ctx = analyse "type C = { X: int }\ntype C() = class end"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate type definition")

                Expect.isTrue hasDup "duplicate-type diagnostic emitted"
            }

            test "ClassMemberIndex maps member name to declaring class" {
                let ctx = analyse "type C() =\n    member this.M () = 1"

                match ctx.Types.ClassMemberIndex.TryGetValue "M" with
                | true, lst -> Expect.equal lst.Length 1 "one class declares M"
                | false, _ -> failtest "M not in ClassMemberIndex"
            }

            test "static member registers with IsStatic = true" {
                let ctx = analyse "type C() =\n    static member M () = 1"

                match ctx.Types.Class.TryGetValue "C" with
                | true, info ->
                    Expect.equal info.Members.Length 1 "one member"
                    Expect.isTrue info.Members.[0].IsStatic "M is static"
                    Expect.equal info.Members.[0].Name "M" "member named M"
                | false, _ -> failtest "class type C not registered"
            }
        ]
