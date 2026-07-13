module XParsec.FSharp.SemanticAnalysis.Tests.NameResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(realProvider.Value, input, lexed)
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

                match TypeRegistry.tryRecord ctx.Types "R" with
                | ValueSome info ->
                    Expect.equal info.Fields.Length 2 "two fields"
                    Expect.equal info.Fields.[0].Name "X" "first field is X"
                    Expect.equal info.Fields.[1].Name "Y" "second field is Y"
                | ValueNone -> failtest "record type R not registered"
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

                match TypeRegistry.tryRecord ctx.Types "P" with
                | ValueSome info ->
                    Expect.isFalse info.Fields.[0].IsMutable "X is immutable"
                    Expect.isTrue info.Fields.[1].IsMutable "Y is mutable"
                | ValueNone -> failtest "record type P not registered"
            }

            test "union type definition registers in ctx.Types.Union" {
                let ctx =
                    analyse "type S =\n    | Circle of float\n    | Rectangle of float * float\n    | Point"

                match TypeRegistry.tryUnionBare ctx.Types "S" with
                | ValueSome info ->
                    Expect.equal info.Cases.Length 3 "three cases"
                    Expect.equal info.Cases.[0].Name "Circle" "Circle case"
                    Expect.equal info.Cases.[1].Name "Rectangle" "Rectangle case"
                    Expect.equal info.Cases.[2].Name "Point" "Point nullary case"
                    Expect.equal info.Cases.[0].Fields.Length 1 "Circle: 1 field"
                    Expect.equal info.Cases.[1].Fields.Length 2 "Rectangle: 2 fields"
                    Expect.equal info.Cases.[2].Fields.Length 0 "Point: nullary"
                | ValueNone -> failtest "union type S not registered"
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
                    Expect.equal infos.[0].UnionName "S" "Circle belongs to S"
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

                match TypeRegistry.tryRecord ctx.Types "Box" with
                | ValueSome info ->
                    Expect.equal info.TypeParams.Length 1 "one typar"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "name is 'a"
                | ValueNone -> failtest "Box not registered"
            }

            test "generic record's field type shares typar identity" {
                // `Value : 'a` — after Unification fills field types, the
                // field's placeholder TyVar links onto the same TyVar root
                // that's stored in TypeParams.
                let input = "type Box<'a> = { Value: 'a }"
                let lexed, file = parseFile input
                let ctx = PassContext(realProvider.Value, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                match TypeRegistry.tryRecord ctx.Types "Box" with
                | ValueSome info ->
                    let _, tparTv = info.TypeParams.[0]
                    let tparRoot = UnionFind.find tparTv
                    let fieldTy = Unification.zonk info.Fields.[0].Type

                    match fieldTy with
                    | TyVar fieldTv ->
                        let fieldRoot = UnionFind.find fieldTv
                        Expect.isTrue (System.Object.ReferenceEquals(tparRoot, fieldRoot)) "field shares typar root"
                    | other -> failtestf "expected TyVar, got %A" other
                | ValueNone -> failtest "Box not registered"
            }

            test "generic record keeps declaration order" {
                let ctx = analyse "type Pair<'a, 'b> = { First: 'a; Second: 'b }"

                match TypeRegistry.tryRecord ctx.Types "Pair" with
                | ValueSome info ->
                    Expect.equal info.TypeParams.Length 2 "two typars"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "first is 'a"
                    Expect.equal (fst info.TypeParams.[1]) "'b" "second is 'b"
                | ValueNone -> failtest "Pair not registered"
            }

            test "generic union registers TypeParams" {
                let ctx = analyse "type Option<'a> = | Some of 'a | None"

                match TypeRegistry.tryUnionBare ctx.Types "Option" with
                | ValueSome info ->
                    Expect.equal info.TypeParams.Length 1 "one typar"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "name is 'a"
                | ValueNone -> failtest "Option not registered"
            }

            test "implicit free typar in type-def diagnoses" {
                // `type Bad = { X: 'a }` with no `<'a>` defn — Unification's
                // strict-mode walk over field types should fire a
                // "Free type parameter" diagnostic.
                let lexed, file = parseFile "type Bad = { X: 'a }"
                let ctx = PassContext(realProvider.Value, "type Bad = { X: 'a }", lexed)
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

                match TypeRegistry.tryAbbrevArity ctx.Types "Name" 0 with
                | ValueSome info -> Expect.isTrue info.TypeParams.IsEmpty "no typars"
                | ValueNone -> failtest "abbreviation Name not registered"
            }

            test "generic abbreviation keeps declaration order" {
                let ctx = analyse "type Pair<'a, 'b> = 'a * 'b"

                match TypeRegistry.tryAbbrevArity ctx.Types "Pair" 2 with
                | ValueSome info ->
                    Expect.equal info.TypeParams.Length 2 "two typars"
                    Expect.equal (fst info.TypeParams.[0]) "'a" "first is 'a"
                    Expect.equal (fst info.TypeParams.[1]) "'b" "second is 'b"
                | ValueNone -> failtest "abbreviation Pair not registered"
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

            test "Phase 1: stamped *TypeInfo.Key is the arity-qualified TypeKey(None, \"\", name)" {
                // Every registered type carries a project-local `SymbolKey`.
                // Non-generic types key on the bare name; generic types carry the
                // `` `N `` arity suffix that matches the emitted metadata name and
                // codegen's `userTypes` keying (`SymbolKeyOps.arityName`).
                let ctx =
                    analyse (
                        "type R = { X: int }\n"
                        + "type Box<'a> = { Value: 'a }\n"
                        + "type Color = | Red | Green\n"
                        + "type Choice<'a, 'b> = | C1 of 'a | C2 of 'b\n"
                        + "type Pair<'a, 'b> = 'a * 'b\n"
                        + "type Name = string"
                    )

                let recordKey n =
                    match TypeRegistry.tryRecord ctx.Types n with
                    | ValueSome info -> info.Key
                    | ValueNone -> failtestf "record %s not registered" n

                let unionKey n =
                    match TypeRegistry.tryUnionBare ctx.Types n with
                    | ValueSome info -> info.Key
                    | ValueNone -> failtestf "union %s not registered" n

                let abbrevKey n arity =
                    match TypeRegistry.tryAbbrevArity ctx.Types n arity with
                    | ValueSome info -> info.Key
                    | ValueNone -> failtestf "abbreviation %s not registered" n

                Expect.equal (recordKey "R") (SymbolKeyOps.typeKey None "" ("R")) "non-generic record → bare key"

                Expect.equal
                    (recordKey "Box")
                    (SymbolKeyOps.typeKey None "" ("Box`1"))
                    "generic record → arity-suffixed key"

                Expect.equal (unionKey "Color") (SymbolKeyOps.typeKey None "" ("Color")) "non-generic union → bare key"

                Expect.equal
                    (unionKey "Choice")
                    (SymbolKeyOps.typeKey None "" ("Choice`2"))
                    "generic union → arity-suffixed key"

                Expect.equal
                    (abbrevKey "Pair" 2)
                    (SymbolKeyOps.typeKey None "" ("Pair`2"))
                    "generic abbrev → arity-suffixed key"

                Expect.equal
                    (abbrevKey "Name" 0)
                    (SymbolKeyOps.typeKey None "" ("Name"))
                    "non-generic abbrev → bare key"

                // The key's name component is exactly what codegen keys `userTypes` on.
                Expect.equal
                    (unionKey "Choice")
                    (SymbolKeyOps.typeKey None "" (SymbolKeyOps.arityName "Choice" 2))
                    "union key name matches SymbolKeyOps.arityName"
            }

            test "Phase 1: generic class Key is arity-suffixed" {
                let ctx = analyse "type C<'a>(x: 'a) =\n    member this.X = x"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.equal info.Key (SymbolKeyOps.typeKey None "" ("C`1")) "generic class → arity-suffixed key"
                | ValueNone -> failtest "class C not registered"
            }

            test "class type registers in ctx.Types.Class with ctor params and members" {
                let ctx = analyse "type C(x: int) =\n    member this.X = x"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.equal info.CtorParams.Length 1 "one ctor param"
                    Expect.equal info.CtorParams.[0].Name "x" "ctor param named x"
                    Expect.equal info.Members.Length 1 "one member"
                    Expect.equal info.Members.[0].Name "X" "member named X"
                    Expect.equal info.Members.[0].Kind ClassMemberKind.Property "member is a property"
                | ValueNone -> failtest "class type C not registered"
            }

            test "ClassMemberIndex maps member name to declaring class" {
                let ctx = analyse "type C() =\n    member this.M () = 1"

                match ctx.Types.ClassMemberIndex.TryGetValue "M" with
                | true, lst -> Expect.equal lst.Length 1 "one class declares M"
                | false, _ -> failtest "M not in ClassMemberIndex"
            }

            test "static member registers with IsStatic = true" {
                let ctx = analyse "type C() =\n    static member M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.equal info.Members.Length 1 "one member"
                    Expect.isTrue info.Members.[0].IsStatic "M is static"
                    Expect.equal info.Members.[0].Name "M" "member named M"
                | ValueNone -> failtest "class type C not registered"
            }

            // B-8: `[<Sealed>]` and `[<AllowNullLiteral>]` decode through
            // `Attributes.decodeClassAttributes` onto `ClassTypeInfo`.
            test "[<Sealed>] stamps ClassTypeInfo.IsSealed" {
                let ctx = analyse "[<Sealed>]\ntype C() = member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.isTrue info.IsSealed "[<Sealed>] sets IsSealed"
                    Expect.isFalse info.AllowNullLiteral "AllowNullLiteral not stamped"
                | ValueNone -> failtest "class type C not registered"
            }

            test "[<AllowNullLiteral>] stamps ClassTypeInfo.AllowNullLiteral" {
                let ctx = analyse "[<AllowNullLiteral>]\ntype C() = member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.isTrue info.AllowNullLiteral "[<AllowNullLiteral>] sets AllowNullLiteral"
                    Expect.isFalse info.IsSealed "IsSealed not stamped"
                | ValueNone -> failtest "class type C not registered"
            }

            test "fully-qualified [<Microsoft.FSharp.Core.Sealed>] still stamps IsSealed" {
                // Attribute resolution is by short name (with the `Attribute`
                // suffix optional) — mirrors `decodeEqualityAttributes`.
                let ctx =
                    analyse "[<Microsoft.FSharp.Core.SealedAttribute>]\ntype C() = member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info -> Expect.isTrue info.IsSealed "long-ident [<...Sealed>] is decoded"
                | ValueNone -> failtest "class type C not registered"
            }

            test "no class-shaping attribute leaves IsSealed=false, AllowNullLiteral=false" {
                let ctx = analyse "type C() = member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.isFalse info.IsSealed "default: not sealed"
                    Expect.isFalse info.AllowNullLiteral "default: no null literal"
                | ValueNone -> failtest "class type C not registered"
            }

            // --- Phase 2 / B-4: inheritance registration (Step 2.1) -------------

            test "inheritance recorded on registry" {
                let ctx =
                    analyse "type B(x: int) =\n    member this.X = x\ntype D(y: int) =\n    inherit B(y)"

                match TypeRegistry.tryClass ctx.Types "D" with
                | ValueSome info ->
                    Expect.equal info.BaseType (ValueSome(TyClass("B", EqArray.empty))) "D inherits B"
                    Expect.isTrue info.BaseCtorArgs.IsSome "base-ctor args captured"
                | ValueNone -> failtest "class type D not registered"
            }

            test "class without inherit clause has ValueNone BaseType" {
                let ctx = analyse "type C() =\n    member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info ->
                    Expect.equal info.BaseType ValueNone "no base type"
                    Expect.equal info.BaseCtorArgs ValueNone "no base-ctor args"
                | ValueNone -> failtest "class type C not registered"
            }

            test "generic inherit clause records translated type args" {
                let ctx =
                    analyse
                        "type Box<'a>(v: 'a) =\n    member this.V = v\ntype IntBox(n: int) =\n    inherit Box<int>(n)"

                match TypeRegistry.tryClass ctx.Types "IntBox" with
                | ValueSome info ->
                    Expect.equal
                        info.BaseType
                        (ValueSome(TyClass("Box", EqArray.singleton (TyConst(RuntimeNames.intKey, EqArray.empty)))))
                        "IntBox inherits Box<int>"
                | ValueNone -> failtest "class type IntBox not registered"
            }

            test "base in scope inside derived member body" {
                let ctx =
                    analyse
                        "type B() =\n    member this.M () = 1\ntype D() =\n    inherit B()\n    member this.N () = base.M()"

                match TypeRegistry.tryClass ctx.Types "D" with
                | ValueSome info ->
                    // The `base` binder gets a self-entry in the instance scope.
                    match ctx.Bindings.Binding.TryGetValue info.BaseKey with
                    | ValueSome rb -> Expect.equal rb.BindingSite info.BaseKey "base self-entry"
                    | ValueNone -> failtest "base not bound in instance scope"

                    // Referencing `base.M()` does not produce an unresolved diagnostic.
                    let unresolvedBase =
                        ctx.Diagnostics
                        |> Seq.exists (fun d -> d.Message.Contains "Unresolved" && d.Message.Contains "base")

                    Expect.isFalse unresolvedBase "base resolves inside derived member"
                | ValueNone -> failtest "class type D not registered"
            }

            test "base not in scope without inherit clause diagnoses" {
                let ctx = analyse "type C() =\n    member this.N () = base.M()"

                let unresolvedBase =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Unresolved" && d.Message.Contains "base")

                Expect.isTrue unresolvedBase "base unresolved without inherit"
            }

            test "override member sets IsOverride on registry" {
                let ctx =
                    analyse
                        "type B() =\n    member this.M () = 1\ntype D() =\n    inherit B()\n    override this.M () = 2"

                match TypeRegistry.tryClass ctx.Types "D" with
                | ValueSome info ->
                    Expect.equal info.Members.Length 1 "one member on D"
                    Expect.isTrue info.Members.[0].IsOverride "override flag set"
                | ValueNone -> failtest "class type D not registered"
            }

            test "plain member leaves IsOverride false" {
                let ctx = analyse "type C() =\n    member this.M () = 1"

                match TypeRegistry.tryClass ctx.Types "C" with
                | ValueSome info -> Expect.isFalse info.Members.[0].IsOverride "plain member is not an override"
                | ValueNone -> failtest "class type C not registered"
            }

            test "inheriting from a non-class diagnoses" {
                let ctx = analyse "type R = { X: int }\ntype D() =\n    inherit R()"

                let cannotInherit =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "inherit")

                Expect.isTrue cannotInherit "cannot-inherit-from-non-class diagnostic"

                match TypeRegistry.tryClass ctx.Types "D" with
                | ValueSome info -> Expect.equal info.BaseType ValueNone "BaseType not stamped for non-class parent"
                | ValueNone -> failtest "class type D not registered"
            }

            test "heritable extern class (# class repr #) registers without diagnostic" {
                // The `class`-tagged intrinsic is admitted as a heritable external base
                // (recorded in `HeritableExternBases`), not rejected.
                let ctx = analyse "type Attribute = (# class \"System.Attribute\" #)"

                Expect.equal ctx.Diagnostics.Count 0 "no diagnostic for a class-tagged intrinsic"
                Expect.isTrue (ctx.Types.HeritableExternBases.Contains "Attribute") "recorded as a heritable base"
            }

            test "heritable extern interface (# interface repr #) is rejected (not yet supported)" {
                // `interface`-tagged intrinsics parse (the AST carries the species) but
                // have no emit path: rejected at registration rather than mis-emitted as
                // a class base, and NOT recorded as a heritable base.
                let ctx = analyse "type IFoo = (# interface \"System.IFoo\" #)"

                let rejected =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "interface" && d.Message.Contains "not yet supported")

                Expect.isTrue rejected "extern interface base rejected"
                Expect.isFalse (ctx.Types.HeritableExternBases.Contains "IFoo") "not recorded as a heritable base"
            }

            test "cyclic inheritance diagnoses" {
                let ctx = analyse "type A() =\n    inherit B()\ntype B() =\n    inherit A()"

                let cyclic = ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "cyclic")

                Expect.isTrue cyclic "cyclic-inheritance diagnostic emitted"
            }

            test "Union decl sites stamp ResolvedType with arity-qualified keys" {
                // NameResolution stamps each union's decl-site NodeKey → the union's
                // minted `SymbolKey`; the type-decl emitter (`Elaborate.tryUnionType`)
                // reads it back by key rather than re-deriving `(name, arity)`.
                let ctx =
                    analyse "type Color = | Red | Green\ntype Choice<'a, 'b> = | C1 of 'a | C2 of 'b"

                let stamped = ctx.Resolution.ResolvedType.AsDictionary()

                let hasValue key =
                    stamped |> Seq.exists (fun kv -> kv.Value = key)

                Expect.isTrue (hasValue (SymbolKeyOps.typeKey None "" ("Color"))) "Color decl site stamped"
                Expect.isTrue (hasValue (SymbolKeyOps.typeKey None "" ("Choice`2"))) "Choice`2 decl site stamped"

                // The stamped key round-trips back to the union through the same
                // reader-side seam the emitter uses.
                match TypeRegistry.tryUnionByKey ctx.Types (SymbolKeyOps.typeKey None "" ("Choice`2")) with
                | ValueSome info -> Expect.equal info.Name "Choice" "Choice`2 key resolves to the Choice union"
                | ValueNone -> failtest "Choice`2 key did not resolve via tryUnionByKey"
            }

            test "Union annotation use site stamps ResolvedType" {
                // `translateType` (via Unification) stamps the use-site type-reference
                // NodeKey as the `Choice<int, string>` annotation resolves — the
                // populate half a Phase 3 consumer will key off.
                let input =
                    "type Choice<'a, 'b> = | C1 of 'a | C2 of 'b\nlet f (x: Choice<int, string>) = x"

                let lexed, file = parseFile input
                let ctx = PassContext(realProvider.Value, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let useStamp =
                    ctx.Resolution.ResolvedType.AsDictionary()
                    |> Seq.exists (fun kv ->
                        kv.Key.Kind = NodeKind.TypeGeneric
                        && kv.Value = SymbolKeyOps.typeKey None "" ("Choice`2")
                    )

                Expect.isTrue useStamp "Choice<int, string> annotation stamped at its TypeGeneric use site"
            }

            test "Declaring namespace is threaded into the minted SymbolKey" {
                // A type under `namespace Foo.Bar` mints `TypeKey(None, "Foo.Bar", name\`arity)`
                // — the identity it emits as (`TDecl.Namespace` + arity-suffixed metadata
                // name) — not the ns="" construction-time placeholder. Proves the
                // declaring path actually survives the module-tree flatten (which used to
                // drop it) and reaches the registry mint.
                let ctx =
                    analyse "namespace Foo.Bar\n\ntype Rec = { x: int }\ntype Choice<'a, 'b> = | C1 of 'a | C2 of 'b"

                match TypeRegistry.tryRecord ctx.Types "Rec" with
                | ValueSome info ->
                    Expect.equal info.Key (SymbolKeyOps.typeKey None "Foo.Bar" ("Rec")) "record key carries namespace"
                | ValueNone -> failtest "Rec not registered"

                match TypeRegistry.tryUnion ctx.Types "Choice" 2 with
                | ValueSome info ->
                    Expect.equal
                        info.Key
                        (SymbolKeyOps.typeKey None "Foo.Bar" ("Choice`2"))
                        "union key carries namespace + arity"
                | ValueNone -> failtest "Choice`2 not registered"
            }

            test "Arity-overloaded types under one namespace mint unique keys" {
                // The uniqueness gate: every accepted type mints a distinct key, so
                // `recordKeyOrigin` reports no collision. `Choice\`2` / `Choice\`3` share a
                // namespace and short name yet stay distinct by arity-suffix.
                let ctx =
                    analyse
                        "namespace Foo\n\ntype Choice<'a, 'b> = | C1 of 'a | C2 of 'b\ntype Choice<'a, 'b, 'c> = | D1 of 'a | D2 of 'b | D3 of 'c"

                let collision =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "SymbolKey collision")

                Expect.isFalse collision "no SymbolKey collision for arity-overloaded Choice"

                Expect.isTrue (TypeRegistry.tryUnion ctx.Types "Choice" 2).IsSome "Choice`2 registered"

                Expect.isTrue (TypeRegistry.tryUnion ctx.Types "Choice" 3).IsSome "Choice`3 registered"
            }
        ]
