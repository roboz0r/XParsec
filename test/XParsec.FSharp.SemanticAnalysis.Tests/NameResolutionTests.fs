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
                // "let x = 1 in x" — body `x` at offset 13, binding `x` at offset 4.
                let ctx = analyse "let x = 1 in x"
                let bodyKey = NodeKey.ofSource 13 NodeKind.ExprIdent
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite bindingKey "binding site"
                | ValueNone -> failtest "body ident not resolved"
            }

            test "lambda parameter is in scope for body" {
                // "let id = fun x -> x" — lambda body `x` resolves to param `x`.
                // Parameter `x` is at offset 13, body `x` at offset 18.
                let ctx = analyse "let id = fun x -> x"
                let bodyKey = NodeKey.ofSource 18 NodeKind.ExprIdent
                let paramKey = NodeKey.ofSource 13 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite paramKey "binding site"
                | ValueNone -> failtest "lambda body ident not resolved"
            }

            test "function-form let: argument is in scope for body" {
                // "let f x = x" — body `x` resolves to arg pattern `x`.
                // Arg `x` at offset 6, body `x` at offset 10.
                let ctx = analyse "let f x = x"
                let bodyKey = NodeKey.ofSource 10 NodeKind.ExprIdent
                let argKey = NodeKey.ofSource 6 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite argKey "binding site"
                | ValueNone -> failtest "fn-form arg not resolved"
            }

            test "shadowing: inner binding wins" {
                // "let x = 1 in let x = 2 in x" — inner x at offset 17.
                // The body uses the inner binding, not the outer.
                let ctx = analyse "let x = 1 in let x = 2 in x"
                let bodyKey = NodeKey.ofSource 26 NodeKind.ExprIdent
                let innerBindingKey = NodeKey.ofSource 17 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bodyKey with
                | ValueSome rb -> Expect.equal rb.BindingSite innerBindingKey "resolves to inner x"
                | ValueNone -> failtest "body x not resolved"
            }

            test "module-level let visible to next module element" {
                // "let x = 1\nlet y = x" — second let's RHS `x` resolves to first let's `x`.
                let ctx = analyse "let x = 1\nlet y = x"
                // After "let x = 1\n" (10 chars), "let y = " brings us to offset 18 for `x`.
                let useKey = NodeKey.ofSource 18 NodeKind.ExprIdent
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Binding.TryGetValue useKey with
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
                // `true` is in MockBuiltins. It resolves through the provider,
                // so no diagnostic — and no Binding entry (external).
                let ctx = analyse "let x = true"
                let useKey = NodeKey.ofSource 8 NodeKind.ExprIdent
                Expect.equal ctx.Diagnostics.Count 0 "no diagnostics for known external"
                Expect.isTrue (ctx.Binding.TryGetValue useKey = ValueNone) "no Binding entry for external symbol"
            }

            test "let mutable: binding-site IsMutable is true" {
                // `let mutable n = 0` — pattern `n` at offset 12.
                let ctx = analyse "let mutable n = 0"
                let bindingKey = NodeKey.ofSource 12 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bindingKey with
                | ValueSome rb -> Expect.isTrue rb.IsMutable "binding-site entry has IsMutable = true"
                | ValueNone -> failtest "no binding-site self-entry for `n`"
            }

            test "let (no mutable): binding-site IsMutable is false" {
                let ctx = analyse "let n = 0"
                let bindingKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Binding.TryGetValue bindingKey with
                | ValueSome rb -> Expect.isFalse rb.IsMutable "binding-site entry has IsMutable = false"
                | ValueNone -> failtest "no binding-site self-entry for `n`"
            }

            test "use of a mutable binding: use-site IsMutable mirrors binding" {
                // `let mutable n = 0 in n` — use `n` at offset 21.
                let ctx = analyse "let mutable n = 0 in n"
                let useKey = NodeKey.ofSource 21 NodeKind.ExprIdent

                match ctx.Binding.TryGetValue useKey with
                | ValueSome rb -> Expect.isTrue rb.IsMutable "use-site IsMutable propagated from binding"
                | ValueNone -> failtest "use of `n` not resolved"
            }

            test "record type definition registers in ctx.RecordTypes" {
                let ctx = analyse "type R = { X: int; Y: int }"

                match ctx.RecordTypes.TryGetValue "R" with
                | true, info ->
                    Expect.equal info.Fields.Length 2 "two fields"
                    Expect.equal info.Fields.[0].Name "X" "first field is X"
                    Expect.equal info.Fields.[1].Name "Y" "second field is Y"
                | false, _ -> failtest "record type R not registered"
            }

            test "record field index is built" {
                let ctx = analyse "type R = { X: int; Y: int }"

                match ctx.FieldIndex.TryGetValue "X" with
                | true, infos -> Expect.equal infos.Length 1 "X referenced by exactly one type"
                | false, _ -> failtest "X not in FieldIndex"

                match ctx.FieldIndex.TryGetValue "Y" with
                | true, infos -> Expect.equal infos.Length 1 "Y referenced by exactly one type"
                | false, _ -> failtest "Y not in FieldIndex"
            }

            test "duplicate record type name diagnoses" {
                let ctx = analyse "type R = { X: int }\ntype R = { Y: int }"

                let hasDup =
                    ctx.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "Duplicate record type")

                Expect.isTrue hasDup "duplicate-record diagnostic emitted"
            }

            test "mutable field IsMutable is true" {
                let ctx = analyse "type P = { X: int; mutable Y: int }"

                match ctx.RecordTypes.TryGetValue "P" with
                | true, info ->
                    Expect.isFalse info.Fields.[0].IsMutable "X is immutable"
                    Expect.isTrue info.Fields.[1].IsMutable "Y is mutable"
                | false, _ -> failtest "record type P not registered"
            }

            // ---- Discriminated unions ----

            test "union type definition registers in ctx.UnionTypes" {
                let ctx =
                    analyse "type S =\n    | Circle of float\n    | Rectangle of float * float\n    | Point"

                match ctx.UnionTypes.TryGetValue "S" with
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

                match ctx.CtorIndex.TryGetValue "Circle" with
                | true, infos ->
                    Expect.equal infos.Length 1 "Circle declared by exactly one union"
                    Expect.equal infos.Head.UnionName "S" "Circle belongs to S"
                | false, _ -> failtest "Circle not in CtorIndex"

                match ctx.CtorIndex.TryGetValue "Point" with
                | true, infos -> Expect.equal infos.Length 1 "Point declared by exactly one union"
                | false, _ -> failtest "Point not in CtorIndex"
            }

            test "nullary ctor in pattern binds nothing" {
                // `match v with | Point -> ()` — `Point` is a known ctor,
                // not a binder. The ident at offset 21 should NOT have a
                // self-binding entry.
                let ctx = analyse "type S = | Point\nmatch 0 with | Point -> 0 | _ -> 0"

                // The Point ident in the pattern is at... let me look up the offset
                // Source: "type S = | Point\n" is 17 chars. "match 0 with | " is +15 → offset 32. So Point starts at 32.
                let patKey = NodeKey.ofSource 32 NodeKind.PatIdent
                let hasBinding = ctx.Binding.ContainsKey patKey
                Expect.isFalse hasBinding "Point pattern should not be a binding site"
            }

            test "bare ctor reference does not emit Unresolved diagnostic" {
                let ctx = analyse "type S = | Point\nlet p = Point"

                let hasUnresolved =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

                Expect.isFalse hasUnresolved "bare ctor name not flagged as unresolved"
            }
        ]
