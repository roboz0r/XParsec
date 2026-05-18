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
        ]
