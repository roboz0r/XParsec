module XParsec.FSharp.SemanticAnalysis.Tests.OpenResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// O1 (docs/symbol-resolution-handoff.md (open-resolution)): NameResolution routes its provider-probe
// sites through `OpenScope.tryQualify`, so a short name resolves against the
// `open`s in scope — and only those declared before it (running accumulator).

/// A provider that knows one qualified *value* (`A.B.thing`) and one qualified
/// generic *type* (`Some.Where.Foo`1`) — enough to exercise the value and type
/// channels of short-name resolution without standing up real metadata.
let private provider: IExternalSymbolProvider =
    { new IExternalSymbolProvider with
        member _.TryLookup n =
            if n = "A.B.thing" then
                ValueSome(ExternalSymbols.mono "thing" (TyConst "int"))
            else
                ValueNone

        member _.TryLookupType n =
            if n = "Some.Where.Foo`1" then
                ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty)))
            else
                ValueNone

        member _.TryLookupMember(_, _) = ValueNone
        member _.TryLookupMembers(_, _) = [||]
    }

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx

let private hasUnresolved (ctx: PassContext) : bool =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

[<Tests>]
let tests =
    testList
        "OpenResolution"
        [
            test "open brings a qualified value into short-name scope" {
                let ctx = analyse "open A.B\nlet x = thing"
                Expect.isFalse (hasUnresolved ctx) "thing resolves via open A.B"
            }

            test "without the open, the short name is unresolved" {
                let ctx = analyse "let x = thing"
                Expect.isTrue (hasUnresolved ctx) "thing is unresolved with no open"
            }

            test "an open after a binding is not visible to that earlier binding" {
                // `thing` (element 0) precedes `open A.B` (element 1); the running
                // accumulator means the open does not reach back to it.
                let ctx = analyse "let x = thing\nopen A.B"
                Expect.isTrue (hasUnresolved ctx) "the open below does not reach the binding above"
            }

            test "short external type name resolves under its open (no unresolved error)" {
                // Mirrors the milestone receiver shape at the NameResolution layer:
                // `Foo<int>` is suppressed as an external-type reference once
                // `open Some.Where` is in scope.
                let ctx = analyse "open Some.Where\nlet f = Foo<int>"
                Expect.isFalse (hasUnresolved ctx) "Foo<int> resolves via open Some.Where"
            }

            test "short external type name without its open stays unresolved" {
                let ctx = analyse "let f = Foo<int>"
                Expect.isTrue (hasUnresolved ctx) "Foo is unresolved with no open"
            }

            // O3 gate (symbol-resolution-handoff.md, open-resolution), the inheritance half: a nested
            // module sees an `open` declared in its enclosing scope. (The
            // referenced-contract ambient prelude — `hash` with no `open` — is the
            // deferred half; see the plan's Status note.)
            test "a nested module inherits an enclosing open" {
                let ctx = analyse "open A.B\nmodule M =\n    let x = thing"
                Expect.isFalse (hasUnresolved ctx) "thing resolves inside nested M via the enclosing open A.B"
            }
        ]
