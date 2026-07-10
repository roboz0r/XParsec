module XParsec.FSharp.SemanticAnalysis.Tests.OpenResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution routes its provider-probe
// sites through `OpenScope.tryQualify`, so a short name resolves against the
// `open`s in scope — and only those declared before it (running accumulator).

/// A provider that knows one qualified *value* (`A.B.thing`) and one qualified
/// generic *type* (`Some.Where.Foo`1`) — enough to exercise the value and type
/// channels of short-name resolution without standing up real metadata.
let private provider: IExternalSymbolProvider =
    { new IExternalSymbolProvider with
        member _.TryLookup n =
            if n = "A.B.thing" then
                ValueSome(ExternalSymbols.monoFrozen "thing" (FTConst(RuntimeNames.intKey, EqArray.empty)))
            // The qualified operator `A.B.(+)` resolves to its compiled name
            // `A.B.op_Addition`.
            elif n = "A.B.op_Addition" then
                ValueSome(ExternalSymbols.monoFrozen "op_Addition" (FTConst(RuntimeNames.intKey, EqArray.empty)))
            else
                ValueNone

        member _.TryLookupType n =
            if n = "Some.Where.Foo`1" then
                ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty)))
            else
                ValueNone

        member _.TryLookupMember(_, _) = ValueNone
        member _.TryLookupMembers(_, _) = [||]
        member _.TryLookupIndexSignature _ = []

        // `Color` is `[<RequireQualifiedAccess>]` (its case `Red` carries the flag);
        // `Hue` is an ordinary union (`Blue` does not). Drives the Gap 1 suppression
        // tests below.
        member _.TryLookupUnionCase caseName =
            let mk union rqa name =
                ValueSome
                    {
                        UnionName = union
                        Arity = 0
                        Origin = SymbolOrigin.Empty
                        Case = ExternalCaseShape.create (name, [||])
                        IsRequireQualifiedAccess = rqa
                    }

            match caseName with
            | "Red" -> mk "Tests.Color" true "Red"
            | "Blue" -> mk "Tests.Hue" false "Blue"
            | _ -> ValueNone

        member _.AmbientOpenPrefixes = []
        member _.TryLookupInlineBody _ = ValueNone
        member _.TryLookupInlineBodyByName _ = ValueNone
        member _.IntrinsicReverseCanon = Map.empty
        member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
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

            // A nested module sees an `open` declared in its enclosing scope.
            test "a nested module inherits an enclosing open" {
                let ctx = analyse "open A.B\nmodule M =\n    let x = thing"
                Expect.isFalse (hasUnresolved ctx) "thing resolves inside nested M via the enclosing open A.B"
            }

            // Gap 1 — `[<RequireQualifiedAccess>]` suppression.
            test "a bare RQA case name is unresolved" {
                // `Color` is RQA, so the short `Red` must not resolve — F# requires
                // `Color.Red`. This is the false-accept the gap closed.
                let ctx = analyse "let x = Red"
                Expect.isTrue (hasUnresolved ctx) "bare Red is rejected for an RQA union"
            }

            test "a qualified RQA case name still resolves" {
                let ctx = analyse "let x = Color.Red"
                Expect.isFalse (hasUnresolved ctx) "Color.Red resolves (qualified form is allowed)"
            }

            test "a bare non-RQA case name resolves" {
                // Control: `Hue` is an ordinary union, so its bare case `Blue` is in
                // scope — the suppression is RQA-specific, not a blanket reject.
                let ctx = analyse "let x = Blue"
                Expect.isFalse (hasUnresolved ctx) "bare Blue resolves for a non-RQA union"
            }

            // Gap 4 — operator-form qualified long idents.
            test "a qualified operator long-ident resolves to its compiled name" {
                let ctx = analyse "let f = A.B.(+)"
                Expect.isFalse (hasUnresolved ctx) "A.B.(+) resolves via A.B.op_Addition"
            }

            test "an unknown qualified operator long-ident is unresolved" {
                let ctx = analyse "let f = A.B.(*)"
                Expect.isTrue (hasUnresolved ctx) "A.B.(*) is unresolved — provider knows no op_Multiply"
            }
        ]
