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
    let mono name =
        ValueSome(
            ExternalSymbols.monoFrozen (SymbolKeyOps.inNamespace "") name (FTConst(RuntimeNames.intKey, EqArray.empty))
        )

    // `Color` is `[<RequireQualifiedAccess>]` (its case `Red` carries the flag);
    // `Hue` is an ordinary union (`Blue` does not). Drives the RQA-suppression
    // tests below.
    let mkCase union rqa name =
        ValueSome
            {
                UnionName = union
                TyparArity = 0
                Origin = SymbolOrigin.Empty
                Case = ExternalCaseShape.create (name, [||])
                IsRequireQualifiedAccess = rqa
            }

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun n ->
                    if n = "A.B.thing" then mono "thing"
                    // The qualified operator `A.B.(+)` resolves to its compiled name
                    // `A.B.op_Addition`.
                    elif n = "A.B.op_Addition" then mono "op_Addition"
                    else ValueNone
            TryLookupType =
                fun n ->
                    if n = "Some.Where.Foo`1" then
                        ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty)))
                    else
                        ValueNone
            TryLookupUnionCase =
                fun caseName ->
                    match caseName with
                    | "Red" -> mkCase "Tests.Color" true "Red"
                    | "Blue" -> mkCase "Tests.Hue" false "Blue"
                    | _ -> ValueNone
        }

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, Hashing.originSourceOfText input lexed)
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
                // Mirrors the external-type receiver shape at the NameResolution layer:
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

            // `[<RequireQualifiedAccess>]` suppression — isolated from the opens gate
            // by opening `Tests` first, so the ONLY reason `Red` is rejected is RQA.
            test "a bare RQA case name is unresolved even when its namespace is open" {
                // `Color` is RQA, so the short `Red` must not resolve — F# requires
                // `Color.Red` — regardless of `open Tests`.
                let ctx = analyse "open Tests\nlet x = Red"
                Expect.isTrue (hasUnresolved ctx) "bare Red is rejected for an RQA union"
            }

            test "a qualified RQA case name still resolves" {
                // Qualified `Color.Red` resolves WITHOUT its namespace opened — F#'s
                // qualified case resolution doesn't consult the per-scope tables.
                let ctx = analyse "let x = Color.Red"
                Expect.isFalse (hasUnresolved ctx) "Color.Red resolves (qualified form is allowed)"
            }

            // The opens gate on bare external cases: F# has no global reverse case
            // index, so a bare non-RQA case is visible only once its declaring
            // namespace is opened/auto-opened.
            test "a bare non-RQA case name resolves once its namespace is opened" {
                let ctx = analyse "open Tests\nlet x = Blue"
                Expect.isFalse (hasUnresolved ctx) "bare Blue resolves under open Tests"
            }

            test "a bare non-RQA case name without its open is unresolved" {
                // `Hue` lives in `Tests`, which is neither opened here nor in the
                // (empty) ambient prelude, so bare `Blue` must not resolve — the
                // opens false-accept this gate closes.
                let ctx = analyse "let x = Blue"
                Expect.isTrue (hasUnresolved ctx) "bare Blue is unresolved with Tests not opened"
            }

            // Operator-form qualified long idents.
            test "a qualified operator long-ident resolves to its compiled name" {
                let ctx = analyse "let f = A.B.(+)"
                Expect.isFalse (hasUnresolved ctx) "A.B.(+) resolves via A.B.op_Addition"
            }

            test "an unknown qualified operator long-ident is unresolved" {
                let ctx = analyse "let f = A.B.(*)"
                Expect.isTrue (hasUnresolved ctx) "A.B.(*) is unresolved — provider knows no op_Multiply"
            }
        ]
