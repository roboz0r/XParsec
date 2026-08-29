module XParsec.FSharp.SemanticAnalysis.Tests.OpenResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A short name resolves against the `open`s in scope, and only those declared above it.

/// A provider publishing two values in `module B` of `namespace A` (`thing` and the compiled
/// operator `op_Addition`), one generic class (`Some.Where.Foo`1`) and two unions in
/// `namespace Tests`. `Color` is `[<RequireQualifiedAccess>]`, `Hue` is ordinary. No prefix
/// is ambient, so every name here needs the `open` its test writes.
let private provider: IExternalSymbolProvider =
    let moduleB = ModuleContainer.InModule(SymbolKeyOps.moduleInNamespace "A" "B")

    let mono name =
        ExternalSymbols.monoFrozen moduleB name (FTConst(RuntimeNames.intKey, EqArray.empty))

    providerOfSurface (fun b ->
        PublishedSurfaceBuilder.addValue b ValueNone (mono "thing")
        // The qualified operator `A.B.(+)` resolves to its compiled name `A.B.op_Addition`.
        PublishedSurfaceBuilder.addValue b ValueNone (mono "op_Addition")
        publishClass b (SymbolKeyOps.qualifiedTypeKeyOf "Some.Where.Foo" 1) []

        publishRqaUnion
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Color" 0)
            [ ExternalCaseShape.create ("Red", EqArray.empty) ]

        publishUnion
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Hue" 0)
            [ ExternalCaseShape.create ("Blue", EqArray.empty) ]
    )

let private analyse (input: string) =
    let lexed, file = parseFile input

    let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

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
                // `thing` (element 0) precedes `open A.B` (element 1), so the open never
                // reaches back to it.
                let ctx = analyse "let x = thing\nopen A.B"
                Expect.isTrue (hasUnresolved ctx) "the open below does not reach the binding above"
            }

            test "short external type name resolves under its open (no unresolved error)" {
                // `Foo<int>` is suppressed as an external-type reference once
                // `open Some.Where` is in scope.
                let ctx = analyse "open Some.Where\nlet f = Foo<int>"
                Expect.isFalse (hasUnresolved ctx) "Foo<int> resolves via open Some.Where"
            }

            test "short external type name without its open stays unresolved" {
                let ctx = analyse "let f = Foo<int>"
                Expect.isTrue (hasUnresolved ctx) "Foo is unresolved with no open"
            }

            test "a nested module inherits an enclosing open" {
                let ctx = analyse "open A.B\nmodule M =\n    let x = thing"
                Expect.isFalse (hasUnresolved ctx) "thing resolves inside nested M via the enclosing open A.B"
            }

            // `open Tests` isolates this from the opens gate: the ONLY reason `Red` is
            // rejected is that `Color` is RQA, so F# requires `Color.Red`.
            test "a bare RQA case name is unresolved even when its namespace is open" {
                let ctx = analyse "open Tests\nlet x = Red"
                Expect.isTrue (hasUnresolved ctx) "bare Red is rejected for an RQA union"
            }

            test "a qualified RQA case name resolves under its open" {
                let ctx = analyse "open Tests\nlet x = Color.Red"
                Expect.isFalse (hasUnresolved ctx) "Color.Red resolves (qualified form is allowed)"
            }

            // `dotnet fsi` rejects this with FS0039 on `Color`: the union's own name needs
            // `open Tests`, and qualifying the case does not supply it.
            test "a qualified RQA case name without its open is unresolved" {
                let ctx = analyse "let x = Color.Red"
                Expect.isTrue (hasUnresolved ctx) "Color.Red is unresolved with Tests not opened"
            }

            // F# has no global reverse case index, so a bare non-RQA case is visible only
            // once its declaring namespace is opened or auto-opened.
            test "a bare non-RQA case name resolves once its namespace is opened" {
                let ctx = analyse "open Tests\nlet x = Blue"
                Expect.isFalse (hasUnresolved ctx) "bare Blue resolves under open Tests"
            }

            test "a bare non-RQA case name without its open is unresolved" {
                // `Hue` lives in `Tests`, neither opened here nor in the (empty) ambient
                // prelude, so bare `Blue` must not resolve.
                let ctx = analyse "let x = Blue"
                Expect.isTrue (hasUnresolved ctx) "bare Blue is unresolved with Tests not opened"
            }

            test "a qualified operator long-ident resolves to its compiled name" {
                let ctx = analyse "let f = A.B.(+)"
                Expect.isFalse (hasUnresolved ctx) "A.B.(+) resolves via A.B.op_Addition"
            }

            test "an unknown qualified operator long-ident is unresolved" {
                let ctx = analyse "let f = A.B.(*)"
                Expect.isTrue (hasUnresolved ctx) "A.B.(*) is unresolved — provider knows no op_Multiply"
            }
        ]
