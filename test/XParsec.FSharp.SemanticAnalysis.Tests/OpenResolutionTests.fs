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
        PublishedSurfaceBuilder.addValue b (mono "thing")
        // The qualified operator `A.B.(+)` resolves to its compiled name `A.B.op_Addition`.
        PublishedSurfaceBuilder.addValue b (mono "op_Addition")
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

/// The contract stack, extended with three modules of `namespace Ref`, of which `Ref.Rqa`
/// alone carries `[<RequireQualifiedAccess>]`. Each module publishes one value named `ext…`.
/// The shadowing sources below declare a module at each path, so each `open` reaches two
/// declarations of one path. The real contracts stay in the stack because a module's own
/// `[<RequireQualifiedAccess>]` is read by resolving the attribute type.
let private shadowedProvider: IExternalSymbolProvider =
    let modul name =
        SymbolKeyOps.moduleInNamespace "Ref" name

    let publishModule (b: PublishedSurfaceBuilder) (name: string) (facts: ModuleFacts) (valueName: string) =
        PublishedSurfaceBuilder.addModule
            b
            (modul name)
            {
                Home = SymbolHome.Unstamped
                Facts = facts
            }

        PublishedSurfaceBuilder.addValue
            b
            (ExternalSymbols.monoFrozen
                (ModuleContainer.InModule(modul name))
                valueName
                (FTConst(RuntimeNames.intKey, EqArray.empty)))

    let published =
        providerOfSurface (fun b ->
            publishModule
                b
                "Rqa"
                { ModuleFacts.plain with
                    RequiresQualifiedAccess = true
                }
                "extV"

            publishModule b "Plain" ModuleFacts.plain "extX"
            publishModule b "Other" ModuleFacts.plain "extW"
        )

    ExternalSymbolProviders.composite [ published; realProvider.Value ]

/// Two packages declaring `Ref.Auto`, of which one carries `[<AutoOpen>]`. Each publishes one
/// value, so a bare read names the declaration the auto-open brought into scope.
let private autoOpenShadowProvider: IExternalSymbolProvider =
    let path = SymbolKeyOps.moduleInNamespace "Ref" "Auto"

    let surface (facts: ModuleFacts) (valueName: string) =
        providerOfSurface (fun b ->
            PublishedSurfaceBuilder.addModule
                b
                path
                {
                    Home = SymbolHome.Unstamped
                    Facts = facts
                }

            PublishedSurfaceBuilder.addValue
                b
                (ExternalSymbols.monoFrozen
                    (ModuleContainer.InModule path)
                    valueName
                    (FTConst(RuntimeNames.intKey, EqArray.empty)))
        )

    ExternalSymbolProviders.composite
        [
            surface
                { ModuleFacts.plain with
                    IsAutoOpen = true
                }
                "autoV"
            surface ModuleFacts.plain "plainV"
            realProvider.Value
        ]

let private analyseWith (external: IExternalSymbolProvider) (input: string) =
    let lexed, file = parseFile input

    let ctx = PassContext(external, LexedFile.ofText lexed, testCompiling)

    NameResolution.run ctx file
    ctx

let private analyse (input: string) = analyseWith provider input

let private hasUnresolved (ctx: PassContext) : bool =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

/// FS0892, `open` of a `[<RequireQualifiedAccess>]` module.
let private refusesOpen (ctx: PassContext) : bool =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Code = DiagCode.FSharp 892)

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

            // A module path declared by this compilation AND by a reference: `open` reaches
            // both declarations, so `[<RequireQualifiedAccess>]` on either one refuses it and a
            // path both declare plainly contributes both their members. The three cases run
            // each refusal direction and the merge, each verified against a compiled reference
            // assembly.
            test "the reference's [<RequireQualifiedAccess>] refuses an open of a plain local module" {
                let ctx =
                    analyseWith
                        shadowedProvider
                        "\
namespace Ref

module Rqa =
    let locV = 1

module Use =
    open Rqa
    let x = locV
"

                Expect.isTrue (refusesOpen ctx) "Ref.Rqa is RQA in the reference"
            }

            // Both members are read QUALIFIED: the refusal falls on the `open` alone, and the
            // reference's `extX` stays reachable through the path the local declaration shares.
            test "a local [<RequireQualifiedAccess>] refuses an open of a plain referenced module" {
                let ctx =
                    analyseWith
                        shadowedProvider
                        "\
namespace Ref

[<RequireQualifiedAccess>]
module Plain =
    let locV = 1

module Use =
    open Plain
    let x = Plain.locV + Plain.extX
"

                Expect.isTrue (refusesOpen ctx) "Ref.Plain is RQA in this compilation"
                Expect.isFalse (hasUnresolved ctx) "both declarations of Ref.Plain answer a qualified read"
            }

            // A local declaration below the `open` has not entered scope there, so the
            // `open` reaches the reference's declaration alone and its own
            // `[<RequireQualifiedAccess>]` does not refuse it.
            test "a local [<RequireQualifiedAccess>] declared below the open does not refuse it" {
                let ctx =
                    analyseWith
                        shadowedProvider
                        "\
namespace Ref

module Use =
    open Plain
    let x = extX

[<RequireQualifiedAccess>]
module Plain =
    let locV = 1
"

                Expect.isFalse (refusesOpen ctx) "the local Ref.Plain enters scope below the open"
                Expect.isFalse (hasUnresolved ctx) "extX is the reference's"
            }

            // Above the local `module Plain`, the reference's `Ref.Plain` supplies a qualified
            // name; below it, the local declaration is in scope as well.
            test "a use above a local module of a referenced path reads the referenced module" {
                let ctx =
                    analyseWith
                        shadowedProvider
                        "\
namespace Ref

module Use =
    let a = Plain.extX

module Plain =
    let locV = 1

module Use2 =
    let b = Plain.locV + Plain.extX
"

                Expect.isFalse (hasUnresolved ctx) "extX is the reference's above and below; locV the local's below"
            }

            // One `open` of a shared path reaches both declarations, so this compilation's
            // `locV` and the reference's `extW` are in scope together.
            test "an open of a path both sources declare plainly admits both their members" {
                let ctx =
                    analyseWith
                        shadowedProvider
                        "\
namespace Ref

module Other =
    let locV = 1

module Use =
    open Other
    let x = locV + extW
"

                Expect.isFalse (refusesOpen ctx) "neither declaration of Ref.Other is RQA"
                Expect.isFalse (hasUnresolved ctx) "locV is this compilation's, extW the reference's"
            }

            // fsi: an `open` addresses a PATH, but an `[<AutoOpen>]` is per-DECLARATION — it
            // brings its own package's contents into scope and leaves another package's
            // declaration of the path closed. `PublishedSurface.ofBuilder` derives a
            // path-shaped `ImplicitOpen` from the marker, which reaches both.
            test "an [<AutoOpen>] declaration of a shared path brings its own contents into scope" {
                let ctx = analyseWith autoOpenShadowProvider "let x = autoV"
                Expect.isFalse (hasUnresolved ctx) "the [<AutoOpen>] declaration of Ref.Auto is in scope"
            }

            ptest "GAP: an [<AutoOpen>] declaration does not auto-open another package's declaration of the path" {
                let ctx = analyseWith autoOpenShadowProvider "let x = plainV"
                Expect.isTrue (hasUnresolved ctx) "the plain declaration of Ref.Auto stays closed"
            }

            // CONTROL: written explicitly, the `open` addresses the path and reaches both.
            test "an explicit open of the path both packages declare reaches both" {
                let ctx = analyseWith autoOpenShadowProvider "open Ref.Auto\nlet x = autoV + plainV"
                Expect.isFalse (hasUnresolved ctx) "both declarations of Ref.Auto answer under one open"
            }
        ]
