module XParsec.FSharp.Codegen.Js.Tests.RefsTableTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A hand-built TWO-manifest slice pinning the CROSS-PACKAGE identity path: package A declares
// `Box<T>` with an instance member `get(): T`; package B does NOT declare `Box`, exports
// `theBox: Box<string>`, and records `Box`'s identity (home `A`, class, arity 1) in its refs.

/// Package A: declares the generic class `Box<T>` (arity 1) with `get(): T`.
let private manifestA: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "A"
        Version = None
        Exports =
            [
                Schema.Export.Class("Box", 1, [ method' "get" (sig0 (typar 0)) ], [], Schema.ImportShape.Named, [])
            ]
        Diagnostics = []
        Refs = []
    }

/// Package B: references (but does not declare) `Box`, exporting `theBox: Box<string>`
/// and carrying `Box`'s foreign identity in its refs table.
let private manifestB: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "B"
        Version = None
        Exports =
            [
                Schema.Export.Variable("theBox", namedG "Box" [ named "string" ], true, Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = [ refEntry "Box" "A" Schema.RefKind.Class 1 ]
    }

/// B's provider ALONE (no A), for inspecting the minted identity and the absent-home path.
let private bProviderRaw: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest manifestB

/// A program that reads `theBox`'s member. `theBox` is bound to a LOCAL first so `b.get()` is
/// unambiguously member access on a value; a bare `theBox.get()` name-resolves as a dotted
/// qualified name instead.
let private program = "let b = theBox\nlet n = b.get()\nprintfn \"%s\" n\n"

[<Tests>]
let tests =
    testList
        "RefsTable"
        [
            test "B's provider mints the foreign Box reference's FTClass identity, which resolves to A's shape" {
                // `theBox`'s frozen scheme is `FTClass` keyed to the foreign `Box`1`, not an
                // opaque `FTConst`, even though B's own registry never saw `Box`. The key is
                // nominal and carries no home; the SHAPE it resolves to is what is homed in A.
                match ScopeContents.tryValueAt bProviderRaw.Scope "theBox" with
                | ValueSome sym ->
                    match sym.Scheme with
                    | FTClass(key, args) ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName key)
                            "Box`1"
                            "the minted key must be the arity-suffixed foreign name (the arity law)"

                        Expect.equal args.Length 1 "Box<string> applies one type arg"

                        match (stackTsMany [ manifestB; manifestA ] :> IExternalSymbolStore).TryLookupType key with
                        | ValueSome(ExternalTypeShape.Class info) ->
                            Expect.equal
                                info.Origin.Home.AssemblyOption
                                (ValueSome(AssemblyName "A"))
                                "the key B minted resolves, through the stack, to a shape homed in package A"
                        | other -> failtestf "the minted key must resolve to A's Box class shape, got %A" other
                    | other -> failtestf "theBox scheme should be an FTClass, got %A" other
                | ValueNone -> failtest "theBox did not resolve as a value symbol"
            }

            test "with A stacked under B, member access on the Box-typed value resolves through the stack" {
                // `b.get()` resolves `get` via A's provider (first-hit-wins over the stack) and
                // grounds the declaring typar to `string`, so `printfn "%s" n` type-checks
                // through the ordinary provider stack.
                let errors = analyseWith (stackTsMany [ manifestB; manifestA ]) program

                Expect.isEmpty
                    errors
                    (sprintf "cross-package member access should type-check, got:\n%A" (errorText errors))
            }

            test "with A ABSENT, member access fails and reports the type that has no shape" {
                // With only B stacked, the `Box` identity B minted has no shape anywhere in the
                // stack, because the refs table is identity-only, so member access errors. It
                // reports the TYPE, not the missing package: a `SymbolKey` carries no home.
                let errors = analyseWith (stackTs manifestB) program
                let text = errorText errors

                Expect.isNonEmpty errors "member access on an absent-home type must error"

                Expect.stringContains
                    text
                    "Unknown class type 'Box`1'"
                    (sprintf "the diagnostic must name the unresolvable type, got:\n%s" text)
            }
        ]
