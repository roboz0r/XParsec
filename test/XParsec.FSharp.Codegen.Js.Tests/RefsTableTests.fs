module XParsec.FSharp.Codegen.Js.Tests.RefsTableTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// Refs-table isolation fixture, systematic-tests-first: a hand-built
// TWO-manifest slice pins the CROSS-PACKAGE identity path end-to-end, independent of
// the extractor/Fable follow-on that will populate a real manifest's `Refs`.
//
//   • package A declares `Box<T>` with an instance member `get(): T`.
//   • package B does NOT declare `Box`; it EXPORTS `theBox: Box<string>` and records
//     `Box`'s foreign identity (`home = A`, kind = class, arity = 1) in its refs table.
//
// The refs table is IDENTITY ONLY — B never inlines `Box`'s shape or members. B's
// provider mints a HOMED `FTClass(TypeKey(Some "A", "", "Box`1"))`; member access on a
// `theBox` value then resolves through the ORDINARY provider stack when A is stacked,
// and fails with a "package not referenced" diagnostic when A is absent.

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

/// B's provider ALONE (no A) — for inspecting the minted identity and the absent-home path.
let private bProviderRaw: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest manifestB

/// A program that reads `theBox`'s member — the discriminator for stack resolution.
/// `theBox` is bound to a LOCAL first so `b.get()` is unambiguously member access on a
/// value (a bare `theBox.get()` name-resolves as a dotted qualified name, never
/// reaching `resolveFieldStep`).
let private program = "let b = theBox\nlet n = b.get()\nprintfn \"%s\" n\n"

[<Tests>]
let tests =
    testList
        "RefsTable"
        [
            test "B's provider mints a HOMED FTClass for the foreign Box reference (identity only)" {
                // Fact (a): `theBox`'s frozen scheme is `FTClass` keyed to A's home under the
                // arity-suffixed name `Box`1` — NOT an opaque `FTConst` — even though B's own
                // registry never saw `Box`. The `qualifiedName` equals what A's provider
                // registers `Box` under (`mint` at nsPath ""), so the stack can resolve it.
                match bProviderRaw.TryLookup "theBox" with
                | ValueSome sym ->
                    match sym.Scheme with
                    | FTClass(key, args) ->
                        Expect.equal
                            (SymbolKeyOps.keyAsm key)
                            (Some "A")
                            "the minted key must carry A as its home assembly"

                        Expect.equal
                            (SymbolKeyOps.qualifiedName key)
                            "Box`1"
                            "the minted key must be the arity-suffixed foreign name (the arity law)"

                        Expect.equal (args |> EqArray.toList |> List.length) 1 "Box<string> applies one type arg"
                    | other -> failtestf "theBox scheme should be a homed FTClass, got %A" other
                | ValueNone -> failtest "theBox did not resolve as a value symbol"
            }

            test "with A stacked under B, member access on the Box-typed value resolves through the stack" {
                // Fact (b): `theBox.get()` resolves `get` via A's provider (first-hit-wins over
                // the stack) and grounds the declaring typar to `string`, so `printfn "%s" n`
                // type-checks. No two-phase loader — the ordinary provider stack does it.
                let errors = analyseWith (stackTsMany [ manifestB; manifestA ]) program

                Expect.isEmpty
                    errors
                    (sprintf "cross-package member access should type-check, got:\n%A" (errorText errors))
            }

            test "with A ABSENT, member access fails naming the un-referenced package" {
                // Fact (c): with only B stacked, the homed `Box` identity has no shape in the
                // stack — the front end names the missing package rather than emitting a
                // generic no-such-member.
                let errors = analyseWith (stackTs manifestB) program
                let text = errorText errors

                Expect.isNonEmpty errors "member access on an absent-home type must error"

                Expect.stringContains
                    text
                    "referenced from package 'A' but that package is not part of the compilation"
                    (sprintf "the diagnostic must name the missing package, got:\n%s" text)
            }
        ]
