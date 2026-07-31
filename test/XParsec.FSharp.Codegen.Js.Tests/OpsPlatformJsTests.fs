module XParsec.FSharp.Codegen.Js.Tests.OpsPlatformJsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS-TARGET layer-1 harvest, built through the JS-native contract (`JsNativeSymbols`)
// — the leaf a real JS build uses, with no dependency on the CLR backend. The
// CROSS-target contrasts (a JS template absent on CLR; the CLR BCL repr) need the BCL
// metadata leaf and live in `Codegen.Clr.Tests.OpsPlatformClrTests`.
//
// The body harvest itself (`InlineBodies.ilOpCodes`) is shared with that suite: both read
// the same contract, only through a different symbol leaf.

[<Tests>]
let tests =
    testList
        "OpsPlatformJs"
        [
            // The declared operator surface IS the JS target's arithmetic-support
            // definition; the manifest states the same matrix. Built through the JS-native
            // leaf, so this is the contract a real JS build resolves against.
            OperatorSurfaceParity.tests
                "js"
                (JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) [ vesperCoreManifest ])
                (JsNativeSymbols.jsNativeInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ])

            // The operators themselves are still `let inline` values in the collection —
            // now bare trait calls, with the per-width IL on the primitives.
            test "arithmetic operators are collected as cross-package inlines (js target)" {
                let js =
                    JsNativeSymbols.jsNativeInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                for name in
                    [
                        "op_Addition"
                        "op_Subtraction"
                        "op_Multiply"
                        "op_Division"
                        "op_Modulus"
                        "op_UnaryNegation"
                    ] do
                    Expect.isTrue (Map.containsKey name js) (sprintf "%s sourced from ops-platform.js.fs" name)
            }

            // The per-width JS templates, read off the primitives that now declare them.
            // Each of these was a `when ^T1 : …` clause on the operator; the width owning
            // its own body is what the freeze has to carry across intact.
            test "the int32 / int64 / float bodies freeze with their JS templates intact" {
                let js =
                    JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) [ vesperCoreManifest ]

                let opsOf width compiled =
                    InlineBodies.ilOpCodes (InlineBodies.operatorBody js width compiled)

                Expect.contains (opsOf "float" "op_Addition") "$0 + $1" "float `+` is the bare JS operator"
                Expect.contains (opsOf "int" "op_Addition") "($0 + $1) | 0" "int32 `+` truncates through `| 0`"

                Expect.contains
                    (opsOf "int64" "op_Addition")
                    "BigInt.asIntN(64, $0 + $1)"
                    "int64 `+` wraps through BigInt"

                // `Math.imul` is the JS answer at 32 bits (contrasted against the CLR
                // `mul` mnemonic in `OpsPlatformClrTests`).
                Expect.contains (opsOf "int" "op_Multiply") "Math.imul($0, $1)" "int32 `*` is the Math.imul template"

                Expect.contains (opsOf "float" "op_UnaryNegation") "-$0" "float `~-` is the bare JS operator"
                Expect.contains (opsOf "int" "op_UnaryNegation") "(-$0) | 0" "int32 `~-` re-truncates"

                // The `%` in the modulus template is NOT a printf placeholder — must survive freeze verbatim.
                Expect.contains (opsOf "float" "op_Modulus") "$0 % $1" "modulus `%` carried through verbatim"
                Expect.contains (opsOf "int" "op_Modulus") "($0 % $1) | 0" "int32 modulus keeps the bare `%`"
            }

            test "equality operators freeze with `===` primitive clauses + structural-call base" {
                let js =
                    JsNativeSymbols.jsNativeInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                // The aggregate base is a CALL to `structuralEquals`, not an IL template;
                // `ilOpCodes` sees the `===` clauses but no `equals(` opcode.
                let eq = InlineBodies.ilOpCodes js.["op_Equality"]
                Expect.contains eq "$0 === $1" "primitive `===` clause"

                Expect.isFalse
                    (eq |> List.exists (fun s -> s.Contains "equals"))
                    "the structural base is an external call, not a bare-name IL template"

                let neq = InlineBodies.ilOpCodes js.["op_Inequality"]
                Expect.contains neq "$0 !== $1" "primitive `!==` clause"
                Expect.contains neq "!$0" "the base negates the `structuralEquals` call via a `!$0` template"

                Expect.isFalse
                    (neq |> List.exists (fun s -> s.Contains "equals"))
                    "the negated base wraps a call, not a bare-name IL template"
            }

            test "JS numeric reprs: canon identities stay distinct while both platform-project to `number`" {
                // `int` and `float` must keep distinct canon faces; a shared repr would conflate %d/%f
                // and integer division.
                let js =
                    JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) [ vesperCoreManifest ]

                let facesOf (name: string) =
                    match js.TryLookupType name |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Intrinsic {
                                                                Id = {
                                                                         Canon = canon
                                                                         Platform = Some platform
                                                                     }
                                                            }) -> SymbolKey.Type canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                let intCanon, intPlat = facesOf "Vesper.int"
                let floatCanon, floatPlat = facesOf "Vesper.float"

                // Identity axis — the canon faces ARE the `.fsi` names, platform-
                // INVARIANT (a JS build never sees a BCL name) and distinct, so the
                // unifier never conflates `int` with `float`.
                Expect.equal intCanon (RuntimeNames.intKey) "int canon identity is the `.fsi` name"
                Expect.equal floatCanon (RuntimeNames.floatKey) "float canon identity is the `.fsi` name"
                Expect.notEqual intCanon floatCanon "int and float MUST keep distinct canon identities"

                Expect.equal intPlat "number" "int platform face repoints to JS `number`"
                Expect.equal floatPlat "number" "float platform face repoints to JS `number`"

                Expect.notEqual
                    (SymbolKeyOps.intrinsicName intCanon)
                    intPlat
                    "the two faces genuinely diverge on JS (identity ≠ runtime repr)"
            }

            test "JS target: unit -> undefined, int64/uint64 -> bigint (canon = `.fsi` name)" {
                // `number` loses precision past 53 bits, so int64/uint64 must use `bigint`.
                let js =
                    JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) [ vesperCoreManifest ]

                let facesOf (name: string) =
                    match js.TryLookupType name |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Intrinsic {
                                                                Id = {
                                                                         Canon = canon
                                                                         Platform = Some platform
                                                                     }
                                                            }) -> SymbolKey.Type canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                Expect.equal (facesOf "Vesper.unit") (RuntimeNames.unitKey, "undefined") "unit -> undefined on JS"

                Expect.equal (facesOf "Vesper.int64") (RuntimeNames.int64Key, "bigint") "int64 -> bigint on JS"

                Expect.equal
                    (facesOf "Vesper.uint64")
                    (RuntimeNames.primitiveKey "uint64", "bigint")
                    "uint64 -> bigint on JS"
            }

            // The bodies above carry the PRODUCER's token indices, so the collection has to
            // hand back the files those indices are read against — this is what makes an
            // `ops-platform.js.fs` position recoverable at all. Assert the retention against
            // the disk it claims to describe: a hash taken from something other than the text
            // that was parsed would turn every later resolution into a spurious hard failure.
            test "the collection retains the producer files its bodies are anchored in" {
                let origins =
                    JsNativeSymbols.jsNativeInlineOriginsFor (Some Target.Js) [ vesperCoreManifest ]
                    |> OriginSources.toList

                Expect.isNonEmpty origins "the JS `inline-bodies` files are retained, not dropped after the parse"

                Expect.isTrue
                    (origins |> List.exists (fun s -> s.File.Path.Relative.Contains "ops-platform"))
                    "…including the one the arithmetic bodies above come from"

                for s in origins do
                    let f = s.File

                    // Resolved the way the collection itself resolved it — the package
                    // directory plus the path the manifest names — because a retained file's
                    // IDENTITY says which file it is, never where this build mounted it.
                    let path =
                        System.IO.Path.Combine(System.IO.Path.GetDirectoryName vesperCoreManifest, f.Path.Relative)

                    Expect.isTrue (System.IO.File.Exists path) (sprintf "%s exists on disk" f.Path.Relative)

                    // `parseFileFull` normalises line endings before lexing, so the retained
                    // text — the string the token offsets index, and the string a source map
                    // would publish — is the normalised one, and the hash is of exactly it.
                    let onDisk = (System.IO.File.ReadAllText path).Replace("\r\n", "\n")

                    Expect.equal s.Input onDisk (sprintf "%s's retained text is the file's text" f.Path.Relative)

                    Expect.equal
                        f.Content
                        (Hashing.hashString onDisk)
                        (sprintf "%s's retained hash is the hash of the text that was parsed" f.Path.Relative)
            }
        ]
