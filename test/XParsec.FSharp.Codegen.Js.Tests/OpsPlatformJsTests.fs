module XParsec.FSharp.Codegen.Js.Tests.OpsPlatformJsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The JS-native contract (`JsNativeSymbols`) is the platform metadata a real JS build uses,
// with no CLR-backend dependency. The CROSS-target contrasts need the .NET metadata reader and
// live in `Codegen.Clr.Tests.OpsPlatformClrTests`, which inspects bodies the same way.

[<Tests>]
let tests =
    testList
        "OpsPlatformJs"
        [
            // The declared operator surface IS the JS target's arithmetic-support definition;
            // the manifest states the same matrix.
            OperatorSurfaceParity.tests "js" (JsNativeSymbols.buildJsNativeContract [ vesperCorePackage ])

            // The operators are `let inline` values in the collection: bare trait calls, with
            // the per-width IL on the primitives.
            test "arithmetic operators are collected as cross-package inlines (js target)" {
                let js = JsNativeSymbols.jsNativeInlineBodies [ vesperCorePackage ]

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

            // The per-width JS templates, read off the primitives that declare them.
            test "the int32 / int64 / float bodies freeze with their JS templates intact" {
                let js = JsNativeSymbols.buildJsNativeContract [ vesperCorePackage ]

                let opsOf width compiled =
                    InlineBodies.ilOpCodes (InlineBodies.operatorBody js width compiled)

                Expect.contains (opsOf "float" "op_Addition") "$0 + $1" "float `+` is the bare JS operator"
                Expect.contains (opsOf "int" "op_Addition") "($0 + $1) | 0" "int32 `+` truncates through `| 0`"

                Expect.contains
                    (opsOf "int64" "op_Addition")
                    "BigInt.asIntN(64, $0 + $1)"
                    "int64 `+` wraps through BigInt"

                // `Math.imul` is the JS answer at 32 bits; the CLR width uses a `mul` mnemonic.
                Expect.contains (opsOf "int" "op_Multiply") "Math.imul($0, $1)" "int32 `*` is the Math.imul template"

                Expect.contains (opsOf "float" "op_UnaryNegation") "-$0" "float `~-` is the bare JS operator"
                Expect.contains (opsOf "int" "op_UnaryNegation") "(-$0) | 0" "int32 `~-` re-truncates"

                // The `%` in the modulus template is NOT a printf placeholder, so it must
                // survive the freeze verbatim.
                Expect.contains (opsOf "float" "op_Modulus") "$0 % $1" "modulus `%` carried through verbatim"
                Expect.contains (opsOf "int" "op_Modulus") "($0 % $1) | 0" "int32 modulus keeps the bare `%`"
            }

            test "equality operators freeze with `===` primitive clauses + structural-call base" {
                let js = JsNativeSymbols.jsNativeInlineBodies [ vesperCorePackage ]

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
                // `int` and `float` must keep distinct canon keys; a shared repr would conflate %d/%f
                // and integer division.
                let js = JsNativeSymbols.buildJsNativeContract [ vesperCorePackage ]

                let facesOf (name: string) =
                    match js.TryLookupType name |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Intrinsic {
                                                                Id = {
                                                                         Canon = canon
                                                                         Platform = IntrinsicPlatform.Repr platform
                                                                     }
                                                            }) -> canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                let intCanon, intPlat = facesOf "Vesper.int"
                let floatCanon, floatPlat = facesOf "Vesper.float"

                // The canon keys ARE the `.fsi` names, platform-INVARIANT (a JS build never
                // sees a BCL name) and distinct, so the unifier cannot conflate the two.
                Expect.equal intCanon (RuntimeNames.intKey) "int canon identity is the `.fsi` name"
                Expect.equal floatCanon (RuntimeNames.floatKey) "float canon identity is the `.fsi` name"
                Expect.notEqual intCanon floatCanon "int and float MUST keep distinct canon identities"

                Expect.equal intPlat "number" "int platform name repoints to JS `number`"
                Expect.equal floatPlat "number" "float platform name repoints to JS `number`"

                Expect.notEqual intCanon.Name intPlat "the two names genuinely diverge on JS (identity ≠ runtime repr)"
            }

            test "JS target: unit -> undefined, int64/uint64 -> bigint (canon = `.fsi` name)" {
                // `number` loses precision past 53 bits, so int64/uint64 must use `bigint`.
                let js = JsNativeSymbols.buildJsNativeContract [ vesperCorePackage ]

                let facesOf (name: string) =
                    match js.TryLookupType name |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Intrinsic {
                                                                Id = {
                                                                         Canon = canon
                                                                         Platform = IntrinsicPlatform.Repr platform
                                                                     }
                                                            }) -> canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                Expect.equal (facesOf "Vesper.unit") (RuntimeNames.unitKey, "undefined") "unit -> undefined on JS"

                Expect.equal (facesOf "Vesper.int64") (RuntimeNames.int64Key, "bigint") "int64 -> bigint on JS"

                Expect.equal
                    (facesOf "Vesper.uint64")
                    (RuntimeNames.primitiveKey "uint64", "bigint")
                    "uint64 -> bigint on JS"
            }

            // The bodies above carry the DECLARING file's token indices, so the collection must
            // hand back the files those indices read against. Asserted against the disk, because
            // retaining anything but the parsed text makes every later resolution a hard failure.
            test "the collection retains the declaring files its bodies are anchored in" {
                let sources =
                    JsNativeSymbols.jsNativeInlineSources [ vesperCorePackage ] |> LexedFiles.toList

                Expect.isNonEmpty sources "the JS `impl` files are retained, not dropped after the parse"

                Expect.isTrue
                    (sources |> List.exists (fun s -> s.Path.Relative.Name.Contains "ops-platform"))
                    "…including the one the arithmetic bodies above come from"

                for s in sources do
                    let f = s.Path

                    // Resolved the way the collection did, as the package directory plus the
                    // manifest-relative path, because a retained file's IDENTITY says which
                    // file it is, never where this build mounted it.
                    let path = System.IO.Path.Combine(vesperCorePackage, f.Relative.Name)

                    Expect.isTrue (System.IO.File.Exists path) (sprintf "%s exists on disk" f.Relative.Name)

                    // Verbatim on both sides: the retained text is the file's bytes as read,
                    // which is what the token offsets index and what a source map publishes.
                    let onDisk = System.IO.File.ReadAllText path

                    Expect.equal s.Input onDisk (sprintf "%s's retained text is the file's text" f.Relative.Name)
            }
        ]
