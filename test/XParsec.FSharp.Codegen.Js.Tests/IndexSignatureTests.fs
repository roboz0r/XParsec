module XParsec.FSharp.Codegen.Js.Tests.IndexSignatureTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A TS index signature `{ [k: K]: V }` gives `x.[k]` reads and `x.[k] <- v` writes,
// lowering to the JS bracket `obj[k]` / `obj[k] = v`; there is no `get_Item` method,
// bracket IS the accessor. A read yields the declared value type, never `dynamic`.

/// `ixlib`: index-signature carriers, named (`Dict`, `EnvDict`) and anonymous structural
/// (`bag`, `cfg`, `lookup`, `lookupOpt`), plus `wantNumber`/`wantString`/`wantStringOpt`
/// slots that admit exactly one type, so a read's element type is pinned by which passes.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "ixlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface("Dict", 0, [], [], [ named "string", named "number" ])
                Schema.Export.Interface(
                    "EnvDict",
                    0,
                    [],
                    [],
                    [ named "string", union [ named "string"; named "undefined" ] ]
                )
                Schema.Export.Variable("dict", named "Dict", true, Schema.ImportShape.Named)
                Schema.Export.Variable("env", named "EnvDict", true, Schema.ImportShape.Named)
                Schema.Export.Variable(
                    "bag",
                    structuralIx
                        "{tag:string;[k:string]:number}"
                        [ "tag", named "string" ]
                        [ named "string", named "number" ],
                    true,
                    Schema.ImportShape.Named
                )
                Schema.Export.Variable(
                    "cfg",
                    structural "{foo:string|undefined}" [ "foo", union [ named "string"; named "undefined" ] ],
                    true,
                    Schema.ImportShape.Named
                )
                Schema.Export.Variable(
                    "lookup",
                    structuralIx "{[k:string]:number}" [] [ named "string", named "number" ],
                    true,
                    Schema.ImportShape.Named
                )
                Schema.Export.Variable(
                    "lookupOpt",
                    structuralIx
                        "{[k:string]:string|undefined}"
                        []
                        [ named "string", union [ named "string"; named "undefined" ] ],
                    true,
                    Schema.ImportShape.Named
                )
                Schema.Export.Function(
                    "wantNumber",
                    [ sig1 "x" (named "number") (named "unit") ],
                    Schema.ImportShape.Named
                )
                Schema.Export.Function(
                    "wantString",
                    [ sig1 "x" (named "string") (named "unit") ],
                    Schema.ImportShape.Named
                )
                Schema.Export.Function(
                    "wantStringOpt",
                    [ sig1 "x" (union [ named "string"; named "undefined" ]) (named "unit") ],
                    Schema.ImportShape.Named
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private contract = contractTs manifest

let private provider: IExternalSymbolProvider = contract.Provider

let private analyseErrors (input: string) : string list =
    analyseWith provider input |> List.map (fun d -> d.Message)

/// Emit through the `ixlib` provider with a stub runtime module, so the variable and
/// function imports resolve (the synthetic package has no `.toml` asset).
let private emitIx (input: string) : string =
    emitWith contract (Map.ofList [ "ixlib", JsPackageOutput.rootModule "ixlib.mjs" "" ]) false input

[<Tests>]
let tests =
    testList
        "IndexSignature"
        [
            test "reading `x.[k]` on a numeric index signature infers the value type" {
                // `wantNumber(d.[k])` admits only if the read is `number`; `wantString(d.[k])`
                // must reject, proving the read is `number` and not `dynamic`, which would
                // admit both.
                let ok = analyseErrors "let d = dict\nwantNumber(d.[\"k\"])\n"
                Expect.isEmpty ok (sprintf "an index read should be `number`, got: %A" ok)

                let bad = analyseErrors "let d = dict\nwantString(d.[\"k\"])\n"
                Expect.isNonEmpty bad "a `number` index read must not satisfy a `string` parameter"
            }

            test "reading `x.[k]` lowers to the JS bracket `x[k]`" {
                let js = emitIx "let d = dict\nlet n = d.[\"k\"]\n"
                Expect.stringContains js "[(\"k\")]" (sprintf "expected a bracket read `d[\"k\"]`, got:\n%s" js)
            }

            test "writing `x.[k] <- v` type-checks and lowers to a bracket assignment" {
                // A non-array object declares no `set_Item`, so a string-keyed write
                // type-checks only through `SetIndex`; the green check plus the bracket
                // assignment pins that path.
                let errs = analyseErrors "let d = dict\nd.[\"k\"] <- 5.0\n"
                Expect.isEmpty errs (sprintf "an index write should type-check, got: %A" errs)

                let js = emitIx "let d = dict\nd.[\"k\"] <- 5.0\n"
                // The operands hoist to temps, so the shape to match is the bracket-index
                // assignment `(…)[(…)] = (…)`.
                Expect.stringContains js ")] = (" (sprintf "expected a bracket write `x[k] = v`, got:\n%s" js)
            }

            test "an index value of `string | undefined` reads the undefined-bearing type" {
                // The `process.env` shape: the read carries `| undefined`.
                let ok = analyseErrors "let e = env\nwantStringOpt(e.[\"PATH\"])\n"
                Expect.isEmpty ok (sprintf "a `string | undefined` read should satisfy the optional slot, got: %A" ok)

                let bad = analyseErrors "let e = env\nwantString(e.[\"PATH\"])\n"
                Expect.isNonEmpty bad "a `string | undefined` read must not satisfy a bare `string` parameter"
            }

            test "an anonymous structural shape's index signature reads through the bracket" {
                // `bag` is `{ tag: string; [k: string]: number }`: the index and the named
                // field both resolve.
                let errs = analyseErrors "let b = bag\nwantNumber(b.[\"any\"])\nwantString(b.tag)\n"
                Expect.isEmpty errs (sprintf "structural index read + field read should type-check, got: %A" errs)

                let js = emitIx "let b = bag\nlet n = b.[\"any\"]\n"

                Expect.stringContains
                    js
                    "[(\"any\")]"
                    (sprintf "expected a bracket read on the structural shape, got:\n%s" js)
            }

            test "a structural field carrying `T | undefined` is seen as the union (optional graduation)" {
                let ok = analyseErrors "let c = cfg\nwantStringOpt(c.foo)\n"

                Expect.isEmpty
                    ok
                    (sprintf "the `foo: string | undefined` field should satisfy the optional slot, got: %A" ok)

                let bad = analyseErrors "let c = cfg\nwantString(c.foo)\n"
                Expect.isNonEmpty bad "a `string | undefined` field must not satisfy a bare `string` parameter"
            }

            test "a FIELDLESS index shape `{ [k: string]: number }` reads through the bracket" {
                // A bare `Record<string, number>` has no named fields, so its index is its
                // whole content; `x.[k]` must still infer `number` rather than `dynamic`.
                let ok = analyseErrors "let l = lookup\nwantNumber(l.[\"k\"])\n"
                Expect.isEmpty ok (sprintf "a fieldless index read should be `number`, got: %A" ok)

                let bad = analyseErrors "let l = lookup\nwantString(l.[\"k\"])\n"

                Expect.isNonEmpty
                    bad
                    "a fieldless `number` index read must not satisfy a `string` parameter (not `dynamic`)"

                let js = emitIx "let l = lookup\nlet n = l.[\"k\"]\n"

                Expect.stringContains
                    js
                    "[(\"k\")]"
                    (sprintf "expected a bracket read on the fieldless index shape, got:\n%s" js)
            }

            test "a fieldless `{ [k: string]: string | undefined }` read carries the union" {
                let ok = analyseErrors "let l = lookupOpt\nwantStringOpt(l.[\"k\"])\n"

                Expect.isEmpty
                    ok
                    (sprintf "a fieldless `string | undefined` read should satisfy the optional slot, got: %A" ok)

                let bad = analyseErrors "let l = lookupOpt\nwantString(l.[\"k\"])\n"
                Expect.isNonEmpty bad "a fieldless `string | undefined` read must not satisfy a bare `string` parameter"
            }
        ]
