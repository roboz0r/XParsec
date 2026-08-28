module XParsec.FSharp.Codegen.Js.Tests.ManifestPrimitiveIdentityTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A manifest-spelled canon name (`string`, `float`, `undefined`, `bigint`) mints its `Vesper`
// identity from the contract-declared intrinsic axis. Negative gates: an undeclared name
// mints opaque, `null` mints the bare `nullKey`, and an empty axis mints every primitive opaque.

let private fn0 (name: string) (ret: string) : Schema.Export =
    Schema.Export.Function(name, [ sig0 (named ret) ], Schema.ImportShape.Named)

let private primsManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "primslib"
        Version = None
        Exports =
            [
                fn0 "wantString" "string"
                fn0 "wantFloat" "float"
                fn0 "wantUndefined" "undefined"
                fn0 "wantBigint" "bigint"
                fn0 "wantNull" "null"
                fn0 "wantMystery" "Mystery"
            ]
        Diagnostics = []
        Refs = []
    }

/// The frozen RETURN of a manifest free function looked up on `prov`'s own scope.
let private returnOf (prov: IExternalSymbolProvider) (name: string) : FrozenType =
    match ScopeContents.tryValueAt prov.Scope name with
    | ValueSome sym ->
        match sym.Scheme with
        | FTFun(_, ret) -> ret
        | other -> failwithf "'%s' scheme is not a function type: %A" name other
    | ValueNone -> failwithf "'%s' did not resolve as a value symbol" name

let private returnKeyOf (prov: IExternalSymbolProvider) (name: string) : TypeKey =
    match returnOf prov name with
    | FTConst(key, _) -> key
    | other -> failwithf "'%s' return is not an FTConst: %A" name other

let private prov: Lazy<IExternalSymbolProvider> = lazy tsProviderOf primsManifest

let private analyse (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast =
        Pipeline.analyseSemFor testCompiling (stackTs primsManifest) (LexedFile.ofText lexed) file

    tast.Diagnostics |> Diagnostic.errors

[<Tests>]
let tests =
    testList
        "ManifestPrimitiveIdentity"
        [
            test "axis-declared canon names mint their Vesper identities" {
                Expect.equal (returnKeyOf prov.Value "wantString") RuntimeNames.stringKey "string"
                Expect.equal (returnKeyOf prov.Value "wantFloat") RuntimeNames.floatKey "float"
                Expect.equal (returnKeyOf prov.Value "wantUndefined") RuntimeNames.undefinedKey "undefined"
                Expect.equal (returnKeyOf prov.Value "wantBigint") RuntimeNames.bigintKey "bigint"
            }

            test "null and an undeclared name mint bare keys" {
                Expect.equal (returnKeyOf prov.Value "wantNull") RuntimeNames.nullKey "null"
                Expect.equal (returnKeyOf prov.Value "wantMystery") (RuntimeNames.opaqueKey "Mystery") "Mystery"
            }

            test "an empty axis mints every primitive opaque" {
                let bare =
                    TsManifestProvider.providerOfManifest IntrinsicTypeMap.empty primsManifest

                Expect.equal (returnKeyOf bare "wantString") (RuntimeNames.opaqueKey "string") "string"
                Expect.equal (returnKeyOf bare "wantFloat") (RuntimeNames.opaqueKey "float") "float"
            }

            test "a manifest string return unifies with the front end's string" {
                let errors =
                    analyse (String.concat "\n" [ "let s: string = wantString()"; "let f: float = wantFloat()"; "" ])

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "the buildContract factory threads the contract axis to the manifest translator" {
                let dir = tmpDir "manifest-primitive-identity"
                let path = System.IO.Path.Combine(dir, "primslib.manifest.json")
                System.IO.File.WriteAllText(path, Codec.serialize primsManifest)

                let contract = TsManifestProvider.buildContract jsPackages [ path ]
                Expect.equal (returnKeyOf contract "wantString") RuntimeNames.stringKey "string"
                Expect.equal (returnKeyOf contract "wantUndefined") RuntimeNames.undefinedKey "undefined"
            }
        ]
