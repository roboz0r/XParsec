module XParsec.FSharp.Codegen.Js.Tests.StructuralNominalTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS provider: an anonymous object shape (`{x:int;y:int}`) is a RESOLVABLE erasing
// nominal. Its fields register as Property members (one per field), so `.x` on a
// structural value resolves through the provider and lowers to a native `objArg.x`
// read — while NOTHING is emitted for the type (no decl, no import, no ctor). Two
// distinct exports of the SAME shape resolve to ONE identity/type, and a nested shape
// registers members at BOTH levels.

let private intT = named "int"

/// The `{x:int;y:int}` shape, in the given field order.
let private point (order: (string * Schema.TypeRef) list) : Schema.TypeRef =
    Schema.TypeRef.Structural("{x:int;y:int}", order, [])

let private xThenY = [ "x", intT; "y", intT ]
let private yThenX = [ "y", intT; "x", intT ]

/// `pointlib`: a `getPoint(): {x:int;y:int}` factory plus two variables typed by the
/// same shape (permuted) and a nested `{pt:{x:int;y:int};label:string}` variable.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "pointlib"
        Version = None
        Exports =
            [
                Schema.Export.Function("getPoint", [ sig0 (point xThenY) ], Schema.ImportShape.Named)
                Schema.Export.Variable("pt1", point xThenY, true, Schema.ImportShape.Named)
                // SAME shape, fields permuted — must resolve to the SAME nominal type.
                Schema.Export.Variable("pt2", point yThenX, true, Schema.ImportShape.Named)
                Schema.Export.Variable(
                    "nested",
                    Schema.TypeRef.Structural(
                        "{pt:{x:int;y:int};label:string}",
                        [ "pt", point xThenY; "label", named "string" ],
                        []
                    ),
                    true,
                    Schema.ImportShape.Named
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private provider: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest manifest

/// The qualified nominal name a structural-typed variable froze to (its `FTClass` key).
let private typeNameOf (varName: string) : string =
    match provider.TryLookup varName with
    | ValueSome sym ->
        match sym.Scheme with
        | FTClass(key, _) -> SymbolKeyOps.typeMetaName key
        | other -> failtestf "'%s' should freeze to an FTClass, got %A" varName other
    | ValueNone -> failtestf "variable '%s' did not resolve" varName

// A runtime whose factory returns a POJO with own `x`/`y` props — a native member READ
// (not a mangled import) is the only lowering that can observe `this`-free data props.
let private runtime = "export function getPoint() { return { x: 3, y: 4 }; }\n"

let private emitPoint (input: string) : string =
    emitWith
        (contractTs manifest)
        (Map.ofList
            [
                "pointlib",
                {
                    FileName = "pointlib.mjs"
                    Source = runtime
                }
            ])
        true
        input

let private harness =
    "import { result } from \"./point-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "StructuralNominal"
        [
            test "a structural field registers as a Property member of the right type" {
                let tn = typeNameOf "pt1"

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey tn 0, "x") with
                | ValueSome m ->
                    Expect.isTrue m.Storage.IsValueMember "a structural field must be a value (property) member"

                    match m.Signature.Return with
                    | FTConst(key, _) when SymbolKeyOps.simpleName key = DisplayName "int" -> ()
                    | other -> failtestf "field 'x' should carry its int type, got %A" other
                | ValueNone -> failtestf "field 'x' did not resolve on '%s'" tn
            }

            test "two exports of the SAME shape resolve to ONE nominal identity" {
                Expect.equal (typeNameOf "pt2") (typeNameOf "pt1") "permuted-field exports must share one nominal type"
            }

            test "a nested shape registers members at BOTH levels" {
                let outer = typeNameOf "nested"
                // The outer `.pt` Property carries the INNER structural nominal as its type.
                let innerName =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey outer 0, "pt") with
                    | ValueSome m ->
                        match m.Signature.Return with
                        | FTClass(key, _) -> SymbolKeyOps.typeMetaName key
                        | other -> failtestf "'.pt' should carry a structural FTClass, got %A" other
                    | ValueNone -> failtestf "'.pt' did not resolve on '%s'" outer

                // The inner nominal is also registered — its own `.x` resolves.
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey innerName 0, "x") with
                | ValueSome m -> Expect.isTrue m.Storage.IsValueMember "inner '.x' must be a property member"
                | ValueNone -> failtestf "inner field 'x' did not resolve on '%s'" innerName

                // The inner nominal is the SAME identity as the standalone `{x;y}` shape.
                Expect.equal innerName (typeNameOf "pt1") "the nested inner shape must share the flat shape's identity"
            }

            test "`.x` lowers to a native objArg.x read; the type emits nothing" {
                let program = String.concat "\n" [ "let p = getPoint()"; "let result = p.x"; "" ]
                let js = emitPoint program

                Expect.isTrue (js.Contains ".x") (sprintf "expected a native `.x` read, got:\n%s" js)

                // No mangled member import, and NOTHING emitted for the erased shape: no
                // synthetic-home import, no ctor/class decl for the anonymous type.
                Expect.isFalse (js.Contains "@struct") (sprintf "the erased shape must not appear in emit:\n%s" js)
                Expect.isFalse (js.Contains "class ") (sprintf "the structural type must emit no class decl:\n%s" js)

                expectNodeOutput
                    "structural-nominal"
                    [ "harness.mjs", harness; "point-program.mjs", js; "pointlib.mjs", runtime ]
                    "3"
            }
        ]
