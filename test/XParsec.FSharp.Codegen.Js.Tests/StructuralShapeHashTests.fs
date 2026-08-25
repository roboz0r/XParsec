module XParsec.FSharp.Codegen.Js.Tests.StructuralShapeHashTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A `Variable` export's scheme is its frozen type, so reading it off the provider's scope is
// how these fixtures observe what an anonymous object shape froze to.

/// A `number`-typed field named `n`.
let private numField (n: string) : string * Schema.TypeRef = n, named "number"

/// One variable export named `n`, typed by the structural type `ty`.
let private var (n: string) (ty: Schema.TypeRef) : Schema.Export =
    Schema.Export.Variable(n, ty, true, Schema.ImportShape.Named)

/// A manifest gathering every fixture variable under one provider.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "shapes"
        Version = None
        Exports =
            [
                // {x:number;y:number} in both field orders — must share one identity.
                var "pXY" (structural "{x:number;y:number}" [ numField "x"; numField "y" ])
                var "pYX" (structural "{y:number;x:number}" [ numField "y"; numField "x" ])
                // A genuinely different shape, so it must be DISTINCT from pXY.
                var "pXZ" (structural "{x:number;z:number}" [ numField "x"; numField "z" ])
                // Nested structural, permuted at BOTH levels, so it hashes stably ⇒ equal.
                var
                    "nA"
                    (structural
                        "{pt:{x:number;y:number};label:string}"
                        [
                            "pt", structural "{x:number;y:number}" [ numField "x"; numField "y" ]
                            "label", named "string"
                        ])
                var
                    "nB"
                    (structural
                        "{label:string;pt:{y:number;x:number}}"
                        [
                            "label", named "string"
                            "pt", structural "{y:number;x:number}" [ numField "y"; numField "x" ]
                        ])
                // `Node` is NOT declared here, so the field must stay hashed BY NAME:
                // `{node:Node}` resolves without `Node` existing, and differs from
                // `{node:Other}`.
                var "refNode" (structural "{node:Node}" [ "node", named "Node" ])
                var "refNode2" (structural "{node:Node}" [ "node", named "Node" ])
                var "refOther" (structural "{node:Other}" [ "node", named "Other" ])
                // A fieldless structural has no usable shape, so identity falls back to the
                // tsc-printed string; two distinct printed strings stay distinct rather
                // than collapsing to one `{}`.
                var "fless1" (structural "() => void" [])
                var "fless2" (structural "Branded<string>" [])
            ]
        Diagnostics = []
        Refs = []
    }

let private provider: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest manifest

/// The frozen scheme a fixture variable resolves to.
let private schemeOf (name: string) : FrozenType =
    match ScopeContents.tryValueAt provider.Scope name with
    | ValueSome sym -> sym.Scheme
    | ValueNone -> failtestf "fixture variable '%s' did not resolve" name

/// `FTClass` where the object shape has members to resolve, `FTConst` where it has none: the
/// erasing nominal's qualified name either way, so assertions read uniformly across both.
let private identityOf (name: string) : string =
    match schemeOf name with
    | FTClass(key, _) -> "class:" + SymbolKeyOps.typeMetaName key
    | FTConst(key, _) -> "const:" + SymbolKeyOps.typeMetaName key
    | other -> failtestf "'%s' froze to an unexpected scheme %A" name other

[<Tests>]
let tests =
    testList
        "StructuralShapeHash"
        [
            test "an object shape with members freezes to a nominal FTClass" {
                match schemeOf "pXY" with
                | FTClass _ -> ()
                | other -> failtestf "an object shape must freeze to FTClass, got %A" other
            }

            test "field-order-permuted shapes share ONE nominal identity" {
                Expect.equal (identityOf "pYX") (identityOf "pXY") "{x;y} and {y;x} must intern to the same identity"
            }

            test "a shape with different fields is DISTINCT" {
                Expect.notEqual (identityOf "pXZ") (identityOf "pXY") "{x;z} must not alias {x;y}"
            }

            test "nested structural hashes stably under permutation at every level" {
                Expect.equal (identityOf "nB") (identityOf "nA") "nested permuted shapes must share one identity"
            }

            test "a named-ref field stays hashed BY NAME, not expanded" {
                // Resolving at all with `Node` undeclared proves it is not expanded.
                Expect.equal
                    (identityOf "refNode2")
                    (identityOf "refNode")
                    "two shapes over the same named ref must be equal"

                Expect.notEqual
                    (identityOf "refOther")
                    (identityOf "refNode")
                    "a different named ref must yield a different identity"
            }

            test "an object shape's nominal name is homed under the reserved synthetic namespace" {
                // Homed apart so a shape-hash cannot collide with a real export's name.
                Expect.stringStarts (identityOf "pXY") "class:@struct." "an object shape homes under @struct"
            }

            // A fieldless shape has no members to resolve, so it freezes opaque. It is still a
            // TYPE with an identity: it homes under `@struct` and unifies only with itself.
            test "fieldless structural freezes opaque, homed and distinct" {
                let a = identityOf "fless1"
                let b = identityOf "fless2"
                Expect.stringStarts a "const:@struct." "a fieldless structural homes under @struct"
                Expect.notEqual b a "distinct fieldless forms must not collapse to one identity"
                Expect.stringContains a "() => void" "the fieldless fallback carries the tsc-printed string"
            }

            // Being a type rather than an untyped position is what buys this: a ground type
            // encodes into a signature and unifies, an `FTUnknown` does neither.
            test "a fieldless structural is a ground type" {
                for n in [ "fless1"; "fless2" ] do
                    Expect.isTrue (FrozenTypeBridge.ftIsGround (schemeOf n)) (sprintf "'%s' is ground" n)
            }
        ]
