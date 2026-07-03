module XParsec.FSharp.Codegen.Js.Tests.StructuralShapeHashTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// Isolation fixture: the provider's `toFrozen` gives an anonymous `Structural` shape a
// CANONICAL, field-ORDER-INVARIANT identity, still rehydrated as an OPAQUE `FTUnknown`
// (no member resolution — the shape has no nominal identity yet). Because `FTUnknown`
// unifies by NAME equality, two field-order-permuted shapes must intern to the SAME
// frozen scheme.
// The test reaches `toFrozen` through the public provider path: a `Variable` export's
// scheme IS `toFrozen ctx ty` (`TsManifestProvider`), so a variable typed by a structural
// shape exposes its frozen identity via `TryLookup`.

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
                // A genuinely different shape — must be DISTINCT from pXY.
                var "pXZ" (structural "{x:number;z:number}" [ numField "x"; numField "z" ])
                // Nested structural, permuted at BOTH levels — hashes stably ⇒ equal.
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
                // Named-ref field: `Node` is NOT declared here — it must stay a LEAF
                // (hashed by name, never expanded), so `{node:Node}` resolves without
                // depending on `Node` existing, and differs from `{node:Other}`.
                var "refNode" (structural "{node:Node}" [ "node", named "Node" ])
                var "refNode2" (structural "{node:Node}" [ "node", named "Node" ])
                var "refOther" (structural "{node:Other}" [ "node", named "Other" ])
                // Fieldless structural (the extractor's non-object form) — no usable
                // shape, so identity falls back to the tsc-printed string; two distinct
                // printed strings stay distinct rather than collapsing to one `{}`.
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
    match provider.TryLookup name with
    | ValueSome sym -> sym.Scheme
    | ValueNone -> failtestf "fixture variable '%s' did not resolve" name

/// Assert a scheme is an opaque `FTUnknown` and hand back its identity name.
let private unknownName (name: string) : string =
    match schemeOf name with
    | FTUnknown n -> n
    | other -> failtestf "'%s' should freeze to an opaque FTUnknown, got %A" name other

[<Tests>]
let tests =
    testList
        "StructuralShapeHash"
        [
            test "field-order-permuted shapes share ONE opaque identity" {
                Expect.equal (unknownName "pYX") (unknownName "pXY") "{x;y} and {y;x} must intern to the same FTUnknown"
            }

            test "a shape with different fields is DISTINCT" {
                Expect.notEqual (unknownName "pXZ") (unknownName "pXY") "{x;z} must not alias {x;y}"
            }

            test "nested structural hashes stably under permutation at every level" {
                Expect.equal (unknownName "nB") (unknownName "nA") "nested permuted shapes must share one identity"
            }

            test "a named-ref field stays a LEAF (by-name), not expanded" {
                // Resolving at all with `Node` undeclared proves it is not expanded.
                Expect.equal
                    (unknownName "refNode2")
                    (unknownName "refNode")
                    "two shapes over the same named ref must be equal"

                Expect.notEqual
                    (unknownName "refOther")
                    (unknownName "refNode")
                    "a different named-ref leaf must yield a different identity"
            }

            test "a permuted shape stays OPAQUE — the canonical hash is its name" {
                // The identity is the canonical shape-hash, NOT the tsc-printed string:
                // pXY and pYX carry DIFFERENT printed strings yet one name.
                Expect.stringStarts
                    (unknownName "pXY")
                    "structural:{"
                    "an object shape's identity is the canonical hash"
            }

            test "fieldless structural falls back to the printed string, no collapse" {
                let a = unknownName "fless1"
                let b = unknownName "fless2"
                Expect.notEqual b a "distinct fieldless forms must not collapse to one identity"
                Expect.stringContains a "() => void" "the fieldless fallback carries the tsc-printed string"
            }
        ]
