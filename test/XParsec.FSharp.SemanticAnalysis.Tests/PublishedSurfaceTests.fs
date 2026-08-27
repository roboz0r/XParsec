module XParsec.FSharp.SemanticAnalysis.Tests.PublishedSurfaceTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `PublishedSurface` claims to be a VALUE: key-ordered under an ordinal rendering, and equal
// to another exactly when it publishes the same thing. Both halves are load-bearing for the
// content-addressed key the surface is headed for, and neither is visible from a call site —
// `toProvider` re-indexes into dictionaries and answers the same either way. So they are
// asserted here rather than left to a downstream consumer to discover.

let private key (ns: string) (name: string) (arity: int) : TypeKey =
    SymbolKeyOps.typeKeyOfArity ns name arity

let private shapeOf (arity: int) : ExternalTypeShape =
    ExternalTypeShape.Class(ExternalClassShape.basic (arity, false, SymbolOrigin.Empty))

/// A builder holding three types and two union cases, filled in the order given, so the same
/// content can be published in two different insertion orders.
let private surfaceOf (names: string list) : PublishedSurface =
    let b = PublishedSurfaceBuilder.create ()

    for n in names do
        let k = key "Ns" n 0
        PublishedSurfaceBuilder.addModuleChain b k
        PublishedSurfaceBuilder.addShape b k (shapeOf 0)

        PublishedSurfaceBuilder.addUnionCase
            b
            {
                UnionKey = k
                Case =
                    {
                        Name = "Case" + n
                        FieldNames = EqArray.empty
                        FrozenFieldTypes = EqArray.empty
                    }
                IsRequireQualifiedAccess = false
            }

    PublishedSurface.ofBuilder b

[<Tests>]
let tests =
    testList
        "PublishedSurface"
        [
            test "tables are ordinal key-ordered" {
                let surface = surfaceOf [ "Zeta"; "alpha"; "Beta" ]

                let shapeNames = [ for e in surface.ShapesByKey -> SymbolKeyOps.typeMetaName e.Key ]

                Expect.equal
                    shapeNames
                    (shapeNames |> List.sortWith (fun a b -> System.String.CompareOrdinal(a, b)))
                    "ShapesByKey is ordered by an ordinal rendering of its key"
            }

            test "insertion order does not change what is published" {
                let a = surfaceOf [ "Zeta"; "alpha"; "Beta" ]
                let b = surfaceOf [ "Beta"; "Zeta"; "alpha" ]

                Expect.equal a.ShapesByKey b.ShapesByKey "same shapes, published in a different order"
                Expect.equal a.UnionCases b.UnionCases "same union cases, published in a different order"
                Expect.equal a b "the whole surface compares equal"
            }

            test "different content is a different value" {
                Expect.notEqual (surfaceOf [ "Alpha" ]) (surfaceOf [ "Beta" ]) "one type each, differently named"

                Expect.notEqual
                    (surfaceOf [ "Alpha" ])
                    (surfaceOf [ "Alpha"; "Beta" ])
                    "a surface that publishes strictly more"
            }

            // The one table that is NOT yet a value: a `ValRepr`'s groups hold POOL-RELATIVE
            // handles, and a TUPLE group's carries the live `PoolBuilder` it was minted into,
            // so the same signature published twice is two unequal values. Asserted INVERTED,
            // as the gap it is: publishing a flat parameter grouping — the fix — makes the
            // two equal and fails here, rather than landing unnoticed.
            test "a tupled `ValRepr` still compares by its pool" {
                let symbol (valRepr: TastAccessor.ValRepr voption) =
                    PublishedSurface.build (fun b ->
                        { ExternalSymbols.scheme
                              (SymbolKeyOps.inNamespace "Ns")
                              "f"
                              (ExternalSignature.unfreezable "test placeholder")
                              0
                              [] with
                            ValRepr = valRepr
                        }
                        |> PublishedSurfaceBuilder.addValue b ValueNone
                    )

                // `a * b -> r`: one group of width 2, which mints a tuple PATTERN into a
                // standalone pool. A width-1 group mints a bound var, which is an integer.
                let tupled () =
                    let elem = ExternalSignature.unfreezable "test placeholder"
                    let pair = FTTuple(EqArray.ofArray [| elem; elem |])
                    ValueSome(TastLower.externalValRepr 0 [ (2, pair) ] elem)

                Expect.equal (symbol ValueNone) (symbol ValueNone) "everything BUT the handles compares by contents"

                Expect.notEqual
                    (symbol (tupled ()))
                    (symbol (tupled ()))
                    "the same signature, published twice, while the handles are live"
            }

            // The cons-list exception belongs to `addTypeWith`, so it holds for the same
            // union arriving from `SignatureResolution` and from `FrozenSignature` alike.
            test "the cons-list's cases are published on the shape but not indexed by name" {
                let union =
                    ExternalTypeShape.Union(
                        1,
                        EqArray.ofList
                            [
                                ExternalCaseShape.create (RuntimeNames.consCaseName, EqArray.empty)
                                ExternalCaseShape.create (RuntimeNames.emptyCaseName, EqArray.empty)
                            ],
                        EqArray.empty,
                        SymbolOrigin.Empty,
                        false
                    )

                let listSurface =
                    PublishedSurface.build (fun b -> PublishedSurfaceBuilder.addType b RuntimeNames.vesperListKey union)

                Expect.isEmpty listSurface.UnionCases "no cons-list case is indexed by name"

                Expect.equal
                    (listSurface.ShapesByKey |> EqArray.map (fun e -> e.Value))
                    (EqArray.singleton union)
                    "the shape still carries both cases"

                // The same shape under any other key IS indexed, so the assertion above is
                // the exception firing rather than the whole union going unpublished.
                let otherSurface =
                    PublishedSurface.build (fun b -> PublishedSurfaceBuilder.addType b (key "Ns" "Chain" 1) union)

                Expect.equal otherSurface.UnionCases.Length 2 "an ordinary union of the same shape is indexed"
            }
        ]
