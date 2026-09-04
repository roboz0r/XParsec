module XParsec.FSharp.SemanticAnalysis.Tests.CompositeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `composite` is first-hit-wins. These pin the priority semantics with trivial
// in-line providers, independent of any real `.fsi` extraction.

/// A provider that publishes exactly `name` on the value, type and member channels,
/// tagging each payload with `tag` — a `TyConst` on the value channel, the
/// `Origin.Namespace` on the other two — so the winning source is identifiable.
let private tagged (name: string) (tag: string) : IExternalSymbolProvider =
    let origin =
        { SymbolOrigin.Empty with
            Namespace = SymbolKeyOps.namespaceKey tag
        }

    let typeKey = SymbolKeyOps.qualifiedTypeKeyOf name 0

    let taggedMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf typeKey name EqArray.empty 0 MemberKind.Method) with
            IsStatic = true
            Signature =
                TestHelpers.mkSignature
                    0
                    0
                    (FTConst(RuntimeNames.unitKey, EqArray.empty))
                    (FTConst(RuntimeNames.opaqueKey tag, EqArray.empty))
            Origin = origin
        }

    TestHelpers.providerOfSurface (fun b ->
        PublishedSurfaceBuilder.addValue
            b
            { ExternalSymbols.monoFrozen
                  (SymbolKeyOps.inNamespace "")
                  name
                  (FTConst(RuntimeNames.opaqueKey tag, EqArray.empty)) with
                Origin = origin
            }

        PublishedSurfaceBuilder.addTypeWith
            b
            typeKey
            (ExternalTypeShape.Class
                { ExternalClassShape.basic (TyparKinds.typeOnly 0, ClassCommitment.Class, origin) with
                    Members = EqArray.singleton taggedMember
                })
            [ taggedMember ]
    )

/// The `TyConst` tag carried by a resolved value symbol, for asserting which
/// source won.
let private valueTag (provider: IExternalSymbolProvider) (name: string) : string voption =
    match ScopeContents.tryValueAt provider.Scope name with
    | ValueSome sym ->
        match ExternalSymbols.instantiateSymbol (TypeStore()) sym 0 with
        | TyConst(key, _) ->
            let (DisplayName name) = SymbolKeyOps.typeSimpleName key
            ValueSome name
        | _ -> ValueNone
    | ValueNone -> ValueNone

[<Tests>]
let tests =
    testList
        "Composite"
        [
            test "three-deep: earliest source wins" {
                let a = tagged "shared" "a"
                let b = tagged "shared" "b"
                let c = tagged "shared" "c"

                let composed = ExternalSymbolProviders.composite [ a; b; c ]

                Expect.equal (valueTag composed "shared") (ValueSome "a") "a (first) wins over b and c"
            }

            test "three-deep: a later source resolves when earlier ones miss" {
                // Only `c` knows `late`; the composite must fall through a and b.
                let a = tagged "early" "a"
                let b = tagged "early" "b"
                let c = tagged "late" "c"

                let composed = ExternalSymbolProviders.composite [ a; b; c ]

                Expect.equal (valueTag composed "late") (ValueSome "c") "c resolves after a/b miss"
                Expect.equal (valueTag composed "early") (ValueSome "a") "a still wins its own name"
            }

            test "three-deep: an unknown name misses through every source" {
                let composed =
                    ExternalSymbolProviders.composite [ tagged "x" "a"; tagged "y" "b"; tagged "z" "c" ]

                Expect.equal (valueTag composed "nope") ValueNone "value miss"

                Expect.isTrue
                    (composed.TryLookupType(SymbolKeyOps.qualifiedTypeKeyOf "nope" 0)
                     |> ValueOption.isNone)
                    "type miss"

                Expect.isTrue
                    (composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf "nope" 0, "nope")
                     |> ValueOption.isNone)
                    "member miss"
            }

            test "priority applies to the type and member channels too" {
                let a = tagged "shared" "a"
                let b = tagged "shared" "b"
                let composed = ExternalSymbolProviders.composite [ a; b ]

                match composed.TryLookupType(SymbolKeyOps.qualifiedTypeKeyOf "shared" 0) with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Origin.Namespace.Dotted "a" "type: a wins"
                | other -> failtestf "expected Class shape from a, got %A" other

                match composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf "shared" 0, "shared") with
                | ValueSome m -> Expect.equal m.Origin.Namespace.Dotted "a" "member: a wins"
                | ValueNone -> failtest "expected member from a"
            }

            test "empty list behaves as nullProvider" {
                let composed = ExternalSymbolProviders.composite []

                Expect.isTrue (ScopeContents.tryValueAt composed.Scope "anything" |> ValueOption.isNone) "value miss"

                Expect.isTrue
                    (composed.TryLookupType(SymbolKeyOps.qualifiedTypeKeyOf "anything" 0)
                     |> ValueOption.isNone)
                    "type miss"

                Expect.isTrue
                    (composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf "anything" 0, "x")
                     |> ValueOption.isNone)
                    "member miss"
            }

            test "singleton list delegates to its one source" {
                let composed = ExternalSymbolProviders.composite [ tagged "only" "a" ]

                Expect.equal (valueTag composed "only") (ValueSome "a") "the one source resolves"
                Expect.equal (valueTag composed "other") ValueNone "and nothing else does"
            }
        ]
