module XParsec.FSharp.SemanticAnalysis.Tests.CompositeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `ExternalSymbolProviders.composite` is the first-hit-wins composition primitive
// These tests pin the priority semantics
// with trivial in-line providers, independent of any real `.fsi` extraction.

/// A provider that answers exactly `name` (value, type, and member channels)
/// with a `TyConst tag` payload, so a winning source is identifiable by its tag.
let private tagged (name: string) (tag: string) : IExternalSymbolProvider =
    let origin =
        { SymbolOrigin.Empty with
            Namespace = SymbolKeyOps.namespaceKey tag
        }

    // The tag rides each channel's payload (the `TyConst` on the value channel, the
    // `Origin.Namespace` on the type/member ones), so a winning source stays
    // distinguishable through `composite`'s first-hit-wins fall-through.
    let taggedMember (t: string) (m: string) : ExternalMember voption =
        if t = name && m = name then
            ValueSome
                { ExternalMember.OfKey(
                      SymbolKeyOps.memberKeyOf
                          (SymbolKeyOps.typeKeyOf origin.Namespace.Dotted name)
                          name
                          EqArray.empty
                          0
                          MemberKind.Method
                  ) with
                    IsStatic = true
                    Signature =
                        TestHelpers.mkSignature
                            0
                            0
                            (FTConst(RuntimeNames.unitKey, EqArray.empty))
                            (FTConst(RuntimeNames.opaqueKey tag, EqArray.empty))
                    Origin = origin
                }
        else
            ValueNone

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun n ->
                    if n = name then
                        ValueSome
                            { ExternalSymbols.monoFrozen
                                  (SymbolKeyOps.inNamespace "")
                                  name
                                  (FTConst(RuntimeNames.opaqueKey tag, EqArray.empty)) with
                                Origin = origin
                            }
                    else
                        ValueNone
            TryLookupType =
                fun n ->
                    if n = name then
                        ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, false, origin)))
                    else
                        ValueNone
            TryLookupMember = fun (t, m) -> taggedMember t m
            TryLookupMembers =
                fun (t, m) ->
                    match taggedMember t m with
                    | ValueSome mem -> [| mem |]
                    | ValueNone -> [||]
        }

/// The `TyConst` tag carried by a resolved value symbol, for asserting which
/// source won.
let private valueTag (provider: IExternalSymbolProvider) (name: string) : string voption =
    match provider.TryLookup name with
    | ValueSome sym ->
        match ExternalSymbols.instantiateSymbol sym 0 with
        | TyConst(key, _) ->
            let (DisplayName name) = SymbolKeyOps.simpleName key
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

            test "three-deep: a later source answers when earlier ones miss" {
                // Only `c` knows `late`; the composite must fall through a and b.
                let a = tagged "early" "a"
                let b = tagged "early" "b"
                let c = tagged "late" "c"

                let composed = ExternalSymbolProviders.composite [ a; b; c ]

                Expect.equal (valueTag composed "late") (ValueSome "c") "c answers after a/b miss"
                Expect.equal (valueTag composed "early") (ValueSome "a") "a still wins its own name"
            }

            test "three-deep: an unknown name misses through every source" {
                let composed =
                    ExternalSymbolProviders.composite [ tagged "x" "a"; tagged "y" "b"; tagged "z" "c" ]

                Expect.equal (valueTag composed "nope") ValueNone "value miss"
                Expect.isTrue (composed.TryLookupType "nope" |> ValueOption.isNone) "type miss"

                Expect.isTrue
                    (composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "nope" 0, "nope")
                     |> ValueOption.isNone)
                    "member miss"
            }

            test "priority applies to the type and member channels too" {
                let a = tagged "shared" "a"
                let b = tagged "shared" "b"
                let composed = ExternalSymbolProviders.composite [ a; b ]

                match composed.TryLookupType "shared" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Origin.Namespace.Dotted "a" "type: a wins"
                | other -> failtestf "expected Class shape from a, got %A" other

                match composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "shared" 0, "shared") with
                | ValueSome m -> Expect.equal m.Origin.Namespace.Dotted "a" "member: a wins"
                | ValueNone -> failtest "expected member from a"
            }

            test "empty list behaves as nullProvider" {
                let composed = ExternalSymbolProviders.composite []

                Expect.isTrue (composed.TryLookup "anything" |> ValueOption.isNone) "value miss"
                Expect.isTrue (composed.TryLookupType "anything" |> ValueOption.isNone) "type miss"

                Expect.isTrue
                    (composed.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "anything" 0, "x")
                     |> ValueOption.isNone)
                    "member miss"
            }

            test "singleton list delegates to its one source" {
                let composed = ExternalSymbolProviders.composite [ tagged "only" "a" ]

                Expect.equal (valueTag composed "only") (ValueSome "a") "the one source answers"
                Expect.equal (valueTag composed "other") ValueNone "and nothing else does"
            }
        ]
