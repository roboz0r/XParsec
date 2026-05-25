module XParsec.FSharp.SemanticAnalysis.Tests.CompositeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `ExternalSymbols.composite` is the N-deep, first-hit-wins generalisation of
// `FSharpLib.chain` (symbol-resolution-plan §5 / P1). These tests pin the
// priority semantics with trivial in-line providers, independent of any real
// `.fsi` extraction.

/// A provider that answers exactly `name` (value, type, and member channels)
/// with a `TyConst tag` payload, so a winning source is identifiable by its tag.
let private tagged (name: string) (tag: string) : IExternalSymbolProvider =
    let origin =
        { SymbolOrigin.Empty with
            Namespace = tag
        }

    { new IExternalSymbolProvider with
        member _.TryLookup n =
            if n = name then
                ValueSome
                    { ExternalSymbols.mono name (TyConst tag) with
                        Origin = origin
                    }
            else
                ValueNone

        member _.TryLookupType n =
            if n = name then
                ValueSome(ExternalTypeShape.Class(0, false, origin))
            else
                ValueNone

        member _.TryLookupMember(t, m) =
            if t = name && m = name then
                ValueSome
                    {
                        Name = name
                        IsStatic = true
                        IsProperty = false
                        BuildSignature = fun _ -> TyConst tag
                        Origin = origin
                    }
            else
                ValueNone
    }

/// The `TyConst` tag carried by a resolved value symbol, for asserting which
/// source won.
let private valueTag (provider: IExternalSymbolProvider) (name: string) : string voption =
    match provider.TryLookup name with
    | ValueSome sym ->
        match sym.Instantiate 0 with
        | TyConst tag -> ValueSome tag
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

                let composed = ExternalSymbols.composite [ a; b; c ]

                Expect.equal (valueTag composed "shared") (ValueSome "a") "a (first) wins over b and c"
            }

            test "three-deep: a later source answers when earlier ones miss" {
                // Only `c` knows `late`; the composite must fall through a and b.
                let a = tagged "early" "a"
                let b = tagged "early" "b"
                let c = tagged "late" "c"

                let composed = ExternalSymbols.composite [ a; b; c ]

                Expect.equal (valueTag composed "late") (ValueSome "c") "c answers after a/b miss"
                Expect.equal (valueTag composed "early") (ValueSome "a") "a still wins its own name"
            }

            test "three-deep: an unknown name misses through every source" {
                let composed =
                    ExternalSymbols.composite [ tagged "x" "a"; tagged "y" "b"; tagged "z" "c" ]

                Expect.equal (valueTag composed "nope") ValueNone "value miss"
                Expect.isTrue (composed.TryLookupType "nope" |> ValueOption.isNone) "type miss"
                Expect.isTrue (composed.TryLookupMember("nope", "nope") |> ValueOption.isNone) "member miss"
            }

            test "priority applies to the type and member channels too" {
                let a = tagged "shared" "a"
                let b = tagged "shared" "b"
                let composed = ExternalSymbols.composite [ a; b ]

                match composed.TryLookupType "shared" with
                | ValueSome(ExternalTypeShape.Class(_, _, origin)) -> Expect.equal origin.Namespace "a" "type: a wins"
                | other -> failtestf "expected Class shape from a, got %A" other

                match composed.TryLookupMember("shared", "shared") with
                | ValueSome m -> Expect.equal m.Origin.Namespace "a" "member: a wins"
                | ValueNone -> failtest "expected member from a"
            }

            test "empty list behaves as nullProvider" {
                let composed = ExternalSymbols.composite []

                Expect.isTrue (composed.TryLookup "anything" |> ValueOption.isNone) "value miss"
                Expect.isTrue (composed.TryLookupType "anything" |> ValueOption.isNone) "type miss"
                Expect.isTrue (composed.TryLookupMember("anything", "x") |> ValueOption.isNone) "member miss"
            }

            test "singleton list delegates to its one source" {
                let composed = ExternalSymbols.composite [ tagged "only" "a" ]

                Expect.equal (valueTag composed "only") (ValueSome "a") "the one source answers"
                Expect.equal (valueTag composed "other") ValueNone "and nothing else does"
            }

            test "chain is composite of exactly two, same ordering" {
                // The P1 invariant: existing 2-deep `chain` tests pass via
                // `composite [a; b]`. `chain` must agree name-for-name.
                let a = tagged "shared" "a"
                let b = tagged "shared" "b"

                let chained = FSharpLib.chain a b
                let composed = ExternalSymbols.composite [ a; b ]

                Expect.equal (valueTag chained "shared") (valueTag composed "shared") "shared name agrees"
                Expect.equal (valueTag chained "shared") (ValueSome "a") "primary wins in chain"
            }
        ]
