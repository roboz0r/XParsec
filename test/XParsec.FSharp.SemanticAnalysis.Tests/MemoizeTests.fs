module XParsec.FSharp.SemanticAnalysis.Tests.MemoizeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `ExternalSymbolProviders.memoize` — the general per-lookup cache. Two properties: it changes
// NO result (a hit or a miss reads the same as the inner provider), and it consults the
// inner AT MOST ONCE per key (subsequent calls, including cached MISSES, never re-hit).
// A counting inner provider pins both.

let private origin = SymbolOrigin.Empty

/// An inner provider that answers only `name` on the value channel and records how many
/// times each channel's factory reached it, so the cache's at-most-once contract is
/// observable.
type private CountingProvider(name: string) =
    let mutable lookupHits = 0
    let mutable typeHits = 0
    let mutable memberKeyHits = 0
    member _.LookupHits = lookupHits
    member _.TypeHits = typeHits
    member _.MemberKeyHits = memberKeyHits

    member private _.TypeByName(n: string) =
        typeHits <- typeHits + 1

        if n = name then
            ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, false, origin)))
        else
            ValueNone

    interface IExternalSymbolProvider

    interface IExternalSymbolResolver with
        member _.TryLookup n =
            lookupHits <- lookupHits + 1

            if n = name then
                ValueSome(
                    ExternalSymbols.monoFrozen
                        (SymbolKeyOps.inNamespace "")
                        n
                        (FTConst(RuntimeNames.opaqueKey "tag", EqArray.empty))
                )
            else
                ValueNone

        member this.TryLookupType(n: string) = this.TypeByName n
        member _.TryLookupUnionCase _ = ValueNone
        member _.TryRecordsWithField _ = [||]
        member _.AmbientOpenPrefixes = []

    interface IExternalSymbolStore with
        member this.TryLookupType(key: SymbolKey) =
            this.TypeByName(SymbolKeyOps.qualifiedName key)

        member _.TryLookupMember(_, _) = ValueNone
        member _.TryLookupMembers(_, _) = [||]

        // Models no members — but the MISS is counted, so the cache's at-most-once
        // contract is observable on this channel too.
        member _.TryLookupMemberByKey(_: MemberKey) =
            memberKeyHits <- memberKeyHits + 1
            ValueNone

        member _.TryLookupIndexSignature _ = []
        member _.TryLookupByKey _ = ValueNone
        member _.IntrinsicReverseCanon = Map.empty
        member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr

[<Tests>]
let tests =
    testList
        "ExternalSymbolProviders.memoize"
        [
            test "a hit reads through to the inner result" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                match cached.TryLookup "known" with
                | ValueSome s ->
                    match ExternalSymbols.instantiateSymbol s 0 with
                    | TyConst(key, _) ->
                        Expect.equal
                            (SymbolKeyOps.simpleName key)
                            (DisplayName "tag")
                            "the inner symbol's payload survives"
                    | other -> failtestf "unexpected realised type %A" other
                | ValueNone -> failtest "known should resolve through the cache"
            }

            test "a repeated hit consults the inner exactly once" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                cached.TryLookup "known" |> ignore
                cached.TryLookup "known" |> ignore
                cached.TryLookup "known" |> ignore

                Expect.equal inner.LookupHits 1 "inner hit once for three lookups"
            }

            test "a repeated MISS is cached too (no re-hit)" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                Expect.isTrue (cached.TryLookup "absent" |> ValueOption.isNone) "miss reads as None"
                cached.TryLookup "absent" |> ignore

                Expect.equal inner.LookupHits 1 "a cached miss does not re-consult the inner"
            }

            test "channels cache independently" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                cached.TryLookup "known" |> ignore
                cached.TryLookup "known" |> ignore
                cached.TryLookupType "known" |> ignore
                cached.TryLookupType "known" |> ignore

                Expect.equal inner.LookupHits 1 "value channel hit once"
                Expect.equal inner.TypeHits 1 "type channel hit once"
            }

            test "the by-key MEMBER channel caches like its siblings" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                let key =
                    SymbolKeyOps.memberKeyOf
                        (SymbolKeyOps.typeKeyOf "Tests" "Widget")
                        "Poke"
                        EqArray.empty
                        0
                        MemberKind.Method

                cached.TryLookupMemberByKey key |> ignore
                cached.TryLookupMemberByKey key |> ignore

                Expect.equal inner.MemberKeyHits 1 "member-by-key channel hit once"
            }
        ]
