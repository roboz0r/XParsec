module XParsec.FSharp.SemanticAnalysis.Tests.MemoizeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The general per-lookup cache: it changes no result, and consults the inner AT MOST ONCE per
// key — cached MISSES included.

let private origin = SymbolOrigin.Empty

/// An inner provider serving only `name`, counting per-channel hits. It implements the
/// interface directly: a decorated stub derives `TryLookupMemberByKey` from
/// `TryLookupMembers`, so `MemberKeyHits` would count a different channel.
type private CountingProvider(name: string) =
    let mutable valueHits = 0
    let mutable typeHits = 0
    let mutable memberKeyHits = 0
    let mutable valueTypeHits = 0
    let mutable tupleTypeHits = 0

    let scope =
        { new IScopeContents with
            member _.TryContainer _ = ValueNone

            member _.TryValue key =
                valueHits <- valueHits + 1
                let n = key.Name

                if n = name then
                    ValueSome(
                        ExternalSymbols.monoFrozen
                            (SymbolKeyOps.inNamespace "")
                            n
                            (FTConst(RuntimeNames.opaqueKey "tag", EqArray.empty))
                    )
                else
                    ValueNone

            member _.UnionCasesNamed(_, _) = EqArray.empty
            member _.TypesNamed(_, _) = EqArray.empty
            member _.DeclarationsOf _ = EqArray.empty
        }

    member _.ValueHits = valueHits
    member _.TypeHits = typeHits
    member _.MemberKeyHits = memberKeyHits
    member _.ValueTypeHits = valueTypeHits
    member _.TupleTypeHits = tupleTypeHits

    interface IExternalSymbolProvider

    interface IExternalSymbolResolver with
        member _.Scope = scope
        member _.TryRecordsWithField _ = EqArray.empty
        member _.ImplicitOpens = []

    interface IExternalSymbolStore with
        member _.TryLookupType(key: TypeKey) =
            typeHits <- typeHits + 1

            if key.Name = name then
                ValueSome(ExternalTypeShape.Class(ExternalClassShape.basic (0, ClassCommitment.Class, origin)))
            else
                ValueNone

        member _.TryLookupMembers(_, _) = EqArray.empty

        // A miss, but a counted one: at-most-once is observable on this channel too.
        member _.TryLookupMemberByKey(_: MemberKey) =
            memberKeyHits <- memberKeyHits + 1
            ValueNone

        member _.TryLookupIndexSignature _ = []
        member _.TryLookupByKey _ = ValueNone
        member _.IntrinsicTypeMap = IntrinsicTypeMap.empty
        member this.Platform = ValueSome(this :> IPlatformFacts)

    interface IPlatformFacts with

        // A miss, but a counted one: a provider with no opinion is asked at most once too.
        member _.IsValueType _ =
            valueTypeHits <- valueTypeHits + 1
            ValueNone

        // Counted for the OPPOSITE reason: this channel is deliberately NOT cached, so a
        // second ask must reach through.
        member _.TupleType _ =
            tupleTypeHits <- tupleTypeHits + 1
            ValueNone

[<Tests>]
let tests =
    testList
        "ExternalSymbolProviders.memoize"
        [
            test "a hit reads through to the inner result" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                match ScopeContents.tryValueAt cached.Scope "known" with
                | ValueSome s ->
                    match ExternalSymbols.instantiateSymbol (TypeStore()) s 0 with
                    | TyConst(key, _) ->
                        Expect.equal
                            (SymbolKeyOps.typeSimpleName key)
                            (DisplayName "tag")
                            "the inner symbol's payload survives"
                    | other -> failtestf "unexpected instantiated type %A" other
                | ValueNone -> failtest "known should resolve through the cache"
            }

            test "a repeated hit consults the inner exactly once" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                ScopeContents.tryValueAt cached.Scope "known" |> ignore
                ScopeContents.tryValueAt cached.Scope "known" |> ignore
                ScopeContents.tryValueAt cached.Scope "known" |> ignore

                Expect.equal inner.ValueHits 1 "inner hit once for three lookups"
            }

            test "a repeated MISS is cached too (no re-hit)" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                Expect.isTrue
                    (ScopeContents.tryValueAt cached.Scope "absent" |> ValueOption.isNone)
                    "miss reads as None"

                ScopeContents.tryValueAt cached.Scope "absent" |> ignore

                Expect.equal inner.ValueHits 1 "a cached miss does not re-consult the inner"
            }

            test "channels cache independently" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                let knownKey = SymbolKeyOps.typeKeyOf "" "known"
                ScopeContents.tryValueAt cached.Scope "known" |> ignore
                ScopeContents.tryValueAt cached.Scope "known" |> ignore
                cached.TryLookupType knownKey |> ignore
                cached.TryLookupType knownKey |> ignore

                Expect.equal inner.ValueHits 1 "value channel hit once"
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

            test "the VALUE-TYPE channel caches like its siblings" {
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner
                let key = SymbolKeyOps.typeKeyOf "Tests" "Widget"

                cached.IsValueType key |> ignore
                cached.IsValueType key |> ignore

                Expect.equal inner.ValueTypeHits 1 "value-type channel hit once"
            }

            test "the TUPLE-TYPE channel is deliberately not cached" {
                // Pinned so re-adding a cache is a deliberate decision.
                let inner = CountingProvider "known"
                let cached = ExternalSymbolProviders.memoize inner

                match cached.Platform with
                | ValueSome facts ->
                    facts.TupleType 2 |> ignore
                    facts.TupleType 2 |> ignore

                    Expect.equal inner.TupleTypeHits 2 "tuple-type channel reached through both times"
                | ValueNone -> failtest "the counting provider publishes platform facts"
            }
        ]
