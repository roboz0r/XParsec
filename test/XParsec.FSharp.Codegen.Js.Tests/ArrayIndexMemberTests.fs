module XParsec.FSharp.Codegen.Js.Tests.ArrayIndexMemberTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// W9 Stage 2a — the array `arr.[i]` READ migrated onto a `get_Item` MEMBER accessor
// on `'T[]` (the landed `IntrinsicAbbrevHost` member-inline mechanism), replacing the
// free `GetArray` inline as the RESOLUTION target on the JS target.
//
// The `GetArray` free function is still present (its deletion is a later stage), so a
// key-agreement MISS would silently fall back to `GetArray` and STILL emit the same
// `arr[i]` — hiding the failure. These tests are the ANTI-MASKING proof that the member
// path is actually taken and the bare-key store/lookup agreement holds.

/// The array's member-contract key — the double-backtick-escaped `arrayName 1`
/// (`` ``[]`` ``), the SAME ordinal string the receiver-side lookup, the consumer
/// contract, and the harvest store all pass to `TryLookupMember`.
let private arrayMemberKey: string = RuntimeNames.arrayContractName

[<Tests>]
let tests =
    testList
        "Codegen.Js ArrayIndexMember"
        [
            // KEY-AGREEMENT PROBE: over the REAL loaded JS-native contract stack, the
            // array's `get_Item` member (a) resolves under the bare key `` ``[]`` `` from the
            // `array-index.js.fsi` contract half, AND (b) its harvested inline body rides
            // THAT VERY ENTRY (`mem.InlineBody`). Store key == lookup key: both are the
            // finalized `SymbolKey` that `TryLookupMember("``[]``", "get_Item")` returns. A
            // disagreement here is exactly the silent-`GetArray`-fallback bug this stage
            // must not introduce.
            test "the array `get_Item` member resolves and carries its inline body under the bare array key" {
                let provider = jsProvider.Value

                let mem =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey arrayMemberKey 0, "get_Item") with
                    | ValueSome m -> m
                    | ValueNone ->
                        failtestf
                            "TryLookupMember(%s, get_Item) MISSED — the `array-index.js.fsi` contract half is absent"
                            arrayMemberKey

                Expect.isTrue
                    mem.InlineBody.IsSome
                    "the `get_Item` entry carries NO inline body — the harvest store key DISAGREES with the lookup key"
            }

            // RESOLVER-PATH PROOF: front-end an `arr.[i]` read and assert Unification
            // recorded an `ExternalAccess` entry for `get_Item` — the trace the member path
            // leaves and the `GetArray` fallback does NOT (it resolves the free `GetArray`
            // through the open scope, never touching `ExternalAccess`). `ExternalAccess` is
            // written by `resolveExternalIndexer` BEFORE inline-splicing, so the entry
            // survives even though Elaborate later splices the member body to `ldelem`.
            test "`arr.[i]` records a `get_Item` ExternalAccess (member path taken, not the `GetArray` fallback)" {
                let input = "let read (a: int[]) (i: int) : int = a.[i]\n"

                let lexed, file = parseFile input

                let ctx, _ =
                    Pipeline.analyseSemWithContextForCore true "" jsProvider.Value input lexed file

                let getItemAccesses =
                    [
                        for KeyValue(_, info) in ctx.Resolution.ExternalAccess.AsDictionary() do
                            if SymbolKeyOps.simpleName info.Key = DisplayName "get_Item" then
                                info
                    ]

                Expect.isNonEmpty
                    getItemAccesses
                    "`arr.[i]` did not record a `get_Item` ExternalAccess — it fell back to the free `GetArray` path"
            }

            // BYTE-IDENTICAL end-to-end: the migrated member body is a byte-copy of
            // `GetArray`'s `ldelem`, so the emitted read is still the bare `arr[i]` — no
            // `.get_Item(` method call leaks (the member is SPLICED, not called).
            test "`arr.[i]` still emits the bare computed-member read (no `.get_Item` leak)" {
                let js =
                    emitJs
                        "let read (a: int[]) (i: int) : int = a.[i]\nprintfn \"%d\" (read (# \"newarr !0\" type (int) 1 : int[] #) 0)"

                Expect.isFalse (js.Contains ".get_Item") (sprintf "a `.get_Item` method call leaked into emit:\n%s" js)
                // The spliced `ldelem` lowers to the bare computed-member read `a[i]` —
                // byte-identical to the free `GetArray` it replaces.
                Expect.stringContains js "a[i]" (sprintf "the array index did not lower to `a[i]`:\n%s" js)
            }
        ]
