module XParsec.FSharp.Codegen.Js.Tests.ArrayIndexMemberTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The array `arr.[i]` READ resolves through a `get_Item` MEMBER on `'T[]`, not the free
// `GetArray` inline. Both emit the same `arr[i]`, so a key-agreement miss would fall back
// to `GetArray` silently; these tests are the ANTI-MASKING proof the member path is taken.

/// The array's member-contract key: the same key the member-access lookup, the consumer
/// contract and the inline-body store all pass to `TryLookupMember`.
let private arrayMemberKey: SymbolKey = RuntimeNames.arrayMemberHostKey

[<Tests>]
let tests =
    testList
        "Codegen.Js ArrayIndexMember"
        [
            // The array's DECLARATION files its shape under the very key a value's type
            // carries, so a support verdict is read off JS's repr rather than off a miss.
            test "the array publishes an intrinsic shape carrying its JS repr under the array key" {
                match jsProvider.Value.TryLookupType(RuntimeNames.arrayKey 1) with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Platform = IntrinsicPlatform.Repr repr
                                                                 }
                                                        }) -> Expect.equal repr "!0[]" "the array's JS platform repr"
                | other -> failtestf "expected `Vesper.[]` as an Intrinsic shape with a JS repr, got %A" other
            }

            // Over the REAL loaded JS-native contract stack, the array's `get_Item`
            // resolves under the bare key `[]` and its lifted inline body rides that very
            // entry, so the store key and the lookup key agree.
            test "the array `get_Item` member resolves and carries its inline body under the bare array key" {
                let provider = jsProvider.Value

                let mem =
                    match provider.TryLookupMember(arrayMemberKey, "get_Item") with
                    | ValueSome m -> m
                    | ValueNone ->
                        failtestf
                            "TryLookupMember(%A, get_Item) MISSED — the `array-index.js.fsi` contract half is absent"
                            arrayMemberKey

                Expect.isTrue
                    mem.InlineBody.IsSome
                    "the `get_Item` entry carries NO inline body — the inline-body store key DISAGREES with the lookup key"
            }

            // The array's own two halves: the key `array-index.js.fsi` publishes and the key
            // the lifted `array-index.js.fs` body was collected under. Compared WHOLE, so an
            // `ArgSig` divergence names itself here.
            test "the array `get_Item` contract-side and impl-side member keys are equal, ArgSig included" {
                expectMemberKeyHalvesAgree jsContract.Value [ vesperCoreManifest ] arrayMemberKey [ "get_Item" ]
            }

            // An `arr.[i]` read must leave an `ExternalAccess` entry for `get_Item`; the
            // `GetArray` fallback resolves through the open scope and leaves none. The
            // entry is written before inline-splicing, so it survives the splice.
            test "`arr.[i]` records a `get_Item` ExternalAccess (member path taken, not the `GetArray` fallback)" {
                let input = "let read (a: int[]) (i: int) : int = a.[i]\n"

                let lexed, file = parseFile input

                let ctx, _ =
                    Pipeline.analyseSemWithContextForCore
                        true
                        ""
                        jsProvider.Value
                        (Hashing.originSourceOfText lexed)
                        file

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

            // The member body is a byte-copy of `GetArray`'s `ldelem`, so the emitted read
            // is still the bare `arr[i]`: the member is SPLICED, never called.
            test "`arr.[i]` still emits the bare computed-member read (no `.get_Item` leak)" {
                let js =
                    emitJs
                        "let read (a: int[]) (i: int) : int = a.[i]\nprintfn \"%d\" (read (# \"newarr !0\" type (int) 1 : int[] #) 0)"

                Expect.isFalse (js.Contains ".get_Item") (sprintf "a `.get_Item` method call leaked into emit:\n%s" js)
                Expect.stringContains js "a[i]" (sprintf "the array index did not lower to `a[i]`:\n%s" js)
            }
        ]
