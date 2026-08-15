module XParsec.FSharp.Codegen.Js.Tests.ArrayIndexMemberTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The array `arr.[i]` READ resolves through a `get_Item` MEMBER on `'T[]`. The member is
// SPLICED, so the emitted `arr[i]` looks the same whether the key agreed or not; these tests
// assert the resolution itself, on the halves and on the recorded access.

/// The array's member-contract key: the same key the member-access lookup, the consumer
/// contract and the inline-body store all pass to `TryLookupMember`.
let private arrayMemberKey: TypeKey = RuntimeNames.arrayKey 1

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
                            "TryLookupMember(%A, get_Item) MISSED — the `prim-types-array.fsi` contract half is absent"
                            arrayMemberKey

                Expect.isTrue
                    mem.InlineBody.IsSome
                    "the `get_Item` entry carries NO inline body — the inline-body store key DISAGREES with the lookup key"
            }

            // The array's own two halves: the key `prim-types-array.fsi` publishes and the key
            // the lifted `prim-types-array.fs` body was collected under. Compared WHOLE, so an
            // `ArgSig` divergence names itself here.
            test "the array's accessor contract-side and impl-side member keys are equal, ArgSig included" {
                expectMemberKeyHalvesAgree
                    jsContract.Value
                    [ vesperCorePackage ]
                    arrayMemberKey
                    [ "get_Item"; "set_Item"; "Length" ]
            }

            // The entry is written before inline-splicing, so it survives the splice — which
            // is what makes the member path observable at all after the body is inlined away.
            test "`arr.[i]` records a `get_Item` ExternalAccess" {
                let input = "let read (a: int[]) (i: int) : int = a.[i]\n"

                let lexed, file = parseFile input

                let ctx, _ =
                    Pipeline.analyseSemWithContext jsProvider.Value (Hashing.originSourceOfText lexed) file

                let getItemAccesses =
                    [
                        for KeyValue(_, info) in ctx.Resolution.ExternalAccess.AsDictionary() do
                            if SymbolKeyOps.simpleName info.Key = DisplayName "get_Item" then
                                info
                    ]

                Expect.isNonEmpty getItemAccesses "`arr.[i]` did not record a `get_Item` ExternalAccess"
            }

            // The member body is the bare `ldelem`, so the emitted read is the bare `arr[i]`:
            // the member is SPLICED, never called.
            test "`arr.[i]` still emits the bare computed-member read (no `.get_Item` leak)" {
                let js =
                    emitJs
                        "let read (a: int[]) (i: int) : int = a.[i]\nprintfn \"%d\" (read (# \"newarr !0\" type (int) 1 : int[] #) 0)"

                Expect.isFalse (js.Contains ".get_Item") (sprintf "a `.get_Item` method call leaked into emit:\n%s" js)
                Expect.stringContains js "a[i]" (sprintf "the array index did not lower to `a[i]`:\n%s" js)
            }

            // The write and the length halves of the same claim: both are members on `'T[]`,
            // and both splice, so neither leaves an accessor call behind.
            test "`arr.[i] <- v` and `arr.Length` splice to the bare `a[i] = v` and `.length`" {
                let js =
                    emitJs
                        "let write (a: int[]) (i: int) (v: int) : int =\n    a.[i] <- v\n    a.Length\nprintfn \"%d\" (write (# \"newarr !0\" type (int) 1 : int[] #) 0 7)"

                Expect.isFalse (js.Contains ".set_Item") (sprintf "a `.set_Item` method call leaked into emit:\n%s" js)
                Expect.stringContains js "a[i] = v" (sprintf "the array write did not lower to `a[i] = v`:\n%s" js)
                Expect.stringContains js "a.length" (sprintf "`arr.Length` did not lower to `a.length`:\n%s" js)
            }
        ]
