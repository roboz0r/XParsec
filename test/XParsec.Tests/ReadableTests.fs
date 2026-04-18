module ReadableTests

open System
open System.Collections.Immutable

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.CharParsers
open XParsec.Parsers

#if FABLE_COMPILER
module Expect =
    let sequenceEqual (actual: 'T seq) (expected: 'T seq) (message: string) =
        let actualList = Seq.toList actual
        let expectedList = Seq.toList expected

        if actualList.Length <> expectedList.Length then
            failwithf
                "%s: Sequence lengths differ. Expected %d but got %d."
                message
                expectedList.Length
                actualList.Length
        else
            for i in 0 .. actualList.Length - 1 do
                if not (actualList.[i] = expectedList.[i]) then
                    failwithf
                        "%s: Sequence items differ at index %d. Expected %A but got %A."
                        message
                        i
                        expectedList.[i]
                        actualList.[i]
#endif

/// <summary>
/// Asserts that an implementation of IReadable adheres to the required semantics:
/// bounds checking, length clamping, exception throwing, and anti-escape nested slicing.
/// </summary>
let assertReadableSemantics (name: string) (readable: #IReadable<'T, 'Slice>) (expected: 'T array) =
    let len = expected.Length

    // 1. Length Validation
    Expect.equal readable.Length len $"{name}: Length should match expected array length."

    // 2. Valid Item & TryItem access
    for i in 0 .. len - 1 do
        Expect.equal (readable.Item i) expected.[i] $"{name}: Item({i}) should map correctly."
        Expect.equal (readable.TryItem i) (ValueSome expected.[i]) $"{name}: TryItem({i}) should map correctly."

    // 3. Safe Out-of-bounds TryItem (should NOT throw, must return ValueNone)
    Expect.equal (readable.TryItem -1) ValueNone $"{name}: TryItem(-1) should return ValueNone safely."
    Expect.equal (readable.TryItem len) ValueNone $"{name}: TryItem({len}) should return ValueNone safely."
    Expect.equal (readable.TryItem(len + 10)) ValueNone $"{name}: TryItem({len + 10}) should return ValueNone safely."

    // 4. Unsafe Out-of-bounds Item (MUST throw exception)
    Expect.throws (fun () -> readable.Item -1 |> ignore) $"{name}: Item(-1) must throw."
    Expect.throws (fun () -> readable.Item len |> ignore) $"{name}: Item({len}) must throw."

    // 5. SpanSlice bounds and clamping
    let fullSpan = readable.SpanSlice(0, len)
    Expect.sequenceEqual (fullSpan.ToArray()) expected $"{name}: SpanSlice(0, {len}) should match the full input."

    if len > 0 then
        let mid = len / 2
        let clampedSpan = readable.SpanSlice(mid, len * 2) // Requesting way past the end
        let expectedClamped = expected.[mid..]

        Expect.sequenceEqual
            (clampedSpan.ToArray())
            expectedClamped
            $"{name}: SpanSlice length should clamp to the end of the readable."

    Expect.throws
        (fun () -> let _ = readable.SpanSlice(-1, 1) in ())
        $"{name}: SpanSlice with negative index must throw."

    Expect.throws
        (fun () -> let _ = readable.SpanSlice(0, -1) in ())
        $"{name}: SpanSlice with negative count must throw."

    // 6. Slice behavior (Returns 'Slice which is also IReadable)
    if len > 1 then
        // Valid Slice
        let subStart = 1
        let subLen = len - 1
        let validSlice = readable.Slice(subStart, subLen)

        Expect.equal
            validSlice.Length
            subLen
            $"{name}: Slice({subStart}, {subLen}) should have exactly Length {subLen}."

        Expect.equal
            (validSlice.Item 0)
            expected.[subStart]
            $"{name}: validSlice.Item(0) should map to parent's index {subStart}."

        // Clamped Slice
        let clampedSlice = readable.Slice(1, len + 10)
        Expect.equal clampedSlice.Length (len - 1) $"{name}: Slice length must clamp to parent bounds."

        // Past-end Slice
        let pastEndSlice = readable.Slice(len + 5, 10)
        Expect.equal pastEndSlice.Length 0 $"{name}: Slicing completely past the end should result in an empty slice."

        // Parent Escape Check
        let half = len / 2
        let parentSlice = readable.Slice(0, half)
        let childSlice = parentSlice.Slice(0, len) // The child asks for the full original length

        Expect.equal
            childSlice.Length
            half
            $"{name}: SECURITY - Nested slice MUST NOT escape its parent's sliced bounds."

    Expect.throws (fun () -> readable.Slice(-1, 1) |> ignore) $"{name}: Slice with negative start must throw."
    Expect.throws (fun () -> readable.Slice(0, -1) |> ignore) $"{name}: Slice with negative length must throw."

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "IReadable Semantics"
        [

            test "ReadableString" {
                let str = "Hello World"
                let readable = ReadableString(str)
                assertReadableSemantics "ReadableString" readable (str.ToCharArray())
            }

            test "ReadableStringSlice" {
                let str = "Pad_Hello World_Pad"
                // Slice out "Hello World"
                let slice = ReadableStringSlice(str, 4, 11)
                assertReadableSemantics "ReadableStringSlice" slice ("Hello World".ToCharArray())
            }

            test "Empty ReadableString" {
                let readable = ReadableString("")
                assertReadableSemantics "Empty ReadableString" readable [||]
            }

            test "Empty ReadableStringSlice" {
                let str = "Pad_Pad"
                let slice = ReadableStringSlice(str, 3, 0)
                assertReadableSemantics "Empty ReadableStringSlice" slice [||]
            }

            test "ReadableArray" {
                let arr = [| 10; 20; 30; 40; 50 |]
                let readable = ReadableArray(arr)
                assertReadableSemantics "ReadableArray" readable arr
            }

            test "ReadableArraySlice" {
                let arr = [| 0; 0; 10; 20; 30; 40; 50; 0; 0 |]
                let slice = ReadableArraySlice(arr, 2, 5)
                assertReadableSemantics "ReadableArraySlice" slice [| 10; 20; 30; 40; 50 |]
            }

            test "Empty ReadableArray" {
                let readable = ReadableArray<int>([||])
                assertReadableSemantics "Empty ReadableArray" readable [||]
            }

            test "Empty ReadableArraySlice" {
                let arr = [| 1; 2; 3 |]
                let slice = ReadableArraySlice(arr, 1, 0)
                assertReadableSemantics "Empty ReadableArraySlice" slice [||]
            }

            test "ReadableImmutableArray" {
                let arr = ImmutableArray.CreateRange([| 10; 20; 30; 40; 50 |])
                let readable = ReadableImmutableArray(arr)
                assertReadableSemantics "ReadableImmutableArray" readable [| 10; 20; 30; 40; 50 |]
            }

            test "ReadableImmutableArraySlice" {
                let arr = ImmutableArray.CreateRange([| 0; 0; 10; 20; 30; 40; 50; 0; 0 |])
                let slice = ReadableImmutableArraySlice(arr, 2, 5)
                assertReadableSemantics "ReadableImmutableArraySlice" slice [| 10; 20; 30; 40; 50 |]
            }

            test "Empty ReadableImmutableArray" {
                let arr = ImmutableArray.CreateRange<int>([||])
                let readable = ReadableImmutableArray(arr)
                assertReadableSemantics "Empty ReadableImmutableArray" readable [||]
            }

#if NET5_0_OR_GREATER
            test "ReadableResizeArray" {
                let arr = ResizeArray<int>([| 10; 20; 30; 40; 50 |])
                let readable = ReadableResizeArray(arr)
                assertReadableSemantics "ReadableResizeArray" readable [| 10; 20; 30; 40; 50 |]
            }

            test "ReadableResizeArraySlice" {
                let arr = ResizeArray<int>([| 0; 0; 10; 20; 30; 40; 50; 0; 0 |])
                let slice = ReadableResizeArraySlice(arr, 2, 5)
                assertReadableSemantics "ReadableResizeArraySlice" slice [| 10; 20; 30; 40; 50 |]
            }

            test "Empty ReadableResizeArray" {
                let arr = ResizeArray<int>()
                let readable = ReadableResizeArray(arr)
                assertReadableSemantics "Empty ReadableResizeArray" readable [||]
            }
#endif

#if !FABLE_COMPILER
            test "ReadableMemory" {
                let arr = [| 10; 20; 30; 40; 50 |]
                let readable = ReadableMemory(ReadOnlyMemory(arr))
                assertReadableSemantics "ReadableMemory" readable arr
            }

            test "ReadableMemory from slice" {
                let arr = [| 0; 0; 10; 20; 30; 40; 50; 0; 0 |]
                let readable = ReadableMemory(ReadOnlyMemory(arr).Slice(2, 5))
                assertReadableSemantics "ReadableMemory from slice" readable [| 10; 20; 30; 40; 50 |]
            }

            test "Empty ReadableMemory" {
                let readable = ReadableMemory(ReadOnlyMemory<int>([||]))
                assertReadableSemantics "Empty ReadableMemory" readable [||]
            }
#endif
        ]
