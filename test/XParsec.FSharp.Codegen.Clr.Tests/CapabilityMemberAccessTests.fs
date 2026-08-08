module XParsec.FSharp.Codegen.Clr.Tests.CapabilityMemberAccessTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Member access on a value typed as a CAPABILITY rather than as a concrete implementer.
//
// A capability's canonical shape (`Vesper.Collections.enumerator`1`) is an
// `IntrinsicInterface`: it NAMES its platform type but carries no member table. So a
// value typed that way used to resolve its shape, find no members on it, and fail with
// "Unknown class type" — even though `capabilities.fsi` declares `MoveNext` / `Current`.
// Member lookup now retries under the capability's platform key
// (`EngineCore.capabilityPlatformKey`), where the members live.
//
// Systematically: every capability that HAS members, every member kind (method, property,
// inherited), and both the direct and deferred resolution paths — one test each, so a
// regression names which one broke rather than "Vesper.Seq stopped building".
//
// The JS side needs none of this: there a capability is a plain `Class` carrying its own
// members, which is why `seq.fs` compiled there before this fix existed.

/// A cursor over `1 .. n` used as the inner enumerator to drive by hand.
let private counterSrc =
    String.concat
        "\n"
        [
            "type Counter ="
            "    val mutable cur: int"
            "    val last: int"
            "    new(last: int) = { cur = 0; last = last }"
            "    interface Vesper.Collections.enumerator<int> with"
            "        member this.Current = this.cur"
            "        member this.MoveNext() ="
            "            if this.cur >= this.last then false"
            "            else"
            "                this.cur <- this.cur + 1"
            "                true"
            "    interface Vesper.disposable with"
            "        member this.Dispose() = ()"
            ""
        ]

[<Tests>]
let tests =
    testList
        "Clr CapabilityMemberAccess"
        [
            test "`MoveNext` resolves on an enumerator-typed value (a METHOD)" {
                // The plainest case: a method declared on the capability, called through it.
                runs
                    "3"
                    (counterSrc
                     + String.concat
                         "\n"
                         [
                             "open Vesper.IntComparison"
                             "let count (e: Vesper.Collections.enumerator<int>) ="
                             "    let mutable n = 0"
                             "    while e.MoveNext() do"
                             "        n <- n + 1"
                             "    n"
                             "printfn \"%d\" (count (new Counter(3) :> Vesper.Collections.enumerator<int>))"
                         ])
            }

            test "`Current` resolves on an enumerator-typed value (a PROPERTY)" {
                // The case that could have made this fix expensive: the contract declares
                // `abstract member Current: 'T`, the BCL member is `get_Current`. The repr
                // hop has to bridge that naming, and does.
                runs
                    "1"
                    (counterSrc
                     + String.concat
                         "\n"
                         [
                             "let firstOf (e: Vesper.Collections.enumerator<int>) ="
                             "    if e.MoveNext() then e.Current else -1"
                             "printfn \"%d\" (firstOf (new Counter(3) :> Vesper.Collections.enumerator<int>))"
                         ])
            }

            test "`Dispose` resolves through the INHERITED disposable capability" {
                // `enumerator` inherits `disposable`, so this one is not on the platform type
                // itself — it is reached by walking `IEnumerator\`1`'s interface chain. That
                // walk only works because the retry rewrites PLATFORM-ward: canon-ward would
                // have erased the BCL type's own bases.
                runs
                    "disposed"
                    (counterSrc
                     + String.concat
                         "\n"
                         [
                             "let closeIt (e: Vesper.Collections.enumerator<int>) ="
                             "    e.Dispose()"
                             "    printfn \"disposed\""
                             "closeIt (new Counter(1) :> Vesper.Collections.enumerator<int>)"
                         ])
            }

            test "`GetEnumerator` resolves on a seq-typed value, and its result drives" {
                // The `seq` capability, and the composition that matters: the member's
                // RESULT is itself capability-typed, so the returned cursor must resolve its
                // own members too. A local implementer rather than a `[1;2;3]` literal, to
                // keep the test on capability resolution and off cons-list support.
                runs
                    "6"
                    (counterSrc
                     + String.concat
                         "\n"
                         [
                             "type Upto ="
                             "    val last: int"
                             "    new(last: int) = { last = last }"
                             "    interface Vesper.Collections.seq<int> with"
                             "        member this.GetEnumerator() ="
                             "            (new Counter(this.last) :> Vesper.Collections.enumerator<int>)"
                             "let sumOf (s: Vesper.Collections.seq<int>) ="
                             "    let e = s.GetEnumerator()"
                             "    let mutable total = 0"
                             "    while e.MoveNext() do"
                             "        total <- total + e.Current"
                             "    total"
                             "printfn \"%d\" (sumOf (new Upto(3) :> Vesper.Collections.seq<int>))"
                         ])
            }

            test "a capability-typed FIELD resolves its members (the deferred path)" {
                // A `val` of capability type, read through `this` — the shape `Seq.truncate`'s
                // cursor is built from, and the one that reaches the deferred
                // `DotSource.ExternalClass` arm rather than the direct one.
                runs
                    "1,2"
                    (counterSrc
                     + String.concat
                         "\n"
                         [
                             "type Wrapper ="
                             "    val inner: Vesper.Collections.enumerator<int>"
                             "    new(inner: Vesper.Collections.enumerator<int>) = { inner = inner }"
                             "    member this.Next() = if this.inner.MoveNext() then this.inner.Current else -1"
                             "let w = new Wrapper(new Counter(5) :> Vesper.Collections.enumerator<int>)"
                             "printfn \"%d,%d\" (w.Next()) (w.Next())"
                         ])
            }

            test "a NON-capability external interface is unaffected by the retry" {
                // The fold returns a non-capability key unchanged, so ordinary external
                // member resolution is untouched — the negative control for the rewrite.
                runs
                    "2"
                    (String.concat "\n" [ "let lengthOf (s: string) = s.Length"; "printfn \"%d\" (lengthOf \"ab\")" ])
            }
        ]
