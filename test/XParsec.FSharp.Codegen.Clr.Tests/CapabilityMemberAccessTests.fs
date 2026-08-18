module XParsec.FSharp.Codegen.Clr.Tests.CapabilityMemberAccessTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Member access on a value typed as a CAPABILITY rather than as a concrete implementer.
// A capability's canonical shape (`Vesper.Collections.enumerator`1`) identifies its platform
// type but carries no member table, so lookup retries under the platform key.

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
                // The contract declares `abstract member Current: 'T`; the BCL member is
                // `get_Current`, so the platform hop has to bridge that naming.
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
                // `Dispose` is not on the platform type itself: it is reached by walking
                // `IEnumerator\`1`'s interface chain, which works only because the retry
                // rewrites PLATFORM-ward. Canon-ward would erase the BCL type's own bases.
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
                // The member's RESULT is itself capability-typed, so the returned cursor
                // has to resolve its own members too. Driven by a local implementer rather
                // than a `[1;2;3]` literal, to keep cons-list support out of the test.
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
                // A `val` of capability type read through `this`, which reaches the
                // DEFERRED external-class resolution arm rather than the direct one.
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
                // A non-capability key comes back unchanged, so ordinary external member
                // resolution is untouched.
                runs
                    "2"
                    (String.concat "\n" [ "let lengthOf (s: string) = s.Length"; "printfn \"%d\" (lengthOf \"ab\")" ])
            }
        ]
