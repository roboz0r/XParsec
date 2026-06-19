module XParsec.FSharp.Codegen.Clr.Tests.StructSeqTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Vertical slice toward the zero-allocation struct `Seq` module
// (`brainstorm-seq-module.md`). These fixtures stand in for the eventual
// `src/Vesper.Seq` struct types; they stay inline F# until the codegen shape is
// proven, then graduate to real library source.
//
// RUNG 2 — landed. The struct pipeline relies on chained `this.field.Method(args)`
// calls (e.g. `this.Source.GetEnumerator()`, `this.Source.MoveNext()`). Two fixes
// made this work, both with isolation tests below:
//   1. Front-end: a method call through a 3+-segment folded LongIdent chain is now
//      recognised (`Resolve.(|ClassChainMethod|_|)` + the FreezeExpr `App` arms);
//      previously it mis-typed the trailing method as a property and lowered the
//      call's `()` to a spurious `Vesper.Fun::Invoke`.
//   2. Codegen: a struct-typed *field* receiver is addressed in place via `ldflda`
//      (`EmitMember.loadStructReceiverAddr`), so a mutating member call persists
//      rather than mutating a spilled copy.

[<Tests>]
let structSeqTests =
    testList
        "StructSeq"
        [
            // Wall B+C (rung 3): a member call on a value whose type is a generic
            // typar constrained to a project-local interface (`'T :> IGetVal`).
            // The receiver is a bare TyVar carrying a `Coercion` constraint; Wall B
            // resolves the member through the interface's members and mints a
            // `CallVia.Interface` node, Wall C emits `constrained. <typar> callvirt`
            // so it RUNS end-to-end.
            test "typar receiver constrained to a local interface dispatches via constrained callvirt (Wall C)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "type Holder(n: int) ="
                            "    interface IGetVal with"
                            "        member _.GetVal() = n"
                            "let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()"
                            "printfn \"%d\" (callIt (Holder 7))"
                        ]

                let _, artifact = compileSource "TyparInterfaceDispatch" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "constrained typar interface dispatch returns the impl value"
            }

            // Wall C, the non-allocating payoff: the SAME generic `callIt` applied to a
            // `[<Struct>]` argument. The constrained-typar dispatch addresses the struct
            // (`ldloca`) and `constrained. !!T callvirt`s the interface slot, so the JIT
            // resolves the struct's impl directly — no boxing. Asserted on `callIt`'s IL:
            // a `constrained.` prefix (0xFE 0x16) is present and there is NO `box` (0x8C).
            test "constrained typar dispatch on a struct arg does not box (Wall C)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "[<Struct>]"
                            "type SBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IGetVal with"
                            "        member this.GetVal() = this.N"
                            "let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()"
                            "printfn \"%d\" (callIt (SBox 9))"
                        ]

                let _, artifact = compileSource "TyparInterfaceStructDispatch" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "9" "constrained struct dispatch returns the impl value"

                // The holder-less top-level `callIt` is emitted on the "Program" type
                // under a synthetic `fn$<n>` name, so target it structurally.
                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix (typar interface dispatch)"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box` (non-allocating struct dispatch)"
            }

            // Rung-3 generic payoff, step 0: a typar constrained to a *generic*
            // interface instantiated at a CONCRETE arg (`'T :> IBox<int>`). Forces
            // the `CallVia.Interface` slot to be minted on the instantiated interface
            // `TypeSpec` (`IBox`1<int>`) rather than the bare definition — the case
            // Wall C deferred (`iface.Typars` non-empty).
            test "constrained typar dispatch on a generic interface (concrete arg) does not box" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'T> ="
                            "    abstract member Get : unit -> 'T"
                            "[<Struct>]"
                            "type IntBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IBox<int> with"
                            "        member this.Get() = this.N"
                            "let callIt (x: 'T when 'T :> IBox<int>) : int = x.Get()"
                            "printfn \"%d\" (callIt (IntBox 5))"
                        ]

                let _, artifact = compileSource "GenericIfaceConcreteDispatch" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "5"
                    "generic-interface constrained dispatch returns the impl value"

                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box`"
            }

            // Rung-3 generic payoff, step 1: a generic struct whose field is a typar
            // (`'S`) constrained to a generic interface whose arg is ANOTHER typar of
            // the enclosing struct (`'S :> IBox<'T>`). The constrained dispatch must
            // mint the slot on `IBox\`1<!T>` where `!T` is the struct's own typar —
            // the interface instantiation is itself a generic parameter, not concrete.
            test "generic struct dispatches through a typar field constrained to a generic interface (typar arg)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'T> ="
                            "    abstract member Get : unit -> 'T"
                            "[<Struct>]"
                            "type IntBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IBox<int> with"
                            "        member this.Get() = this.N"
                            "[<Struct>]"
                            "type Wrap<'S, 'T when 'S :> IBox<'T>> ="
                            "    val Inner : 'S"
                            "    new(inner: 'S) = { Inner = inner }"
                            "    member this.Fetch() : 'T = this.Inner.Get()"
                            "let w = Wrap<IntBox, int>(IntBox 7)"
                            "printfn \"%d\" (w.Fetch())"
                        ]

                let _, artifact = compileSource "GenericStructTyparIface" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "generic-struct typar-arg interface dispatch returns the impl value"
            }

            // Rung-3 sub-task 1 (north-star probe gap): a [<Struct>] implementing an
            // interface that declares an abstract *property* (`Current`). The impl
            // property getter must be wired (MethodImpl / get_-getter) to the
            // interface's getter slot, else TypeLoadException "Method 'Current' ...
            // does not have an implementation".
            test "struct implements an interface with an abstract property and dispatches" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "let e = ArrayEnumerator([| 10; 20 |])"
                            "let i = (e :> IStructEnumerator)"
                            "i.MoveNext() |> ignore"
                            "printfn \"%d\" i.Current"
                        ]

                let _, artifact = compileSource "StructIfaceProperty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "struct interface property dispatch returns the impl value"
            }

            // rung-3 §2.1 sub-gap 1: a *generic* struct implementing a *generic*
            // local interface AT ITS OWN TYPAR (`Box<'T> : IBox<'T>`). Every other
            // interface-impl fixture instantiates the interface at a CONCRETE arg
            // (`IBox<int>`, `IStructSeq<ArrayEnumerator>`); here the impl member's
            // return type `'T` is the enclosing struct's own type parameter, which
            // must be threaded into the impl member's scope (not diagnosed free) AND
            // emitted as a generic MethodImpl so the dispatch round-trips at any
            // instantiation. Boxing the struct to the interface and calling `Unwrap`
            // is the producer-side proof.
            test
                "a generic struct implements a generic local interface at its own typar and dispatches (rung 3 §2.1 sub-gap 1)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'E> ="
                            "    abstract member Unwrap : unit -> 'E"
                            "[<Struct>]"
                            "type Box<'T> ="
                            "    val Value : 'T"
                            "    new(value: 'T) = { Value = value }"
                            "    interface IBox<'T> with"
                            "        member this.Unwrap() : 'T = this.Value"
                            "let b = Box<int>(42)"
                            "let i = (b :> IBox<int>)"
                            "printfn \"%d\" (i.Unwrap())"
                        ]

                let _, artifact = compileSource "GenericStructGenericIface" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42"
                    "generic struct implementing a generic interface at its own typar dispatches the impl value"
            }

            // Wall A (rung 3): a project-local class implementing a project-local
            // interface, dispatched through the interface. Existing interface-impl
            // tests all use BCL interfaces; `resolveInterfaceImpls` only recognises an
            // interface via the external provider, so a local interface errors with
            // "Type 'IGetVal' is not an interface".
            test "project-local class implements a project-local interface and dispatches" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "type Holder(n: int) ="
                            "    interface IGetVal with"
                            "        member _.GetVal() = n"
                            "let h = Holder(42)"
                            "let v = (h :> IGetVal).GetVal()"
                            "printfn \"%d\" v"
                        ]

                let _, artifact = compileSource "LocalInterfaceImpl" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local interface dispatch returns the impl value"
            }

            // Minimal repro of the chained-method-call gap: `this.I.Get()` parses as
            // `App(LongIdent[this; I; Get], ())`. The 3-segment chain isn't recognised
            // as a method call, so `Get` is mis-typed as a property and `()` becomes a
            // `Fun::Invoke` over-application → ExecutionEngine at JIT.
            test "chained this.field.Method() call (3-segment) resolves and runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Inner ="
                            "    val Cur : int"
                            "    new(c: int) = { Cur = c }"
                            "    member this.Get() : int = this.Cur"
                            "[<Struct>]"
                            "type Outer ="
                            "    val I : Inner"
                            "    new(i: Inner) = { I = i }"
                            "    member this.StepGet() : int = this.I.Get()"
                            "let run () ="
                            "    let o = Outer(Inner(7))"
                            "    printfn \"%d\" (o.StepGet())"
                            "run ()"
                        ]

                let _, artifact = compileSource "StructFieldGet" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "7" "chained method call on a struct field works"
            }

            // A 4-segment chain `this.A.B.Bump()` through two intermediate struct
            // fields, mutating the innermost — exercises the recursive `ldflda`
            // addressing (`this` → `ldflda A` → `ldflda B` → call by address).
            test "4-segment chained method call through nested struct fields mutates in place" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Leaf ="
                            "    val mutable N : int"
                            "    new(n: int) = { N = n }"
                            "    member this.Bump() : int ="
                            "        this.N <- this.N + 1"
                            "        this.N"
                            "[<Struct>]"
                            "type Mid ="
                            "    val mutable L : Leaf"
                            "    new(l: Leaf) = { L = l }"
                            "[<Struct>]"
                            "type Top ="
                            "    val mutable M : Mid"
                            "    new(m: Mid) = { M = m }"
                            "    member this.Step() : int = this.M.L.Bump()"
                            "let run () ="
                            "    let mutable t = Top(Mid(Leaf(10)))"
                            "    let a = t.Step()"
                            "    let b = t.Step()"
                            "    printfn \"%d %d\" a b"
                            "run ()"
                        ]

                let _, artifact = compileSource "NestedStructChain" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "11 12"
                    "nested struct-field mutation persists across calls"
            }

            // A chain headed by an ordinary local (not `this`): `o.I.Get()`.
            test "chained method call headed by a local variable resolves" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Inner ="
                            "    val Cur : int"
                            "    new(c: int) = { Cur = c }"
                            "    member this.Get() : int = this.Cur"
                            "[<Struct>]"
                            "type Outer ="
                            "    val I : Inner"
                            "    new(i: Inner) = { I = i }"
                            "let run () ="
                            "    let o = Outer(Inner(42))"
                            "    printfn \"%d\" (o.I.Get())"
                            "run ()"
                        ]

                let _, artifact = compileSource "LocalHeadedChain" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local-headed chained method call works"
            }

            // Rung-3 payoff, for-in step 1: `for y in s` over a GENERIC typar source
            // (`'S :> ISeq`) whose `GetEnumerator` is reached through a project-local
            // *custom* (non-`IEnumerable`) interface. The source receiver is a typar, so
            // `GetEnumerator` must dispatch via `constrained. !S callvirt ISeq::GetEnumerator`.
            // The enumerator `E` is here a CONCRETE struct exposing public pattern
            // `MoveNext`/`Current`, so the loop body stays the existing by-address struct
            // walk — isolating the new for-in-over-typar-source dispatch.
            test "for-in over a generic typar source via a custom interface (concrete enumerator)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type ISeq ="
                            "    abstract member GetEnumerator : unit -> ArrayEnumerator"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface ISeq with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> ISeq) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "TyparSeqSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar seq source sums via constrained dispatch"
            }

            // Rung-3 payoff, for-in step 2a: `for y in s` over a generic typar source
            // whose seq interface is GENERIC instantiated at a CONCRETE enumerator
            // (`'S :> IStructSeq<ArrayEnumerator>`). `GetEnumerator` dispatches via
            // `constrained. !S callvirt IStructSeq`1<ArrayEnumerator>::GetEnumerator` —
            // the slot minted on the instantiated interface `TypeSpec`. The enumerator
            // is concrete so its members stay the by-address struct walk.
            test "for-in over a generic typar source via a generic interface (concrete enumerator)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type IStructSeq<'E> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<ArrayEnumerator>) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "GenericIfaceTyparSeqSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar source via a generic seq interface sums"
            }

            // Rung-3 payoff, for-in step 2b (the §2 north-star): a fully generic
            // `sumSeq` over ANY struct sequence — both the seq interface arg `'E` and
            // the enumerator are typars (`'S :> IStructSeq<'E> and 'E :> IStructEnumerator`).
            // `GetEnumerator` and the enumerator's `MoveNext`/`Current` ALL dispatch via
            // `constrained. callvirt`, with `'E` inferred from `ArraySeq`'s interface impl.
            test "for-in over a fully generic struct seq source (north-star)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "FullyGenericStructSeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "fully generic struct seq sums via constrained dispatch"
            }

            // Rung 2 target: a concrete `[<Struct>] MapSeq` holding a concrete
            // `[<Struct>] ArraySeq` field + a reference-type closure, walked by
            // `for y in s` — the value-type-source (rung 1) + chained struct-field
            // dispatch (`this.Source.GetEnumerator()` / `this.Source.MoveNext()`).
            test "concrete struct MapSeq pipeline maps and folds (rung 2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator ="
                            "    val mutable Source : ArrayEnumerator"
                            "    val F : int -> int"
                            "    new(source: ArrayEnumerator, f: int -> int) = { Source = source; F = f }"
                            "    member this.MoveNext() : bool = this.Source.MoveNext()"
                            "    member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq ="
                            "    val Source : ArraySeq"
                            "    val F : int -> int"
                            "    new(source: ArraySeq, f: int -> int) = { Source = source; F = f }"
                            "    member this.GetEnumerator() : MapEnumerator = MapEnumerator(this.Source.GetEnumerator(), this.F)"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq(ArraySeq(xs), fun x -> x * 2)"
                            "for y in s do"
                            "    printfn \"%d\" y"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "StructMapSeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "2\n4\n6\ndone" "maps the struct pipeline in order"
            }

            // Rung-3 payoff, step 2a: a GENERIC `MapSeq<'S>` whose source field is a
            // typar (`'S :> IStructSeq<ArrayEnumerator>`), instantiated at a CONCRETE
            // `ArraySeq` at the use site, walked by `for y in s`. Two capabilities meet:
            //   - inside `MapSeq.GetEnumerator`, `this.Source.GetEnumerator()` is a
            //     constrained-typar dispatch (the Wrap.Fetch capability), and
            //   - `for y in s` sources a CONCRETE instantiation of a generic struct
            //     (`MapSeq`1<ArraySeq>`), so the for-in pattern walk addresses a
            //     generic-struct value by address and calls its pattern `GetEnumerator`.
            // The enumerator (`MapEnumerator`) stays concrete to isolate the generic
            // *source* from a generic *enumerator* (step 2b).
            test "for-in over a generic MapSeq wrapping a concrete ArraySeq (rung 3 step 2a)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type IStructSeq<'E> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator ="
                            "    val mutable Source : ArrayEnumerator"
                            "    val F : int -> int"
                            "    new(source: ArrayEnumerator, f: int -> int) = { Source = source; F = f }"
                            "    member this.MoveNext() : bool = this.Source.MoveNext()"
                            "    member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S when 'S :> IStructSeq<ArrayEnumerator>> ="
                            "    val Source : 'S"
                            "    val F : int -> int"
                            "    new(source: 'S, f: int -> int) = { Source = source; F = f }"
                            "    member this.GetEnumerator() : MapEnumerator = MapEnumerator(this.Source.GetEnumerator(), this.F)"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq<ArraySeq>(ArraySeq(xs), fun x -> x * 2)"
                            "for y in s do"
                            "    printfn \"%d\" y"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "GenericMapSeqConcreteSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "2\n4\n6\ndone"
                    "generic MapSeq over concrete ArraySeq maps in order"
            }

            // Rung-3 payoff, step 2b: the FULLY GENERIC map pipeline. Both `MapSeq` and
            // `MapEnumerator` are generic; the enumerator chains a generic inner
            // enumerator `'E :> IStructEnumerator` — so `MapEnumerator<'E>.MoveNext` /
            // `.Current` dispatch on a typar field via `constrained. !E callvirt`. The
            // for-in source `s` is a concrete instantiation `MapSeq`2<ArraySeq,
            // ArrayEnumerator>`; its `GetEnumerator` yields a concrete-but-generic
            // `MapEnumerator`1<ArrayEnumerator>`. This is the `ArraySeq → map` tree, all
            // generic, the keystone of the §2 north-star.
            test "fully generic struct map pipeline chains a generic enumerator (rung 3 step 2b)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator<'E when 'E :> IStructEnumerator> ="
                            "    val mutable Source : 'E"
                            "    val F : int -> int"
                            "    new(source: 'E, f: int -> int) = { Source = source; F = f }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool = this.Source.MoveNext()"
                            "        member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S, 'E when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator> ="
                            "    val Source : 'S"
                            "    val F : int -> int"
                            "    new(source: 'S, f: int -> int) = { Source = source; F = f }"
                            "    interface IStructSeq<MapEnumerator<'E>> with"
                            "        member this.GetEnumerator() : MapEnumerator<'E> = MapEnumerator<'E>(this.Source.GetEnumerator(), this.F)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq<ArraySeq, ArrayEnumerator>(ArraySeq(xs), fun x -> x * 2)"
                            "printfn \"%d\" (sumSeq s)"
                        ]

                let _, artifact = compileSource "FullyGenericMapPipeline" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "12" "fully generic map pipeline sums the mapped values"
            }

            // §2.2 graduation prerequisite: the consuming TERMINAL `fold`. Every
            // prior fixture sums inline with `total <- total + y`; none drives a
            // generic struct seq through a `fold` that threads a STATE accumulator
            // and applies a passed reference-type closure `(fun acc x -> acc + x)`
            // per element. This is the shape `src/Vesper.Seq` `Seq.fold` will have:
            // a free generic function `fold f seed s`, generic over the struct seq
            // `'S`/enumerator `'E`, walking `for y in s` and folding. Proves the
            // terminal codegens + runs before the library graduation.
            // §7.2 escape hatch: a generic struct sequence/enumerator (generic over
            // `'T`) ALSO implements the BCL `IEnumerable<'T>` / `IEnumerator<'T>` /
            // `IEnumerator` / `IDisposable` so it boxes transparently when handed to a
            // standard .NET API. Proves the generic-struct-implements-generic-BCL-interface
            // declaration + the `IEnumerator<'T> :> IEnumerator` upcast (§2.1 sub-gap 2)
            // round-trip: the struct upcast to `IEnumerable<int>` enumerates 1,2,3.
            test "generic struct seq implements IEnumerable<'T> escape hatch and enumerates (§7.2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections.Generic"
                            "open System.Collections"
                            "[<Struct>]"
                            "type ArrayEnumerator<'T> ="
                            "    val Arr : 'T[]"
                            "    val mutable Idx : int"
                            "    new(arr: 'T[]) = { Arr = arr; Idx = -1 }"
                            "    interface IEnumerator<'T> with"
                            "        member this.Current : 'T = this.Arr.[this.Idx]"
                            "    interface IEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : obj = box (this.Arr.[this.Idx])"
                            "        member this.Reset() : unit = ()"
                            "    interface System.IDisposable with"
                            "        member this.Dispose() : unit = ()"
                            "[<Struct>]"
                            "type ArraySeq<'T> ="
                            "    val Arr : 'T[]"
                            "    new(arr: 'T[]) = { Arr = arr }"
                            "    interface IEnumerable<'T> with"
                            "        member this.GetEnumerator() : IEnumerator<'T> = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator<'T>)"
                            "    interface IEnumerable with"
                            "        member this.GetEnumerator() : IEnumerator = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator)"
                            "let sum3 (xs: IEnumerable<int>) : int ="
                            "    let e = xs.GetEnumerator()"
                            "    let mutable total = 0"
                            "    while e.MoveNext() do"
                            "        total <- total + e.Current"
                            "    total"
                            "let s = ArraySeq<int>([| 1; 2; 3 |])"
                            "printfn \"%d\" (sum3 (s :> IEnumerable<int>))"
                        ]

                let _, artifact = compileSource "StructSeqEscapeHatch" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "6" "escape-hatch enumerates via IEnumerable<'T>"
            }

            test "fold over a fully generic struct seq threads state through a closure (rung 3 §2.2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let fold (f: 'State -> int -> 'State) (seed: 'State) (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : 'State ="
                            "    let mutable state = seed"
                            "    for y in s do"
                            "        state <- f state y"
                            "    state"
                            "let xs = [| 1; 2; 3; 4 |]"
                            "printfn \"%d\" (fold (fun acc x -> acc + x) 0 (ArraySeq(xs)))"
                        ]

                let _, artifact = compileSource "StructSeqFold" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "fold threads state through the closure over a generic struct seq"
            }
        ]
