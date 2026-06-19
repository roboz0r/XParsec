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
        ]
