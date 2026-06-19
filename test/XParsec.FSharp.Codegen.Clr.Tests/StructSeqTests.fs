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
