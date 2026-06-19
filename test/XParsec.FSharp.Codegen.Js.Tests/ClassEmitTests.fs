module XParsec.FSharp.Codegen.Js.Tests.ClassEmitTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

// Step B + C — general JS class emission and the custom-equality dispatch slot.
//
// Before this work the JS backend dropped `TTypeKindG.Class` entirely (`collectTypes`
// matched only Record/Union). These tests prove a plain class emits + RUNS on JS
// (field, ctor, a free-function member, instantiation, field access) and that a
// `[<CustomEquality>]` class emits an ATTACHED `Equals(` instance method (the runtime
// dispatch slot — `Vesper.Core.eq` calls `a.Equals(b)`).

[<Tests>]
let tests =
    testList
        "Codegen.Js ClassEmit"
        [
            // ---- general class emission: shape ----

            test "a plain class emits a JS class with a positional ctor and a free member function" {
                let src =
                    emitJs (
                        lines
                            [
                                "type Box(value: int) ="
                                "    member _.Value = value"
                                "    member _.Plus n = value + n"
                                "let b = Box(7)"
                                "printfn \"%d\" b.Value"
                            ]
                    )

                Expect.stringContains src "class Box {" "the class emits"
                Expect.stringContains src "constructor(value)" "positional ctor over the ctor param"
                Expect.stringContains src "this.value = value;" "ctor stores the field under its source name"
                // Regular members are FREE receiver-first functions (tree-shaking).
                Expect.stringContains src "const Box__get_Value = (" "instance property → free function"
                Expect.stringContains src "const Box__Plus = (" "instance method → free function"
                // Instantiation is `new Box(...)`.
                Expect.stringContains src "new Box(7)" "instantiation lowers to `new`"
            }

            // ---- general class emission: execution under Node ----

            test "a plain class runs on JS — ctor, field access, and a free-function member call" {
                match
                    runJs
                        "class-plain"
                        (lines
                            [
                                "type Box(value: int) ="
                                "    member _.Value = value"
                                "    member _.Plus n = value + n"
                                "let b = Box(40)"
                                "printfn \"%d\" b.Value"
                                "printfn \"%d\" (b.Plus 2)"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "40\n42" "field access reads the ctor-stored field; the free member adds"
            }

            // ---- custom-equality dispatch slot: ATTACHED Equals ----

            test "a [<CustomEquality>] class emits an attached `Equals(` instance method (the dispatch slot)" {
                let src =
                    emitJs (
                        lines
                            [
                                "[<CustomEquality; NoComparison>]"
                                "type Tagged(id: int, payload: int) ="
                                "    member _.Id = id"
                                "    member _.Payload = payload"
                                "    override this.Equals(o: obj) = false"
                                "    override this.GetHashCode() = id"
                                "    interface System.IEquatable<Tagged> with"
                                "        member this.Equals(other: Tagged) = (id = other.Id)"
                                "let a = Tagged(1, 10)"
                                "printfn \"%d\" a.Id"
                            ]
                    )

                Expect.stringContains src "class Tagged {" "the custom-eq class emits"
                // The IEquatable<Self>.Equals impl attaches as an instance method named
                // `Equals` (the runtime hook `a.Equals(b)` finds it). The redundant
                // obj-typed `Object.Equals` override is dropped (interface wins the slot).
                Expect.stringContains src "Equals(other)" "typed IEquatable.Equals attaches as `Equals`"
                // The `override GetHashCode` attaches so `hashOf` can find it.
                Expect.stringContains src "GetHashCode()" "override GetHashCode attaches"
            }

            // ---- custom-equality dispatch slot: execution proves the slot is live ----
            //
            // The fuller custom-eq round-trip oracle is a later step; this is a single
            // execution check that the ATTACHED `Equals` slot is genuinely dispatched by
            // `Vesper.Core.eq` (`a.Equals(b)`). `Tagged.Equals` compares ONLY `id`, so
            // `a = b` (same id, different payload) is `true` ONLY via the custom member
            // — structural would be `false` (payload differs), reference `false`.
            test "a [<CustomEquality>] class dispatches `=` to the attached IEquatable.Equals on JS" {
                match
                    runJs
                        "class-customeq"
                        (lines
                            [
                                "[<CustomEquality; NoComparison>]"
                                "type Tagged(id: int, payload: int) ="
                                "    member _.Id = id"
                                "    member _.Payload = payload"
                                "    override this.Equals(o: obj) = false"
                                "    override this.GetHashCode() = id"
                                "    interface System.IEquatable<Tagged> with"
                                "        member this.Equals(other: Tagged) = (id = other.Id)"
                                "let a = Tagged(1, 10)"
                                "let b = Tagged(1, 20)"
                                "let c = Tagged(2, 10)"
                                "printfn \"%b\" (a = b)"
                                "printfn \"%b\" (a = c)"
                                "printfn \"%b\" (a = a)"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue" "`=` dispatches to IEquatable<Tagged>.Equals (id-only)"
            }

            // ---- custom-comparison dispatch slot: execution proves `<`/`>`/`<=`/`>=` route through CompareTo ----
            //
            // Mirrors the CLR oracle `CustomEqualityComparisonDispatchTests.fs`. `Ranked.CompareTo`
            // orders by id DESCENDING (inverted), so `a(1) < b(2)` is FALSE under the custom member
            // (a natural ordering would give true). Passing proves `<`/`>`/`<=`/`>=` dispatch through
            // `Vesper.Comparison.structuralCompare` → `cmp(a,b)` → `a.CompareTo(b)`.
            test
                "a [<CustomComparison>] class dispatches `<`/`>`/`<=`/`>=` to the attached IComparable.CompareTo on JS (inverted order)" {
                match
                    runJs
                        "class-customcmp"
                        (lines
                            [
                                "[<CustomEquality; CustomComparison>]"
                                "type Ranked(id: int) ="
                                "    member _.Id = id"
                                "    override this.Equals(o: obj) = false"
                                "    override this.GetHashCode() = id"
                                "    interface System.IEquatable<Ranked> with"
                                "        member this.Equals(other: Ranked) = (id = other.Id)"
                                "    interface System.IComparable<Ranked> with"
                                "        member this.CompareTo(other: Ranked) ="
                                "            if other.Id < id then -1"
                                "            elif other.Id > id then 1"
                                "            else 0"
                                "let a = Ranked(1)"
                                "let b = Ranked(2)"
                                "printfn \"%b\" (a < b)"
                                "printfn \"%b\" (a > b)"
                                "printfn \"%b\" (a <= b)"
                                "printfn \"%b\" (a >= b)"
                                "printfn \"%b\" (a <= a)"
                                "printfn \"%b\" (a >= a)"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "false\ntrue\nfalse\ntrue\ntrue\ntrue"
                        "`<`/`>`/`<=`/`>=` dispatch to IComparable<Ranked>.CompareTo (inverted order)"
            }

            // ---- custom-hash dispatch slot: execution proves `hash` routes through GetHashCode ----
            //
            // `Hashed.GetHashCode` returns a CONSTANT 42, distinguishable from any structural hash.
            // Proves `hash a` → `Vesper.Core.structuralHash` → `hashOf` → `a.GetHashCode()`.
            test "a [<CustomEquality>] class dispatches `hash` to the attached GetHashCode on JS (constant)" {
                match
                    runJs
                        "class-customhash"
                        (lines
                            [
                                "[<CustomEquality; NoComparison>]"
                                "type Hashed(id: int) ="
                                "    member _.Id = id"
                                "    override this.Equals(o: obj) = false"
                                "    override this.GetHashCode() = 42"
                                "    interface System.IEquatable<Hashed> with"
                                "        member this.Equals(other: Hashed) = (id = other.Id)"
                                "let a = Hashed(7)"
                                "printfn \"%d\" (hash a)"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "`hash` dispatches to GetHashCode (constant 42), not a structural hash"
            }
        ]
