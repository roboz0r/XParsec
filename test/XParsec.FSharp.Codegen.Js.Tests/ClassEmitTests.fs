module XParsec.FSharp.Codegen.Js.Tests.ClassEmitTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

// General JS class emission and the custom-equality dispatch slot.
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
                // The IEquatable<Self>.Equals impl attaches as a COMPUTED-KEY registry-symbol
                // method `[Symbol.for("vesper.equality")](other)` (the runtime hook
                // `a[Symbol.for("vesper.equality")](b)` finds it). The redundant obj-typed
                // `Object.Equals` override is dropped (interface wins the slot); the old named
                // `Equals(` method form is GONE (re-keyed to the symbol — plan §14.5).
                Expect.stringContains
                    src
                    "[Symbol.for(\"vesper.equality\")](other)"
                    "typed IEquatable.Equals attaches as the registry-symbol method"

                Expect.isFalse (src.Contains "Equals(other)") "the named `Equals(` method form is gone (re-keyed)"
                // The `override GetHashCode` attaches as `[Symbol.for("vesper.hash")]()` so
                // `hashOf` can find it via the registry symbol.
                Expect.stringContains
                    src
                    "[Symbol.for(\"vesper.hash\")]()"
                    "override GetHashCode attaches as the registry-symbol method"
            }

            // ---- custom-equality dispatch slot: execution proves the slot is live ----
            //
            // The fuller custom-eq round-trip oracle is later work; this is a single
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

            // ---- §14.6 slice 5: a RECORD implementing a LOCAL interface ----
            //
            // A record is a single JS class. Implementing a non-capability local
            // interface routes the impl through the SAME partition the class path uses:
            // a local interface → an ATTACHED instance method on the record's class
            // (the plain-attached path, not the registry-symbol/iterator path). The
            // method reads a field of `this`.

            test "a record implementing a local interface emits the impl as an attached method on its class" {
                let src =
                    emitJs (
                        lines
                            [
                                "type IRank ="
                                "    abstract member Rank : unit -> int"
                                "type R ="
                                "    { N: int }"
                                "    interface IRank with"
                                "        member this.Rank() = this.N"
                                "let r = { N = 7 }"
                                "printfn \"%d\" ((r :> IRank).Rank())"
                            ]
                    )

                Expect.stringContains src "class R {" "the record emits as a JS class"

                Expect.stringContains
                    src
                    "Rank()"
                    "the IRank.Rank impl attaches as an instance method on the record class"
            }

            test
                "a record implementing a local interface dispatches `Rank()` through the attached method on JS (prints 7)" {
                match
                    runJs
                        "record-localiface"
                        (lines
                            [
                                "type IRank ="
                                "    abstract member Rank : unit -> int"
                                "type R ="
                                "    { N: int }"
                                "    interface IRank with"
                                "        member this.Rank() = this.N"
                                "let r = { N = 7 }"
                                "printfn \"%d\" ((r :> IRank).Rank())"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "(r :> IRank).Rank() dispatches to the attached method reading this.N (=7)"
            }

            // ---- instance preamble (`let` / `do`) ----
            //
            // The RUNTIME semantics of the instance preamble (declaration order,
            // `let mutable` sharing one storage location, `let rec`, generics, a
            // function-`let` used first-class) are owned by the cross-backend
            // conformance corpus (`test/Codegen.Conformance/classes/preamble-*.fs`, run
            // on JS *and* CLR). What lives here is what the corpus cannot see: the
            // EMITTED SHAPE, and the shapes the JS backend must reject loudly.

            test "an instance `let` emits a ctor field store AFTER the ctor-param stores" {
                let src =
                    emitJs (
                        lines
                            [
                                "type C(n: int) ="
                                "    let m = n + 1"
                                "    member _.M = m"
                                "printfn \"%d\" (C(41).M)"
                            ]
                    )

                // The initialiser reads the ctor param through `this`, so it can only run
                // after the param store — the order is the semantics.
                let paramStore = src.IndexOf "this.n = n;"
                let letStore = src.IndexOf "this.m = "
                Expect.isGreaterThan paramStore -1 "the ctor param field store emits"
                Expect.isGreaterThan letStore paramStore "the instance-`let` store follows the ctor-param store"
            }

            // A `static let` backing field is a property on the emitted class object
            // (`C.scale`), initialised by a module-load static preamble; a member reads the
            // same slot. The RUNTIME semantics of `static let mutable` sharing are pinned in
            // the conformance corpus (`classes/static-mutable.fs`, run on JS *and* CLR).
            test "a class `static let` emits a class-object field initialised at module load" {
                let src =
                    emitJs (
                        lines
                            [
                                "type C(n: int) ="
                                "    static let scale = 2 * 3"
                                "    member _.N = n * scale"
                                "printfn \"%d\" (C(2).N)"
                            ]
                    )

                // The class is declared before its static field is initialised, and the
                // member reads the same `C.scale` slot.
                let classDecl = src.IndexOf "class C"
                let scaleInit = src.IndexOf "C.scale = "
                Expect.isGreaterThan classDecl -1 "the class emits"
                Expect.isGreaterThan scaleInit classDecl "the static field is initialised after the class is declared"
                Expect.stringContains src "C.scale" "the member reads the static field off the class object"
            }

            // No `extends` / `super(...)` is emitted, so an admitted `inherit` would run neither
            // the base ctor nor its `do`, and the base's members would be absent from the
            // prototype — a program the CLR backend compiles correctly, silently mis-run here.
            test "a class with an `inherit` clause fails loudly on the JS target" {
                let compile () =
                    emitJs (
                        lines
                            [
                                "type Base(x: int) ="
                                "    do printfn \"base %d\" x"
                                "    member this.X () = x"
                                "type Derived(y: int) ="
                                "    inherit Base(y + 2)"
                                "    let z = y + 5"
                                "    do printfn \"derived z=%d\" z"
                                "let d = Derived(3)"
                                "printfn \"%d\" (d.X ())"
                            ]
                    )
                    |> ignore

                Expect.throwsC
                    compile
                    (fun ex ->
                        Expect.stringContains
                            ex.Message
                            "class inheritance is not yet supported"
                            "the failure names the unsupported feature rather than dropping it"
                    )
            }

            // JS has ONE constructor per class. On the `val`-form class the positional field ctor
            // IS the lowering of its field-initialising `new(…) = { … }` (and `Vesper.List`'s
            // enumerator relies on that) — but where a PRIMARY ctor already owns the one JS
            // constructor, a `new(…)` overload has nowhere to go: a call at its arity would land
            // in the primary with the wrong arguments.
            test "a secondary constructor alongside a primary one fails loudly on the JS target" {
                let compile () =
                    emitJs (
                        lines
                            [
                                "type C(n: int) ="
                                "    new () = C(1 + 1)"
                                "    member _.N = n"
                                "printfn \"%d\" (C().N)"
                            ]
                    )
                    |> ignore

                Expect.throwsC
                    compile
                    (fun ex ->
                        Expect.stringContains
                            ex.Message
                            "secondary constructor overloads are not yet supported"
                            "the failure names the unsupported feature rather than dropping it"
                    )
            }
        ]
