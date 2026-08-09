module XParsec.FSharp.Codegen.Js.Tests.ClassEmitTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

// JS class emission, and the runtime slots that `=`, `<` and `hash` dispatch through on a
// class overriding them.

[<Tests>]
let tests =
    testList
        "Codegen.Js ClassEmit"
        [
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
                // Regular members are FREE type-prefixed functions (tree-shaking).
                Expect.stringContains src "const Box__get_Value = (" "instance property → free function"
                Expect.stringContains src "const Box__Plus = (" "instance method → free function"
                Expect.stringContains src "new Box(7)" "instantiation lowers to `new`"
            }

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

            test "a [<CustomEquality>] class attaches its dispatch slots under registry symbols" {
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
                // The obj-typed `Object.Equals` override is dropped: the interface impl wins
                // the slot.
                Expect.stringContains
                    src
                    "[Symbol.for(\"vesper.equality\")](other)"
                    "typed IEquatable.Equals attaches as the registry-symbol method"

                Expect.isFalse (src.Contains "Equals(other)") "the named `Equals(` method form is gone (re-keyed)"

                Expect.stringContains
                    src
                    "[Symbol.for(\"vesper.hash\")]()"
                    "override GetHashCode attaches as the registry-symbol method"
            }

            // `Tagged.Equals` compares ONLY `id`, so `a = b` (same id, different payload) is
            // `true` only via the custom member: structural equality would say `false`
            // (the payload differs), and reference equality `false` too.
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

            // `Ranked.CompareTo` orders by id DESCENDING, so `a(1) < b(2)` is FALSE under the
            // custom member where a natural ordering would give `true`.
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

            // `Hashed.GetHashCode` returns a CONSTANT 42, distinguishable from any structural
            // hash.
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

            // A record is one JS class, so a local (non-capability) interface impl attaches as
            // a plain instance method on it, not under a registry symbol.
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

            // Unlike the record above, a class's impl body can reach a ctor parameter, through
            // the field the primary constructor stores.
            let offsetSrc =
                lines
                    [
                        "type IAdder ="
                        "    abstract member Add : int -> int"
                        "type Offset(k: int) ="
                        "    interface IAdder with"
                        "        member _.Add(n) = k + n"
                        "let a = Offset(10) :> IAdder"
                    ]

            test "a class implementing a local interface emits the impl as an attached method" {
                let src = emitJs (offsetSrc + "\nprintfn \"%d\" (a.Add 5)")

                Expect.stringContains src "class Offset {" "the class emits"
                Expect.stringContains src "this.k = k;" "the ctor param it captures is a stored field"
                Expect.stringContains src "Add(n)" "the IAdder.Add impl attaches as an instance method"
                // The upcast is an identity: one JS class, no wrapper object.
                Expect.stringContains src "const a = new Offset(10);" "`:> IAdder` erases"
            }

            test "a class's interface impl reads a captured ctor param through the upcast (10 + 5 = 15)" {
                match runJs "class-localiface" (offsetSrc + "\nprintfn \"%d\" (a.Add 5)") with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "15" "the attached method adds its argument to the ctor-stored field"
            }

            // The instance preamble's RUNTIME semantics live in the cross-backend corpus
            // (`test/Codegen.Conformance/classes/preamble-*.fs`, run on JS and CLR). Here:
            // the emitted shape, and the shapes the JS backend must reject.
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

                // The initialiser reads the ctor param through `this`, so it can only run after
                // the param store.
                let paramStore = src.IndexOf "this.n = n;"
                let letStore = src.IndexOf "this.m = "
                Expect.isGreaterThan paramStore -1 "the ctor param field store emits"
                Expect.isGreaterThan letStore paramStore "the instance-`let` store follows the ctor-param store"
            }

            // `static let mutable` sharing is pinned in the conformance corpus
            // (`classes/static-mutable.fs`, run on JS and CLR).
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

                let classDecl = src.IndexOf "class C"
                let scaleInit = src.IndexOf "C.scale = "
                Expect.isGreaterThan classDecl -1 "the class emits"
                Expect.isGreaterThan scaleInit classDecl "the static field is initialised after the class is declared"
                Expect.stringContains src "C.scale" "the member reads the static field off the class object"
            }

            // No `extends` / `super(...)` is emitted, so an admitted `inherit` would run neither
            // the base ctor nor its `do`, and the base's members would be absent from the
            // prototype: a program the CLR backend compiles correctly, silently mis-run here.
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

            // A positional ctor over the declared fields would take the wrong arity here, so
            // every field the ctor initialises itself would arrive `undefined`, silently for
            // any field whose default happens to be falsy.
            test "a `val`-form class's explicit ctor keeps its own arity, not the field count" {
                let src =
                    emitJs (
                        lines
                            [
                                "type Counter ="
                                "    val mutable Cur : int"
                                "    val mutable Started : bool"
                                "    new(cur: int) = { Cur = cur; Started = false }"
                                "    member this.Sum = this.Cur"
                                "let c = Counter(7)"
                                "printfn \"%d\" c.Sum"
                            ]
                    )

                Expect.stringContains src "constructor(cur)" "the ctor's own single parameter"
                Expect.stringContains src "this.Started = false;" "the initialiser the source wrote, not a parameter"
                Expect.isFalse (src.Contains "constructor(Cur, Started)") "not positional over the declared fields"
            }

            test "a `val`-form class's explicitly initialised field is a real value under Node" {
                match
                    runJs
                        "class-val-form-ctor"
                        (lines
                            [
                                "type Counter ="
                                "    val mutable Cur : int"
                                "    val mutable Started : bool"
                                "    new(cur: int) = { Cur = cur; Started = false }"
                                "    member this.Report = this.Cur"
                                "let c = Counter(7)"
                                "printfn \"%d\" c.Report"
                                "printfn \"%b\" c.Started"
                            ])
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7\nfalse" "`Started` is `false`, not an absent property printing as undefined"
            }

            // JS has ONE constructor per class, so a SECOND arity (here a primary ctor plus a
            // `new(…)`) has nowhere to go, and a call at that arity would land in the survivor
            // with the wrong arguments.
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
                            "declares 2 constructors; a JS class has exactly one"
                            "the failure names the unsupported feature rather than dropping it"
                    )
            }
        ]
