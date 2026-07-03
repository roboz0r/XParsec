module XParsec.FSharp.Codegen.Clr.Tests.TypeTestAsBinderTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Regression probes for two `structural-printer.fs` workarounds that were needed
// against 2026-06 codegen but have since been fixed (many `:? T as x` arms in a
// member body; a module `let` read from a member), and for the interface-receiver
// member-resolution shapes the printer relies on — including a `System.Object`-
// inherited member on an interface receiver, which needed the CLR metadata
// provider to walk through to `Object` (MetadataSymbols.computeMembers). Every
// probe is expected to PASS; a failure means a regression.

[<Tests>]
let tests =
    testList
        "TypeTestAsBinder"
        [
            // Issue (1): many `:? T as x` arms — all binders named `x`, distinct
            // types — in an INTERFACE-IMPL member body. Each arm reads its own
            // binder (the `if x > 0` guard), so a dropped/collided slot would
            // mis-branch or emit invalid IL. Directly refutes the name-collision
            // hypothesis: keys are source-position based, so identically-named
            // binders in different arms must still route to their own slot.
            test "many same-named `:? T as x` arms in an interface member bind per-arm" {
                runsSelfHostLines
                    [ "11"; "21"; "31"; "41"; "51" ]
                    (String.concat
                        "\n"
                        [
                            "type IPick ="
                            "    abstract member Pick: obj -> int"
                            "type C() ="
                            "    interface IPick with"
                            "        member this.Pick (value: obj) : int ="
                            "            match value with"
                            "            | :? int as x -> if x > 0 then 11 else 12"
                            "            | :? int64 as x -> if x > 0L then 21 else 22"
                            "            | :? int16 as x -> if x > 0s then 31 else 32"
                            "            | :? byte as x -> if x > 0uy then 41 else 42"
                            "            | :? bool as x -> if x then 51 else 52"
                            "            | _ -> -1"
                            "let c = C() :> IPick"
                            "printfn \"%d\" (c.Pick (box 5))"
                            "printfn \"%d\" (c.Pick (box 5L))"
                            "printfn \"%d\" (c.Pick (box 5s))"
                            "printfn \"%d\" (c.Pick (box 5uy))"
                            "printfn \"%d\" (c.Pick (box true))"
                        ])
            }

            // Issue (2): a module-level `let` value read from inside an
            // interface-impl member body (the `ldsfld` the workaround comment
            // claims is unresolved in the member emit environment).
            test "a module `let` value resolves when read from an interface member" {
                runsSelfHostLines
                    [ "105" ]
                    (String.concat
                        "\n"
                        [
                            "let bump = 100"
                            "type IAdd ="
                            "    abstract member Add: int -> int"
                            "type C() ="
                            "    interface IAdd with"
                            "        member this.Add (n: int) : int = n + bump"
                            "let c = C() :> IAdd"
                            "printfn \"%d\" (c.Add 5)"
                        ])
            }

            // Interface-DECLARED method on an interface-typed `as`-binder
            // (`IFormattable.ToString(string, provider)`). This is the shape the
            // printer's `formatPrimitive` `IFormattable` arm needs — verified to
            // WORK, so that arm can drop its `:?>`-cast workaround.
            test "interface-declared method on an interface-typed `as`-binder works" {
                runsSelfHost
                    "42"
                    (String.concat
                        "\n"
                        [
                            "type IShow ="
                            "    abstract member Show: obj -> string"
                            "type C() ="
                            "    interface IShow with"
                            "        member this.Show (value: obj) : string ="
                            "            match value with"
                            "            | :? System.IFormattable as f -> f.ToString(null, null)"
                            "            | _ -> \"?\""
                            "let c = C() :> IShow"
                            "printfn \"%s\" (c.Show (box 42))"
                        ])
            }

            // A method call on a VALUE-TYPE `as`-binder (`d.ToString(...)` on a
            // `double`) inside a module function — the exact shape the printer's
            // `formatPrimitive` needs to drop its `:?>`-cast workaround.
            test "method call on a value-type `as`-binder works (module fn)" {
                runsSelfHostLines
                    [ "1.5"; "2.5f"; "42" ]
                    (String.concat
                        "\n"
                        [
                            "open System.Globalization"
                            "let fmt (value: obj) : string ="
                            "    match value with"
                            "    | :? double as d -> d.ToString(null, CultureInfo.InvariantCulture)"
                            "    | :? single as f -> f.ToString(null, CultureInfo.InvariantCulture) + \"f\""
                            "    | _ -> value.ToString()"
                            "printfn \"%s\" (fmt (box 1.5))"
                            "printfn \"%s\" (fmt (box 2.5f))"
                            "printfn \"%s\" (fmt (box 42))"
                        ])
            }

            // A PROPERTY on an interface-typed `as`-binder (`ITuple.Length`) —
            // interface-declared, resolves correctly.
            test "property on an interface-typed `as`-binder works (ITuple.Length)" {
                runsSelfHost
                    "3"
                    (String.concat
                        "\n"
                        [
                            "open System.Runtime.CompilerServices"
                            "type IShow ="
                            "    abstract member Show: obj -> int"
                            "type C() ="
                            "    interface IShow with"
                            "        member this.Show (value: obj) : int ="
                            "            match value with"
                            "            | :? ITuple as t -> t.Length"
                            "            | _ -> -1"
                            "let c = C() :> IShow"
                            "printfn \"%d\" (c.Show (box (1, 2, 3)))"
                        ])
            }

            // A `System.Object`-inherited member (`ToString`/`Equals`/`GetHashCode`)
            // on an interface-typed receiver. These are inherited, not declared on
            // the interface, so the CLR metadata provider must walk through to
            // `System.Object` to surface them (MetadataSymbols.computeMembers). On a
            // string boxed as `obj` the `IEnumerable` arm fires and `xs.ToString()`
            // returns the string itself.
            test "Object-inherited member on an interface receiver resolves (IEnumerable.ToString)" {
                runsSelfHost
                    "abc"
                    (String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "type IShow ="
                            "    abstract member Show: obj -> string"
                            "type C() ="
                            "    interface IShow with"
                            "        member this.Show (value: obj) : string ="
                            "            match value with"
                            "            | :? IEnumerable as xs -> xs.ToString()"
                            "            | _ -> \"?\""
                            "let c = C() :> IShow"
                            "printfn \"%s\" (c.Show (box \"abc\"))"
                        ])
            }

            // An `Object`-inherited member on an external CLASS receiver. On the CLR
            // every class inherits `Equals`/`GetHashCode`/`ToString` from `Object`, so
            // the provider walks the class's base chain (which terminates at `Object`).
            // `StringBuilder` does not override `Equals`, so `sb.Equals(sb)` resolves to
            // `Object.Equals` — reference-equal, hence true.
            test "Object-inherited member on an external class receiver resolves (StringBuilder.Equals)" {
                runsSelfHost
                    "true"
                    (String.concat
                        "\n"
                        [
                            "open System.Text"
                            "let sb = StringBuilder()"
                            "printfn \"%b\" (sb.Equals(sb :> obj))"
                        ])
            }

            // An INTERMEDIATE-base member on an external class receiver: `Message` is
            // declared on `System.Exception`, not on `ArgumentException`, so resolving
            // `e.Message` needs the base-chain walk (ArgumentException -> SystemException
            // -> Exception), not just `Object`.
            test "intermediate-base member on an external class receiver resolves (Exception.Message)" {
                runsSelfHost
                    "boom"
                    (String.concat "\n" [ "let e = System.ArgumentException(\"boom\")"; "printfn \"%s\" e.Message" ])
            }

            // OVERLOADS split across the hierarchy must ALL survive the base-chain
            // merge (the case a first-level-wins `tryPick` got wrong). `StringWriter`
            // overrides `Write(string)` but inherits `Write(bool)` from `TextWriter`, so
            // a single call site using both proves the base overload is still a
            // candidate alongside the derived one.
            test "overloads split across base classes both resolve (TextWriter/StringWriter.Write)" {
                runsSelfHost
                    "Truex"
                    (String.concat
                        "\n"
                        [
                            "open System.IO"
                            "let sw = new StringWriter()"
                            "sw.Write(true)"
                            "sw.Write(\"x\")"
                            "printfn \"%s\" (sw.ToString())"
                        ])
            }

            // An OVERRIDE must not double-count: `StringBuilder` overrides
            // `ToString()` (0 args) and `Object` also declares `ToString()` (0 args).
            // The merge dedups by signature so only the most-derived survives — no
            // ambiguity — and the builder's own content is returned.
            test "an overridden member is not double-counted across the base chain (StringBuilder.ToString)" {
                runsSelfHost
                    "hi"
                    (String.concat "\n" [ "open System.Text"; "printfn \"%s\" (StringBuilder(\"hi\").ToString())" ])
            }
        ]
