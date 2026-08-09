module XParsec.FSharp.Codegen.Clr.Tests.TypeTestAsBoundVarTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Member-resolution probes on `as`-bound variables: many `:? T as x` arms in one member
// body, a module `let` read from a member, and interface / value-type / base-inherited
// members on an interface-typed object argument.

[<Tests>]
let tests =
    testList
        "TypeTestAsBoundVar"
        [
            // Every arm binds a variable named `x` at a distinct type and reads it in its
            // own `if x > 0` guard. Keys are source-position based, so identically-named
            // bound variables in different arms still route to their own slot.
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

            // A module-level `let` read from inside an interface-impl member body (an `ldsfld`).
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

            test "interface-declared method on an interface-typed `as`-bound variable works" {
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

            test "method call on a value-type `as`-bound variable works (module fn)" {
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

            test "property on an interface-typed `as`-bound variable works (ITuple.Length)" {
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

            // `ToString`/`Equals`/`GetHashCode` are inherited from `System.Object`, not declared
            // on the interface, so the metadata provider must walk through to `Object`. On a
            // string boxed as `obj` the `IEnumerable` arm fires and `xs.ToString()` is "abc".
            test "Object-inherited member on an interface object argument resolves (IEnumerable.ToString)" {
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

            // A class's base chain terminates at `Object`. `StringBuilder` does not override
            // `Equals`, so `sb.Equals(sb)` resolves to `Object.Equals`, which compares by
            // reference and so returns true.
            test "Object-inherited member on an external class object argument resolves (StringBuilder.Equals)" {
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

            // `Message` is declared on `System.Exception`, not `ArgumentException`, so this
            // needs the full walk (ArgumentException -> SystemException -> Exception).
            test "intermediate-base member on an external class object argument resolves (Exception.Message)" {
                runsSelfHost
                    "boom"
                    (String.concat "\n" [ "let e = System.ArgumentException(\"boom\")"; "printfn \"%s\" e.Message" ])
            }

            // `StringWriter` overrides `Write(string)` but inherits `Write(bool)` from
            // `TextWriter`, so using both proves the base overload survives the base-chain
            // merge as a candidate alongside the derived one.
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

            // `StringBuilder` overrides `ToString()` and `Object` declares it too; the merge
            // dedups by signature, so only the most-derived survives and there is no ambiguity.
            test "an overridden member is not double-counted across the base chain (StringBuilder.ToString)" {
                runsSelfHost
                    "hi"
                    (String.concat "\n" [ "open System.Text"; "printfn \"%s\" (StringBuilder(\"hi\").ToString())" ])
            }
        ]
