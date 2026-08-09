module XParsec.FSharp.Codegen.Js.Tests.MemberEmissionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

let private memberUnion =
    lines
        [
            "type Lst ="
            "    | Nil"
            "    | Cons of int * Lst"
            ""
            "    member this.IsEmpty ="
            "        match this with"
            "        | Nil -> true"
            "        | Cons(_, _) -> false"
            ""
            "    member this.Head ="
            "        match this with"
            "        | Cons(h, _) -> h"
            "        | Nil -> 0"
            ""
            "    member this.Length ="
            "        match this with"
            "        | Nil -> 0"
            "        | Cons(_, t) -> 1 + t.Length"
            ""
            "    member this.AddHead n ="
            "        match this with"
            "        | Cons(h, _) -> h + n"
            "        | Nil -> n"
            ""
            "    static member Empty = Nil"
            "    static member Single x = Cons(x, Nil)"
        ]

[<Tests>]
let tests =
    testList
        "Codegen.Js Type Members"
        [
            // ---- golden text: emission shape ----

            test "members emit as mangled free functions (instance props/method, static)" {
                let src = emitJs memberUnion

                Expect.stringContains src "const Lst__get_IsEmpty = (" "instance property getter, mangled name"
                Expect.stringContains src "const Lst__get_Length = (" "recursive instance property getter"
                // Instance method: object argument, then one curried arrow per argument.
                Expect.stringContains src "const Lst__AddHead = (" "instance method, mangled name"
                Expect.stringContains src ") => (n) =>" "method curries the object argument then its argument"
                // Static members: single underscore, no object argument.
                Expect.stringContains src "const Lst_Empty = new Lst_Nil()" "static property is a value binding"
                Expect.stringContains src "const Lst_Single = (x) =>" "static method drops the object argument"
            }

            test "an external member imports its mangled name from the runtime module" {
                let src = emitJs "let f (o: int option) = o.IsSome"

                Expect.stringContains
                    src
                    "import { Option__get_IsSome as $Option__get_IsSome } from \"./Vesper.Option.mjs\""
                    "the consumer imports the mangled member export under its `$`-aliased name"

                Expect.stringContains src "$Option__get_IsSome(o)" "and applies it to the object argument"
            }

            // ---- execution under Node ----

            test "an instance property dispatches on the case tag (IsEmpty)" {
                match
                    runJs
                        "member-isempty"
                        (memberUnion
                         + "\nlet xs = Cons(7, Cons(8, Nil))\n"
                         + "let e = Nil\n"
                         + "printfn \"%b\" xs.IsEmpty\n"
                         + "printfn \"%b\" e.IsEmpty")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "false\ntrue" "Lst__get_IsEmpty reads .tag"
            }

            test "a recursive instance property (Length) recurses; Head reads its field" {
                match
                    runJs
                        "member-length-head"
                        (memberUnion
                         + "\nlet xs = Cons(7, Cons(8, Cons(9, Nil)))\n"
                         + "printfn \"%d\" xs.Length\n"
                         + "printfn \"%d\" xs.Head")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3\n7" "get_Length recurses via t.Length; get_Head reads the field"
            }

            test "an instance method takes the object argument then its argument (AddHead)" {
                match
                    runJs "member-method" (memberUnion + "\nlet xs = Cons(10, Nil)\nprintfn \"%d\" (xs.AddHead 5)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "15" "Lst__AddHead(this$)(n) = h + n"
            }

            test "a static property reads as a value binding (Lst.Empty)" {
                match runJs "member-static-prop" (memberUnion + "\nlet e = Lst.Empty\nprintfn \"%b\" e.IsEmpty") with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "Lst_Empty is the Nil value the StaticPropertyGet reads"
            }

            test "a static method applies its argument (Lst.Single)" {
                match
                    runJs "member-static-method" (memberUnion + "\nlet xs = Lst.Single 42\nprintfn \"%d\" xs.Head")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "Lst_Single(x) = Cons(x, Nil)"
            }
        ]
