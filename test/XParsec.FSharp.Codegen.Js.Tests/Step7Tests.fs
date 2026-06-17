module XParsec.FSharp.Codegen.Js.Tests.Step7Tests

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
        "Codegen.Js Step7"
        [
            // ---- golden text: emission shape ----

            test "members emit as mangled free functions (instance props/method, static)" {
                let src = emitJs memberUnion

                Expect.stringContains src "const Lst__get_IsEmpty = (" "instance property getter, mangled name"
                Expect.stringContains src "const Lst__get_Length = (" "recursive instance property getter"
                // Instance method: receiver, then one curried arrow per argument.
                Expect.stringContains src "const Lst__AddHead = (" "instance method, mangled name"
                Expect.stringContains src ") => (n) =>" "method curries the receiver then its argument"
                // Static members: single underscore, no receiver.
                Expect.stringContains src "const Lst_Empty = new Lst_Nil()" "static property is a value binding"
                Expect.stringContains src "const Lst_Single = (x) =>" "static method drops the receiver"
            }

            test "an external member imports its mangled name from the runtime module" {
                let src = emitJs "let f (o: int option) = o.IsSome"

                Expect.stringContains
                    src
                    "import { Option__get_IsSome as $Option__get_IsSome } from \"./Vesper.Option.mjs\""
                    "the consumer imports the mangled member export under its `$`-aliased name"

                Expect.stringContains src "$Option__get_IsSome(o)" "and applies it receiver-first"
            }

            // ---- execution under Node ----

            test "an instance property dispatches on the case tag (IsEmpty)" {
                match
                    runJs
                        "step7-isempty"
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

            test "a recursive instance property (Length) and Head read fields" {
                match
                    runJs
                        "step7-length-head"
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

            test "an instance method takes the receiver then its argument (AddHead)" {
                match
                    runJs "step7-method" (memberUnion + "\nlet xs = Cons(10, Nil)\nprintfn \"%d\" (xs.AddHead 5)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "15" "Lst__AddHead(this$)(n) = h + n"
            }

            test "a static property reads as a value binding (Lst.Empty)" {
                match runJs "step7-static-prop" (memberUnion + "\nlet e = Lst.Empty\nprintfn \"%b\" e.IsEmpty") with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "Lst_Empty is the Nil value the StaticPropertyGet reads"
            }

            test "a static method applies its argument (Lst.Single)" {
                match
                    runJs "step7-static-method" (memberUnion + "\nlet xs = Lst.Single 42\nprintfn \"%d\" xs.Head")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "Lst_Single(x) = Cons(x, Nil)"
            }

            test "failwith lowers to a throwing IIFE (a non-thrown branch returns)" {
                match
                    runJs "step7-failwith-ok" "let f b = if b then 7 else failwith \"boom\"\nprintfn \"%d\" (f true)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "the non-failing branch evaluates normally"
            }

            test "failwith throws a native Error with the message (uncaught → non-zero exit)" {
                match
                    runJs
                        "step7-failwith-throw"
                        "let f b = if b then 7 else failwith \"boom\"\nprintfn \"%d\" (f false)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "boom" "the Error carries the failwith message"
            }

            test "raise of a constructed exception emits `new Error(msg)`" {
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"boom\")"

                Expect.stringContains src "new Error(" "the exception construction lowers to `new Error`"
                Expect.stringContains src "throw" "the `raise` template throws the constructed error"
            }

            test "raise of a constructed exception throws with the message (uncaught → non-zero)" {
                match
                    runJs
                        "step7-raise-exn"
                        ("let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"kaboom\")\n"
                         + "printfn \"%d\" (f false)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "kaboom" "the Error carries the exception's message"
            }

            test "raise of a constructed exception leaves the non-raising branch intact" {
                match
                    runJs
                        "step7-raise-exn-ok"
                        ("let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"kaboom\")\n"
                         + "printfn \"%d\" (f true)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "the non-raising branch evaluates normally"
            }

            test "a member body can failwith on the empty case (list-style Head)" {
                let lst =
                    lines
                        [
                            "type Lst ="
                            "    | Nil"
                            "    | Cons of int * Lst"
                            ""
                            "    member this.Head ="
                            "        match this with"
                            "        | Cons(h, _) -> h"
                            "        | Nil -> failwith \"The input list was empty.\""
                        ]

                match runJs "step7-member-failwith" (lst + "\nlet e = Nil\nprintfn \"%d\" e.Head") with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "Nil.Head throws"
                    Expect.stringContains out "The input list was empty." "the member body's failwith surfaces"
            }
        ]
