module XParsec.FSharp.Codegen.Js.Tests.ExceptionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

[<Tests>]
let tests =
    testList
        "Codegen.Js Exceptions"
        [
            // ---- failwith ----

            test "failwith lowers to a throwing IIFE (a non-thrown branch returns)" {
                match
                    runJs "exn-failwith-ok" "let f b = if b then 7 else failwith \"boom\"\nprintfn \"%d\" (f true)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "the non-failing branch evaluates normally"
            }

            test "failwith throws a native Error with the message (uncaught → non-zero exit)" {
                match
                    runJs "exn-failwith-throw" "let f b = if b then 7 else failwith \"boom\"\nprintfn \"%d\" (f false)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "boom" "the Error carries the failwith message"
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

                match runJs "exn-member-failwith" (lst + "\nlet e = Nil\nprintfn \"%d\" e.Head") with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "Nil.Head throws"
                    Expect.stringContains out "The input list was empty." "the member body's failwith surfaces"
            }

            // ---- raise of a constructed exception → `new Error` (exn erases to the JS Error root) ----

            test "raise of a constructed exception emits `new Error(msg)`" {
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"boom\")"

                Expect.stringContains src "new Error(" "the exception construction lowers to `new Error`"
                Expect.stringContains src "throw" "the `raise` template throws the constructed error"
            }

            test "raise of a constructed exception throws with the message (uncaught → non-zero)" {
                match
                    runJs
                        "exn-raise"
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
                        "exn-raise-ok"
                        ("let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"kaboom\")\n"
                         + "printfn \"%d\" (f true)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "the non-raising branch evaluates normally"
            }

            // `open System` is required for the bare spelling: `Vesper.Exceptions` declares
            // its roots in `namespace System`, and only the language prelude
            // (`RuntimeNames.preludeNamespaces`) is implicitly open.
            test "a constructed exception lowers to `new Error(message)` (exn repr from contract)" {
                let src =
                    emitJs "open System\nlet f (b: bool) = if b then 7 else raise (InvalidOperationException \"boom\")"

                Expect.stringContains src "new Error(" "the exception lowers to `new Error` (exn → Error)"
            }

            test "a different exception type also resolves through the contract chain" {
                // Every exception in the contract erases to the one `exn` root — not a per-name special case.
                let src =
                    emitJs "open System\nlet f (b: bool) = if b then 7 else raise (ArgumentException \"bad\")"

                Expect.stringContains src "new Error(" "ArgumentException erases to the `exn` root too"
                Expect.isFalse (src.Contains "ArgumentException") "the BCL name does not leak into the output"
            }

            test "a constructed exception throws with its message under Node (uncaught → non-zero)" {
                match
                    runJs
                        "exn-raise-notsupported"
                        ("let f (b: bool) = if b then 7 else raise (System.NotSupportedException \"nope\")\n"
                         + "printfn \"%d\" (f false)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "nope" "the native Error carries the exception's message"
            }

            test "a caught exception is a JS Error carrying the message" {
                // The thrown value is a genuine `Error`, so `catch (e)` reads `e.message`.
                match
                    runJs
                        "exn-catch-error"
                        ("open System\n"
                         + "let f (b: bool) = if b then 7 else raise (InvalidOperationException \"kaboom\")\n"
                         + "let g () = (# \"(() => { try { return $0; } catch (e) { console.log(e.message); return 0; } })()\" (f false) : int #)\n"
                         + "printfn \"%d\" (g ())")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 "the caught throw lets the program exit zero"
                    Expect.stringContains out "kaboom" "the caught Error carries the message"
            }
        ]
