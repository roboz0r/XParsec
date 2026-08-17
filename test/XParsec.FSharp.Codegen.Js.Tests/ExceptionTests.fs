module XParsec.FSharp.Codegen.Js.Tests.ExceptionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

let private lines xs = String.concat "\n" xs

/// The generated `Vesper.Core/exceptions.mjs`: the BCL-shaped roster as real classes.
let private generated: Lazy<string> =
    lazy
        compileOwnLibrary
            coreDepsJsContract.Value
            "Vesper.Core"
            "exceptions.js.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Core" "exceptions.js.fs"))

/// Normalise line endings so a CRLF checkout still matches the printer's `\n` output.
let private lf (s: string) : string = s.Replace("\r\n", "\n")

[<Tests>]
let tests =
    testList
        "Codegen.Js Exceptions"
        [
            // ---- failwith ----

            test "the non-failing branch of a `failwith` evaluates normally" {
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

            // ---- the roster's own module ----

            test "the roster emits real classes whose `extends` chain is the BCL's" {
                let src = generated.Value

                // `exn` carries the `(# class "Error" #)` repr, so the root extends the JS
                // global; every other root extends the class the BCL derives it from.
                for derived, baseName in
                    [
                        "SystemException", "Error"
                        "InvalidOperationException", "SystemException"
                        "ArgumentException", "SystemException"
                        "ArgumentNullException", "ArgumentException"
                        "NotSupportedException", "SystemException"
                        "IndexOutOfRangeException", "SystemException"
                        "FormatException", "SystemException"
                    ] do
                    Expect.stringContains
                        src
                        (sprintf "class %s extends %s {" derived baseName)
                        (sprintf "%s derives from %s, as it does on the CLR" derived baseName)
            }

            test "each constructor chains the message through `super`" {
                Expect.stringContains
                    (generated.Value)
                    "constructor(message) {\n    super(message);"
                    "the base ctor runs, so `Error` records the message"
            }

            test "the committed exceptions.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.Core" "exceptions.mjs"

                if Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" = "1" then
                    IO.File.WriteAllText(path, generated.Value)

                Expect.equal
                    (lf generated.Value)
                    (lf (IO.File.ReadAllText path))
                    "committed asset is stale — rerun with UPDATE_SNAPSHOTS=1"
            }

            // ---- construction at a use site ----

            test "raise of a constructed exception imports the class and constructs it" {
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (System.InvalidOperationException \"boom\")"

                Expect.stringContains
                    src
                    "from \"./Vesper.Core/index.mjs\""
                    "the class comes from the package barrel, not a bare `Error`"

                Expect.stringContains src "new $Vesper_Core_InvalidOperationException(" "constructed nominally"
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

            // `open System` is required for the bare spelling: the exception roots are
            // declared in `namespace System`, and only the language prelude is implicitly
            // open.
            test "a different exception type resolves to its own class through the contract chain" {
                let src =
                    emitJs "open System\nlet f (b: bool) = if b then 7 else raise (ArgumentException \"bad\")"

                Expect.stringContains src "new $Vesper_Core_ArgumentException(" "keyed by its own BCL name"
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

            // The point of the whole nominal lowering: the prototype chain answers the
            // subtype question the CLR answers with a type test.
            test "a caught ArgumentNullException IS an ArgumentException, and IS an Error" {
                let probe =
                    "(() => { try { return $0; } catch (e) { "
                    + "console.log(e.constructor.name); "
                    + "console.log(e instanceof Error); "
                    + "return 0; } })()"

                match
                    runJs
                        "exn-catch-subtype"
                        ("open System\n"
                         + "let f (b: bool) = if b then 7 else raise (ArgumentNullException \"paramName\")\n"
                         + sprintf "let g () = (# \"%s\" (f false) : int #)\n" probe
                         + "printfn \"%d\" (g ())")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "the caught throw lets the program exit zero (%s)" out)
                    Expect.stringContains out "ArgumentNullException" "the thrown value keeps its own identity"
                    Expect.stringContains out "true" "and is still an `Error`, through the chain"
            }
        ]
