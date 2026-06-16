module XParsec.FSharp.Codegen.Js.Tests.Step7Tests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 7 — instance / static type members. A member declared on a
// record / union is emitted as a *free, curried, receiver-first* function under a
// shared mangled name (mirroring Fable): an instance method → `<Type>__<member>`, an
// instance property getter → `<Type>__get_<Prop>`, a static member →
// `<Type>_<member>`. The data-carrying class stays exactly as Steps 3–4 emit it
// (constructor + fields, no methods), so the Step-5b/6 structural-interop invariant
// (`.tag` + own data keys, never a prototype method / `instanceof`) is untouched: a
// free function reads `this$.X` and works on a plain runtime cell and a class
// instance alike. The call arms (`PropertyGet` / `MethodCall` / `StaticPropertyGet`
// / `StaticMethodCall`) lower to a `Call` of the mangled name, curried over
// receiver-then-args; `ExternalMember` imports the mangled member from the
// declaring type's `runtime-js` module. Golden text plus execution under Node.

let private lines xs = String.concat "\n" xs

// The proven union-with-members shape (mirrors the CLR backend's `memberUnionSrc`):
// instance properties (`IsEmpty`/`Head`/`Length`), an instance method (`Add`), and
// static members (`Empty`/`Single`). `Head`'s `Nil` arm yields `0` rather than
// `failwith` (the `raise`/`throw` arm is a separate adjacent slice).
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

                // Instance property getter: a single receiver-first arrow, named under
                // the double-underscore + `get_` mangling. (The `this` binder is
                // synthetic in the frozen TAST, so the receiver name is a stable
                // `_s<n>` rather than `this$` — the body's `this` refs carry the same
                // key, so they line up regardless.)
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
        ]
