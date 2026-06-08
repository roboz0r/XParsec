module XParsec.FSharp.Codegen.Clr.Tests.LocalModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-g-wall.md G15 / G16 — local (in-file) module resolution.
//
// G15: a `let`-bound value/function of a sibling *local* module resolves
//      *qualified* (`A.f`), from another module's body and from a class member.
//      The module tree is flattened before name resolution, so without the
//      `LocalModules` pre-pass the sibling is unresolvable (the provider only
//      knows dependency packages, and a module name is not a value binding).
//
// G16: an *unqualified* reference from a type nested *inside* a module up to one
//      of that module's `let`-bound siblings (`SetIterator` → `collapseLHS` in
//      `set.fs`) resolves — the enclosing module's bindings enter the nested
//      type's member-body scope.
//
// Both ride the `runs` driver (compile → run → assert stdout) so the resolution
// fix is proven end to end: name resolution records the use site as an ordinary
// local `Binding`, Unification types it via the member's generalised scheme, and
// Freeze lowers it to the same `TExpr.Var` a bare local reference produces.

[<Tests>]
let tests =
    testList
        "LocalModule"
        [
            // ---- G15: qualified sibling-module resolution ---------------------
            test "G15: a module body calls a sibling module's let-bound function (A.f)" {
                runsLines
                    [ "11" ]
                    (String.concat
                        "\n"
                        [
                            "module A ="
                            "    let f x = x + 1"
                            "module B ="
                            "    let g y = A.f y"
                            "printfn \"%d\" (B.g 10)"
                        ])
            }

            test "G15: a class member body calls a local module's function (mirrors Set→SetTree)" {
                runsLines
                    [ "7" ]
                    (String.concat
                        "\n"
                        [
                            "module Tree ="
                            "    let twice x = x + x"
                            "    let inc x = x + 1"
                            "type Wrap(n: int) ="
                            "    member w.Value = Tree.inc (Tree.twice w.N)"
                            "    member w.N = n"
                            "let r = Wrap(3)"
                            "printfn \"%d\" r.Value"
                        ])
            }

            // ---- G16: unqualified enclosing-module reference from a nested type
            test "G16: a struct nested in a module calls a let-bound module sibling unqualified" {
                runsLines
                    [ "42" ]
                    (String.concat
                        "\n"
                        [
                            "module M ="
                            "    let secret () = 42"
                            "    [<Struct>]"
                            "    type Holder(seed: int) ="
                            "        member h.Compute() = secret () + seed"
                            "let r = Holder(0)"
                            "printfn \"%d\" (r.Compute())"
                        ])
            }

            // The exact SetIterator shape: a `val`-field struct whose *secondary
            // ctor field-init block* calls a module sibling (`stack = collapseLHS
            // [s]`). The member-body scope alone isn't enough — the ctor scope must
            // also see the enclosing module's bindings.
            test "G16: a nested struct's secondary-ctor field-init calls a module sibling (SetIterator shape)" {
                runsLines
                    [ "10" ]
                    (String.concat
                        "\n"
                        [
                            "module M ="
                            "    let seed (x: int) = x + x"
                            "    [<Struct>]"
                            "    type Box ="
                            "        val mutable N: int"
                            "        new(x: int) = { N = seed x }"
                            "        member this.Get() = this.N"
                            "let b = Box(5)"
                            "printfn \"%d\" (b.Get())"
                        ])
            }
        ]
