module XParsec.FSharp.Codegen.Clr.Tests.LocalModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// G15 / G16 — local (in-file) module resolution.
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
// Elaborate lowers it to the same `TExpr.Var` a bare local reference produces.

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

// ---- A module is not yet part of a type's CLAIM (pinned defect) --------------
//
// The type's KEY now names its module — `stampLocalTypeKey` mints
// `TypeKey(Holder = InModule M)` for `namespace N` + `module M` + `type T`
// (`SymbolKeyTests`, "SymbolKey local type containment"). What has NOT moved is the
// `(name, arity)` CLAIM: `TypeRegistry.TypeClaims` is module- AND namespace-blind, so two
// sibling modules declaring `type T` still contest ONE claim. Nor has the emitted
// metadata: the CLR backend writes `T` as a TOP-LEVEL TypeDef with Namespace `N` — the
// NestedClass table is never written.
//
// CORRECT behaviour (what F# does, and what these tests must be flipped to assert once
// the CLAIM is holder-aware and the emitter nests the TypeDef — one commit, since a
// holder-aware claim admits two `T`s that a flat emitter would collide): the program
// below is LEGAL. `N.A.T` and `N.B.T` are two distinct types and must both compile, each
// emitting its own TypeDef nested in its module's holder class.
//
// CURRENT behaviour, pinned below: the two are indistinguishable. The front end rejects
// the second as `Duplicate type definition: T`, the second type is never registered, and
// the PE carries a single `N.T`. Where a *body* then uses the dropped type, the failure
// is worse than a diagnostic — codegen crashes outright.
//
// There is no known-failing-test convention in this suite (`ptest` marks debug probes,
// `skiptest` marks unavailable-environment rows), so these assert the WRONG current
// behaviour rather than invent a bespoke skip: the fix flips them.

/// `namespace N` holding two sibling modules `A` and `B`, each declaring its OWN
/// `type T` (`inA` / `inB` are the module bodies, indented in). Two distinct types
/// under one short name — legal F#, and the shape that collapses to one identity here.
let private siblingModuleTypes (inA: string list) (inB: string list) : string =
    let body (m: string) (lines: string list) =
        (sprintf "module %s =" m) :: (lines |> List.map (fun l -> "    " + l))

    String.concat "\n" ([ "namespace N"; "" ] @ body "A" inA @ [ "" ] @ body "B" inB)

/// The record pair: `N.A.T = { x: int }` and `N.B.T = { y: int }` — same name,
/// incompatible field sets, so nothing can excuse conflating them.
let private recordA = [ "type T = { x: int }" ]
let private recordB = [ "type T = { y: int }" ]
let private recordPair = siblingModuleTypes recordA recordB

[<Tests>]
let moduleIsNotPartOfTypeIdentity =
    testList
        "LocalModule type identity"
        [
            for kind, inA, inB in
                [
                    "record", recordA, recordB
                    "union", [ "type T ="; "    | Ca of int" ], [ "type T ="; "    | Cb of string" ]
                ] ->
                test $"a sibling module's same-named {kind} is (wrongly) rejected as a duplicate" {
                    failsWith "Duplicate type definition: T" (siblingModuleTypes inA inB)
                }

            yield
                test "only ONE N.T TypeDef is emitted — the sibling module's type is dropped" {
                    let _, artifact = compileSource "SiblingModuleTypeIdentity" recordPair

                    let ts =
                        peTypeDefNames (Codegen.toBytes artifact) |> List.filter (fun n -> n = "N.T")

                    // Correct: TWO distinct type-defs (`N.A/T` and `N.B/T`, nested in their
                    // module holders). Current: one `N.T` — the module is not in the key, so
                    // `B`'s `T` is never registered and never emitted.
                    Expect.equal ts [ "N.T" ] "expected the single flattened N.T the module-blind key mints"
                }

            // The dropped registration is not merely cosmetic. `B`'s `{ y = 2 }` finds no
            // record type behind the name, so its type stays an unresolved typar and the
            // BACKEND faults on it — a compiler crash, not a reported error.
            yield
                test "a body constructing the dropped sibling type crashes the backend" {
                    let src =
                        siblingModuleTypes
                            [ "type T = { x: int }"; "let mk () = { x = 1 }" ]
                            [ "type T = { y: int }"; "let mk () = { y = 2 }" ]

                    Expect.throws
                        (fun () -> compileSource "SiblingModuleTypeCtor" src |> ignore)
                        "expected the emitter to fault on the unregistered sibling record"
                }

            // Classes take an even worse path: the collision surfaces inside codegen as a
            // duplicate-key insert, so the compiler throws before any diagnostic is reported.
            yield
                test "same-named classes in sibling modules crash codegen on a duplicate key" {
                    let src =
                        siblingModuleTypes
                            [ "type T(n: int) ="; "    member _.N = n" ]
                            [ "type T(s: string) ="; "    member _.S = s" ]

                    Expect.throws
                        (fun () -> compileSource "SiblingModuleClassIdentity" src |> ignore)
                        "expected the duplicate nominal key insert to throw"
                }
        ]
