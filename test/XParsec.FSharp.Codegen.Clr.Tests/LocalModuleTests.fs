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

// ---- A module-held type is a NESTED class -----------------------------------
//
// The type's key says a module holds it (`TypeHolder.InModule`), the renderer spells that
// `N.MModule+T`, and the emitter writes exactly that: a `TypeDef` with an empty namespace
// column and a `NestedClass` row into the module's compiled holder class. These pin the
// two faces MEETING — the name the key renders is the name the runtime binds.

/// `namespace N` + `module M` (whose name collides with `type M`, so its holder takes the
/// `Module` suffix) + a `type T` the module holds. `M` holds ONLY types — no `let` — so
/// its holder class exists solely because a type needs it.
let private moduleHeldType =
    String.concat
        "\n"
        [
            "namespace N"
            ""
            "type M = { tag: int }"
            ""
            "module M ="
            "    type T = { x: int }"
        ]

[<Tests>]
let nestedEmission =
    testList
        "LocalModule nested emission"
        [
            test "a module-held type binds by its nested metadata name" {
                let artifact = compileSourceTo (ProjectInfo.library "ModuleHeldType") moduleHeldType
                let bytes = Codegen.toBytes artifact
                MetadataStructure.assertWellFormed "ModuleHeldType" bytes

                let asm = loadAssembly bytes

                // The name `SymbolKeyOps.typeMetaName` renders for this type's key. That
                // the RUNTIME binds it is the whole point of the nesting: the flat `N.T`
                // and the dotted `N.M.T` both bind nothing.
                Expect.isNotNull (asm.GetType "N.MModule+T") "expected N.MModule+T to bind"
                Expect.isNull (asm.GetType "N.T") "expected no flat N.T"
            }

            // A module with no `let` at all still gets its holder class — holder discovery
            // reads the emitted TYPES' holder chains, not just the bindings'.
            test "a module holding only types still gets its holder class" {
                let artifact = compileSourceTo (ProjectInfo.library "TypeOnlyModule") moduleHeldType
                let bytes = Codegen.toBytes artifact

                let ts = MetadataStructure.emittedTypes bytes |> List.map (fun t -> t.Name)

                Expect.contains ts "N.MModule" "expected the type-only module's holder class"
                Expect.contains ts "N.MModule+T" "expected the held type nested in it"
            }

            // A nested module's holder is itself nested — in its PARENT's holder. The
            // parent must therefore be emitted even when it holds nothing of its own,
            // which is the ancestor half of holder discovery.
            test "a nested module's holder nests in its parent's, ancestors included" {
                let src =
                    String.concat
                        "\n"
                        [
                            "namespace N"
                            ""
                            "module Outer ="
                            "    module Inner ="
                            "        type T = { x: int }"
                            "        let twice (n: int) = n + n"
                        ]

                let artifact = compileSourceTo (ProjectInfo.library "NestedModuleHolder") src
                let bytes = Codegen.toBytes artifact
                MetadataStructure.assertWellFormed "NestedModuleHolder" bytes

                MetadataStructure.assertTypeMembers
                    "NestedModuleHolder"
                    bytes
                    [
                        // `Outer` holds nothing directly; it exists so `Inner` has an
                        // enclosing type.
                        {
                            Type = "N.Outer"
                            Fields = []
                            Methods = []
                        }
                        {
                            Type = "N.Outer+Inner"
                            Fields = []
                            Methods = [ "twice" ]
                        }
                        {
                            Type = "N.Outer+Inner+T"
                            Fields = [ "x" ]
                            Methods = [ ".ctor"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                    ]

                let asm = loadAssembly bytes
                Expect.isNotNull (asm.GetType "N.Outer+Inner+T") "expected N.Outer+Inner+T to bind"
            }
        ]

// ---- A module is not yet part of a type's CLAIM (pinned defect) --------------
//
// The type's KEY names its module — `stampLocalTypeKey` mints `TypeKey(Holder = InModule
// M)` for `namespace N` + `module M` + `type T` (`SymbolKeyTests`, "SymbolKey local type
// containment") — and the emitter now honours it: `T` is a TypeDef nested in `M`'s
// compiled holder class, with a `NestedClass` row (asserted below and in
// `MetadataStructureTests`).
//
// What has NOT moved is the `(name, arity)` CLAIM: `TypeRegistry.TypeClaims` is module-
// AND namespace-blind, so two sibling modules declaring `type T` still contest ONE claim.
//
// CORRECT behaviour (what F# does, and what these tests must be flipped to assert once
// the CLAIM is holder-aware): the program below is LEGAL. `N.A.T` and `N.B.T` are two
// distinct types and must both compile, each emitting its own TypeDef nested in its
// module's holder class. Nested emission is the PRECONDITION for that flip — two claims
// need two distinguishable metadata names — and it has landed; the claim itself has not.
//
// CURRENT behaviour, pinned below: the front end rejects the second as `Duplicate type
// definition: T`, the second type is never registered, and the PE carries only `A`'s.
// Where a *body* then uses the dropped type, the failure is worse than a diagnostic —
// codegen crashes outright.
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
                test "only A's T is emitted, nested in A's holder — the sibling module's type is dropped" {
                    let _, artifact = compileSource "SiblingModuleTypeIdentity" recordPair
                    let bytes = Codegen.toBytes artifact

                    let ts =
                        MetadataStructure.emittedTypes bytes
                        |> List.map (fun t -> t.Name)
                        |> List.filter (fun n -> n.EndsWith "+T")

                    // Correct: BOTH `N.A+T` and `N.B+T`. Current: only `A`'s — the claim is
                    // module-blind, so `B`'s `T` is never registered and never emitted. The
                    // *nesting* is right either way, which is what makes the flip possible.
                    Expect.equal ts [ "N.A+T" ] "expected A's T nested in A's holder, and B's dropped"

                    MetadataStructure.assertWellFormed "SiblingModuleTypeIdentity" bytes
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
