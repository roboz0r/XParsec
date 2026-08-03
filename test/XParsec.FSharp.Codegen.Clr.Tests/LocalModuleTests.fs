module XParsec.FSharp.Codegen.Clr.Tests.LocalModuleTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// The source analyses with no errors. Every use of it pairs this with an assertion on the
/// resolved IDENTITY (the emitted metadata, or a disjoint field set only one candidate type
/// carries) — acceptance alone would let a conflation through.
let private compiles (src: string) : unit =
    let errs =
        [
            for d in (analyse src).Diagnostics do
                if Diagnostic.isError d then
                    yield d.Message
        ]

    Expect.isEmpty errs (sprintf "expected no errors; got %A" errs)

// Local (in-file) module resolution, in two shapes:
//
//   * a `let`-bound value/function of a sibling *local* module resolves *qualified*
//     (`A.f`), from another module's body and from a class member. The module tree is
//     flattened before name resolution, so without the `LocalModules` pre-pass the sibling
//     is unresolvable (the provider only knows dependency packages, and a module name is
//     not a value binding).
//   * an *unqualified* reference from a type nested *inside* a module up to one of that
//     module's `let`-bound siblings (`SetIterator` → `collapseLHS` in `set.clr.fs`) resolves —
//     the enclosing module's bindings enter the nested type's member-body scope.
//
// Both ride the `runs` driver (compile → run → assert stdout), so resolution is proven end
// to end: name resolution records the use site as an ordinary local `Binding`, Unification
// types it via the member's generalised scheme, and Elaborate lowers it to the same
// `TExpr.Var` a bare local reference produces.

[<Tests>]
let tests =
    testList
        "LocalModule"
        [
            // ---- qualified sibling-module resolution ---------------------
            test "a module body calls a sibling module's let-bound function (A.f)" {
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

            test "a class member body calls a local module's function (mirrors Set→SetTree)" {
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

            // ---- unqualified enclosing-module reference from a nested type
            test "a struct nested in a module calls a let-bound module sibling unqualified" {
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
                            "open M"
                            "let r = Holder(0)"
                            "printfn \"%d\" (r.Compute())"
                        ])
            }

            // The exact SetIterator shape: a `val`-field struct whose *secondary
            // ctor field-init block* calls a module sibling (`stack = collapseLHS
            // [s]`). The member-body scope alone isn't enough — the ctor scope must
            // also see the enclosing module's bindings.
            test "a nested struct's secondary-ctor field-init calls a module sibling (SetIterator shape)" {
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
                            "open M"
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
// key and emitter MEETING — the name the key renders is the name the runtime binds.

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

// ---- The module is part of a type's CLAIM ------------------------------------
//
// A type claims `(holder, name, arity)`, so `N.A.T` and `N.B.T` are two types — not one
// name contested twice. Each is a `TypeDef` nested in its own module's compiled holder
// class, which is what gives two same-named types two distinguishable metadata names, and
// each body constructs ITS OWN.
//
// Asserted on the emitted metadata and through the loaded PE, not on acceptance alone: two
// claims silently collapsed onto one type would compile clean and emit ONE nested `T`.

/// `namespace N` holding two sibling modules `A` and `B`, each declaring its OWN
/// `type T` (`inA` / `inB` are the module bodies, indented in). Two distinct types under
/// one short name.
let private siblingModuleTypes (inA: string list) (inB: string list) : string =
    let body (m: string) (lines: string list) =
        (sprintf "module %s =" m) :: (lines |> List.map (fun l -> "    " + l))

    String.concat "\n" ([ "namespace N"; "" ] @ body "A" inA @ [ "" ] @ body "B" inB)

/// The record pair: `N.A.T = { x: int }` and `N.B.T = { y: int }` — same name,
/// incompatible field sets, so a conflation cannot hide.
let private recordA = [ "type T = { x: int }" ]
let private recordB = [ "type T = { y: int }" ]
let private recordPair = siblingModuleTypes recordA recordB

[<Tests>]
let moduleIsPartOfTypeIdentity =
    testList
        "LocalModule type identity"
        [
            for kind, inA, inB in
                [
                    "record", recordA, recordB
                    "union", [ "type T ="; "    | Ca of int" ], [ "type T ="; "    | Cb of string" ]
                ] -> test $"sibling modules may each declare a {kind} named T" { compiles (siblingModuleTypes inA inB) }

            yield
                test "each sibling module's T is emitted, nested in ITS OWN holder" {
                    let _, artifact = compileSource "SiblingModuleTypeIdentity" recordPair
                    let bytes = Codegen.toBytes artifact

                    let ts =
                        MetadataStructure.emittedTypes bytes
                        |> List.map (fun t -> t.Name)
                        |> List.filter (fun n -> n.EndsWith "+T")
                        |> List.sort

                    Expect.equal ts [ "N.A+T"; "N.B+T" ] "expected two nested Ts, one per module holder"

                    MetadataStructure.assertWellFormed "SiblingModuleTypeIdentity" bytes

                    // They are two TYPES, not one reused: the loaded PE binds both, and each
                    // carries only its own field.
                    let asm = loadAssembly bytes
                    let ta = asm.GetType "N.A+T"
                    let tb = asm.GetType "N.B+T"
                    Expect.isNotNull ta "expected N.A+T to bind"
                    Expect.isNotNull tb "expected N.B+T to bind"
                    Expect.notEqual ta tb "the sibling Ts are distinct runtime types"

                    let fields (t: System.Type) =
                        t.GetFields(
                            System.Reflection.BindingFlags.Instance
                            ||| System.Reflection.BindingFlags.Public
                            ||| System.Reflection.BindingFlags.NonPublic
                        )
                        |> Array.map (fun f -> f.Name)
                        |> Array.toList

                    Expect.equal (fields ta) [ "x" ] "A's T keeps its own field"
                    Expect.equal (fields tb) [ "y" ] "B's T keeps its own field"
                }

            // Each module's body resolves `T` to the `T` ITS OWN module declares. The field
            // sets are DISJOINT, so acceptance IS the identity assertion: were `B`'s `T`
            // bound to `A`'s, `{ y = 2 }` would name no field of it.
            yield
                test "each sibling module's body constructs its own T" {
                    compiles (
                        siblingModuleTypes
                            [ "type T = { x: int }"; "let mk () : T = { x = 1 }"; "let get () = (mk ()).x" ]
                            [ "type T = { y: int }"; "let mk () : T = { y = 2 }"; "let get () = (mk ()).y" ]
                    )
                }

            yield
                test "same-named classes in sibling modules are two classes" {
                    let src =
                        siblingModuleTypes
                            [ "type T(n: int) ="; "    member _.N = n" ]
                            [ "type T(s: string) ="; "    member _.S = s" ]

                    let _, artifact = compileSource "SiblingModuleClassIdentity" src
                    let bytes = Codegen.toBytes artifact

                    let asm = loadAssembly bytes
                    Expect.isNotNull (asm.GetType "N.A+T") "expected N.A+T to bind"
                    Expect.isNotNull (asm.GetType "N.B+T") "expected N.B+T to bind"
                }

            // A type is reached from OUTSIDE the module holding it by naming that module.
            // `B` declares its own `T` with a different field, so a conflation would be caught
            // twice over: the construction would take the wrong ctor argument, and the emitted
            // member would hang off the wrong nested type.
            yield
                test "a body outside A constructs A.T by its qualified name" {
                    let src =
                        String.concat
                            "\n"
                            ([
                                "namespace N"
                                ""
                                "module A ="
                                "    type T(n: int) ="
                                "        member _.N = n"
                                ""
                                "module B ="
                                "    type T(s: string) ="
                                "        member _.S = s"
                                ""
                                "module C ="
                                // Both the annotation and the construction name A's T through
                                // the module holding it.
                                "    let make (n: int) : A.T = A.T(n)"
                                "    let read (v: A.T) = v.N"
                            ])

                    let _, artifact = compileSource "QualifiedModuleTypeCtor" src
                    let bytes = Codegen.toBytes artifact
                    MetadataStructure.assertWellFormed "QualifiedModuleTypeCtor" bytes

                    let asm = loadAssembly bytes
                    let ta = asm.GetType "N.A+T"
                    Expect.isNotNull ta "expected N.A+T to bind"

                    // `make` returns A's T — and A's T is the one taking an int and carrying
                    // `N`, not B's `string`/`S`.
                    let make = (asm.GetType "N.C").GetMethod "make"
                    Expect.equal make.ReturnType ta "make returns N.A+T"

                    let v = make.Invoke(null, [| box 7 |])
                    Expect.equal (v.GetType()) ta "the constructed value IS N.A+T"

                    // A's own member (a property emits as `get_N`) reads back the ctor
                    // argument: the construction ran A's ctor, not B's.
                    Expect.equal ((ta.GetMethod "get_N").Invoke(v, [||]) :?> int) 7 "A.T(7).N = 7"
                }
        ]
