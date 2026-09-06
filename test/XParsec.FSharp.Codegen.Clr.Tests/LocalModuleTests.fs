module XParsec.FSharp.Codegen.Clr.Tests.LocalModuleTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.ReflectionHarness

/// The source analyses with no errors. Acceptance alone cannot tell two same-named types
/// apart, so every caller pairs this with an assertion on the resolved identity.
let private compiles (src: string) : unit =
    let errs =
        [
            for d in (analyse src).Diagnostics do
                if Diagnostic.isError d then
                    yield d.Message
        ]

    Expect.isEmpty errs (sprintf "expected no errors; got %A" errs)

// Resolving a `let` binding of an in-file module: qualified from a sibling module's body
// (`A.f`), and unqualified from a type nested inside the module that holds it. The
// provider knows only dependency packages, and a module name is not a value binding.

[<Tests>]
let tests =
    testList
        "LocalModule"
        [
            test "a module body calls a sibling module's let-bound function (A.f)" {
                runsLines
                    [ "11" ]
                    (lines
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
                    (lines
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

            test "a struct nested in a module calls a let-bound module sibling unqualified" {
                runsLines
                    [ "42" ]
                    (lines
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

            // The call sits in a `new(x) = { … }` field-init block, not a member body, so
            // the ctor scope has to see the enclosing module's bindings as well.
            test "a nested struct's secondary-ctor field-init calls a module sibling (SetIterator shape)" {
                runsLines
                    [ "10" ]
                    (lines
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

// A module-held type is emitted as a nested class: an empty namespace column plus a
// `NestedClass` row into the module's class, spelled `N.MModule+T`.

/// `module M` collides with `type M`, so the module class takes the `Module` suffix; and
/// `M` holds only a type, no `let`, so its class exists solely to hold that type.
let private moduleHeldType =
    lines
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

                // Neither the flat `N.T` nor the dotted `N.M.T` binds anything.
                Expect.isNotNull (asm.GetType "N.MModule+T") "expected N.MModule+T to bind"
                Expect.isNull (asm.GetType "N.T") "expected no flat N.T"
            }

            // Module-class discovery reads the emitted TYPES' containment chains, so a
            // module with no `let` at all still gets a class.
            test "a module holding only types still gets its module class" {
                let artifact = compileSourceTo (ProjectInfo.library "TypeOnlyModule") moduleHeldType

                let bytes = Codegen.toBytes artifact

                let ts = MetadataStructure.emittedTypes bytes |> List.map (fun t -> t.Name)

                Expect.contains ts "N.MModule" "expected the type-only module's module class"
                Expect.contains ts "N.MModule+T" "expected the held type nested in it"
            }

            // A nested module's class nests in its PARENT's, so the parent must be emitted
            // even when it holds nothing of its own.
            test "a nested module's module class nests in its parent's, ancestors included" {
                let src =
                    lines
                        [
                            "namespace N"
                            ""
                            "module Outer ="
                            "    module Inner ="
                            "        type T = { x: int }"
                            "        let twice (n: int) = n + n"
                        ]

                let artifact = compileSourceTo (ProjectInfo.library "NestedModuleClass") src
                let bytes = Codegen.toBytes artifact
                MetadataStructure.assertWellFormed "NestedModuleClass" bytes

                MetadataStructure.assertTypeMembers
                    "NestedModuleClass"
                    bytes
                    [
                        // `Outer` holds nothing directly; it exists so `Inner` can nest.
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
                            Fields = [ "x@" ]
                            Methods = [ ".ctor"; "get_x"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                    ]

                let asm = loadAssembly bytes
                Expect.isNotNull (asm.GetType "N.Outer+Inner+T") "expected N.Outer+Inner+T to bind"
            }
        ]

// A type claims `(container, name, arity)`, so `N.A.T` and `N.B.T` are two types rather
// than one name contested twice, and the nesting is what gives them distinguishable
// metadata names. Collapsing them onto one type would compile clean and emit ONE `T`.

/// `namespace N` over two sibling modules `A` and `B`, each declaring its own `type T`;
/// `inA` / `inB` are the module bodies, indented in.
let private siblingModuleTypes (inA: string list) (inB: string list) : string =
    let body (m: string) (members: string list) =
        (sprintf "module %s =" m) :: (members |> List.map (fun l -> "    " + l))

    lines ([ "namespace N"; "" ] @ body "A" inA @ [ "" ] @ body "B" inB)

/// `N.A.T = { x: int }` and `N.B.T = { y: int }`: same name, disjoint field sets, so a
/// conflation cannot hide.
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
                test "each sibling module's T is emitted, nested in ITS OWN module class" {
                    let artifact = compileSource "SiblingModuleTypeIdentity" recordPair
                    let bytes = Codegen.toBytes artifact

                    let ts =
                        MetadataStructure.emittedTypes bytes
                        |> List.map (fun t -> t.Name)
                        |> List.filter (fun n -> n.EndsWith "+T")
                        |> List.sort

                    Expect.equal ts [ "N.A+T"; "N.B+T" ] "expected two nested Ts, one per module class"

                    MetadataStructure.assertWellFormed "SiblingModuleTypeIdentity" bytes

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

                    Expect.equal (fields ta) [ "x@" ] "A's T keeps its own field"
                    Expect.equal (fields tb) [ "y@" ] "B's T keeps its own field"
                }

            // Acceptance IS the identity assertion here: were `B`'s `T` bound to `A`'s,
            // `{ y = 2 }` would set a field that type does not have.
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

                    let artifact = compileSource "SiblingModuleClassIdentity" src
                    let bytes = Codegen.toBytes artifact

                    let asm = loadAssembly bytes
                    Expect.isNotNull (asm.GetType "N.A+T") "expected N.A+T to bind"
                    Expect.isNotNull (asm.GetType "N.B+T") "expected N.B+T to bind"
                }

            // From outside, a held type is reached by naming its module. `B.T` takes a
            // `string` where `A.T` takes an `int`, so a conflation cannot type-check.
            yield
                test "a body outside A constructs A.T by its qualified name" {
                    let src =
                        lines (
                            [
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
                                // Both the annotation and the construction name A's T.
                                "    let make (n: int) : A.T = A.T(n)"
                                "    let read (v: A.T) = v.N"
                            ]
                        )

                    let artifact = compileSource "QualifiedModuleTypeCtor" src
                    let bytes = Codegen.toBytes artifact
                    MetadataStructure.assertWellFormed "QualifiedModuleTypeCtor" bytes

                    let asm = loadAssembly bytes
                    let ta = asm.GetType "N.A+T"
                    Expect.isNotNull ta "expected N.A+T to bind"

                    let make = (asm.GetType "N.C").GetMethod "make"
                    Expect.equal make.ReturnType ta "make returns N.A+T"

                    let v = make.Invoke(null, [| box 7 |])
                    Expect.equal (v.GetType()) ta "the constructed value IS N.A+T"

                    // A property emits as `get_N`; reading back the ctor argument shows A's
                    // ctor ran, not B's.
                    Expect.equal ((ta.GetMethod "get_N").Invoke(v, [||]) :?> int) 7 "A.T(7).N = 7"
                }
        ]
