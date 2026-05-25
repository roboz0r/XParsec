module XParsec.FSharp.Codegen.Clr.Tests.StructuralEqualityTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// C-Eq1 backend slice 2 (docs/core-operators-handoff.md): a monomorphic user DU
// gets real `Equals(object)` / `GetHashCode()` overrides walking each case's
// fields by the §3.2 rule (`EqualityComparer<F>.Default` / `System.HashCode`).
// Generation is independent of a `=` *use site* (which can't type a DU operand
// until `MockBuiltins` is demoted), so these reflect the emitted members and
// invoke them directly — `(Circle 3).Equals(Circle 3)` etc.

[<Tests>]
let tests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let factory (ty: Type) (name: string) =
        ty.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static)

    let equalsMethod (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

    let hashMethod (ty: Type) =
        ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

    // The typed `IEquatable<Self>::Equals(Self)` — selected by exact parameter type.
    let typedEqualsMethod (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

    let eqTyped (ty: Type) (a: obj) (b: obj) : bool =
        (typedEqualsMethod ty).Invoke(a, [| b |]) :?> bool

    let eq (ty: Type) (a: obj) (b: obj) : bool =
        (equalsMethod ty).Invoke(a, [| b |]) :?> bool

    let hash (ty: Type) (a: obj) : int = (hashMethod ty).Invoke(a, [||]) :?> int

    // A DU exercising every field shape the flat walk must cover: two distinct
    // single-`int` cases (so a tag mismatch with equal payload is the only
    // difference), a two-`int` case, and a nullary case.
    let shapeSrc =
        String.concat
            "\n"
            [
                "type Shape ="
                "    | Circle of int"
                "    | Square of int"
                "    | Box of int * int"
                "    | Dot"
            ]

    // A self-recursive DU, so a field of the union's own type drives the
    // `EqualityComparer<Tree>.Default` recursion into the nested override.
    let treeSrc =
        String.concat "\n" [ "type Tree ="; "    | Leaf of int"; "    | Branch of Tree * Tree" ]

    testList
        "StructuralEquality"
        [
            test "a monomorphic DU emits Equals(object) + GetHashCode() overrides on its own class" {
                let _, artifact = compileSource "EqMeta" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"
                Expect.isNotNull ty "the assembly contains the union type Shape"

                Expect.isNotNull (equalsMethod ty) "Shape declares its own Equals(object) override"
                Expect.isNotNull (hashMethod ty) "Shape declares its own GetHashCode() override"
                Expect.isTrue (equalsMethod ty).IsVirtual "Equals is virtual (overrides Object.Equals)"
                Expect.isTrue (hashMethod ty).IsVirtual "GetHashCode is virtual (overrides Object.GetHashCode)"
            }

            test "DU equality pins no FSharp.Core dependency (eq §4: no runtime library)" {
                let _, artifact = compileSource "EqNoDep" shapeSrc

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "generated Equals/GetHashCode reference only the BCL (%A)" artifact.FSharpCoreDependencies)
            }

            test "nullary cases are equal once their tags match; unequal across cases" {
                let _, artifact = compileSource "EqNullary" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let dot1 = (factory ty "Dot").Invoke(null, [||])
                let dot2 = (factory ty "Dot").Invoke(null, [||])
                let circle = (factory ty "Circle").Invoke(null, [| box 3 |])

                Expect.isTrue (eq ty dot1 dot2) "Dot = Dot"
                Expect.isFalse (eq ty dot1 circle) "Dot <> Circle 3"
                Expect.isFalse (eq ty dot1 null) "Dot <> null"
            }

            test "a payload case compares by its field; equal iff the fields are equal" {
                let _, artifact = compileSource "EqPayload" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let c3a = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c3b = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c5 = (factory ty "Circle").Invoke(null, [| box 5 |])

                Expect.isTrue (eq ty c3a c3b) "Circle 3 = Circle 3"
                Expect.isFalse (eq ty c3a c5) "Circle 3 <> Circle 5"
            }

            test "two cases with the same payload are distinguished by tag" {
                let _, artifact = compileSource "EqTag" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let circle3 = (factory ty "Circle").Invoke(null, [| box 3 |])
                let square3 = (factory ty "Square").Invoke(null, [| box 3 |])

                Expect.isFalse (eq ty circle3 square3) "Circle 3 <> Square 3 (different case, same int payload)"
            }

            test "a multi-field case compares every field" {
                let _, artifact = compileSource "EqMultiField" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let b23a = (factory ty "Box").Invoke(null, [| box 2; box 3 |])
                let b23b = (factory ty "Box").Invoke(null, [| box 2; box 3 |])
                let b24 = (factory ty "Box").Invoke(null, [| box 2; box 4 |])

                Expect.isTrue (eq ty b23a b23b) "Box(2,3) = Box(2,3)"
                Expect.isFalse (eq ty b23a b24) "Box(2,3) <> Box(2,4)"
            }

            test "equal values hash equal; distinct cases / payloads hash apart" {
                let _, artifact = compileSource "EqHash" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let c3a = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c3b = (factory ty "Circle").Invoke(null, [| box 3 |])
                let s3 = (factory ty "Square").Invoke(null, [| box 3 |])
                let dot = (factory ty "Dot").Invoke(null, [||])

                Expect.equal (hash ty c3a) (hash ty c3b) "equal values hash equal"
                Expect.notEqual (hash ty c3a) (hash ty s3) "Circle 3 and Square 3 hash apart (tag in the hash)"
                Expect.notEqual (hash ty c3a) (hash ty dot) "Circle 3 and Dot hash apart"
            }

            test "a field of the union's own type recurses into the nested override" {
                let _, artifact = compileSource "EqNested" treeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tree"

                let leaf n =
                    (factory ty "Leaf").Invoke(null, [| box (n: int) |])

                let branch l r =
                    (factory ty "Branch").Invoke(null, [| l; r |])

                // The `Equals(object)` override still reuses `Object`'s slot (so a
                // boxed `.Equals(obj)` call routes to it). The recursion through
                // `EqualityComparer<Tree>.Default` now goes via the typed
                // `IEquatable<Tree>::Equals` path (asserted in its own test below),
                // but both walks share the same rule.
                Expect.equal
                    ((equalsMethod ty).GetBaseDefinition().DeclaringType)
                    typeof<obj>
                    "Equals(object) overrides Object.Equals (reuses its slot), not a new virtual slot"

                Expect.isTrue ((leaf 1).Equals(leaf 1)) "Leaf 1 = Leaf 1 via the virtual Object.Equals slot"

                let t1 = branch (leaf 1) (leaf 2)
                let t2 = branch (leaf 1) (leaf 2)
                let t3 = branch (leaf 1) (leaf 9)

                Expect.isTrue (eq ty t1 t2) "Branch(Leaf 1, Leaf 2) = Branch(Leaf 1, Leaf 2)"
                Expect.isFalse (eq ty t1 t3) "Branch(Leaf 1, Leaf 2) <> Branch(Leaf 1, Leaf 9)"
                Expect.equal (hash ty t1) (hash ty t2) "structurally equal trees hash equal"
            }

            test "a monomorphic DU implements IEquatable<Self> with a typed Equals(Self)" {
                let _, artifact = compileSource "EqIEquatable" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let iface = typedefof<IEquatable<_>>.MakeGenericType ty
                Expect.isTrue (iface.IsAssignableFrom ty) "Shape implements IEquatable<Shape>"

                let typed = typedEqualsMethod ty
                Expect.isNotNull typed "Shape declares a typed Equals(Shape)"
                Expect.isTrue typed.IsVirtual "the typed Equals(Shape) is virtual (implements the interface slot)"
                Expect.isTrue typed.IsFinal "the typed Equals(Shape) is final (the union is sealed)"
            }

            test "the typed Equals(Self) compares structurally and rejects null" {
                let _, artifact = compileSource "EqTyped" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let c3a = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c3b = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c5 = (factory ty "Circle").Invoke(null, [| box 5 |])
                let square3 = (factory ty "Square").Invoke(null, [| box 3 |])

                Expect.isTrue (eqTyped ty c3a c3b) "Circle 3 = Circle 3 via typed Equals"
                Expect.isFalse (eqTyped ty c3a c5) "Circle 3 <> Circle 5 via typed Equals"
                Expect.isFalse (eqTyped ty c3a square3) "Circle 3 <> Square 3 via typed Equals (tag)"
                Expect.isFalse (eqTyped ty c3a null) "Circle 3 <> null via typed Equals"
            }

            test "EqualityComparer<Self>.Default selects the IEquatable-based comparer and walks correctly" {
                let _, artifact = compileSource "EqComparer" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let comparerTy =
                    typedefof<System.Collections.Generic.EqualityComparer<_>>.MakeGenericType ty

                let defaultComparer = comparerTy.GetProperty("Default").GetValue null

                // IEquatable<Self> makes the BCL pick `GenericEqualityComparer` (the
                // typed, boxing-free path) over `ObjectEqualityComparer`.
                Expect.stringContains
                    (defaultComparer.GetType().Name)
                    "GenericEqualityComparer"
                    "EqualityComparer<Shape>.Default is the IEquatable-based GenericEqualityComparer"

                let comparerEquals = comparerTy.GetMethod("Equals", [| ty; ty |])

                let c3a = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c3b = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c5 = (factory ty "Circle").Invoke(null, [| box 5 |])

                Expect.isTrue
                    (comparerEquals.Invoke(defaultComparer, [| c3a; c3b |]) :?> bool)
                    "comparer.Equals(Circle 3, Circle 3) — through the typed path"

                Expect.isFalse
                    (comparerEquals.Invoke(defaultComparer, [| c3a; c5 |]) :?> bool)
                    "comparer.Equals(Circle 3, Circle 5) — through the typed path"
            }
        ]
