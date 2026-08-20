module XParsec.FSharp.Codegen.Clr.Tests.StructuralEqualityTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// A monomorphic user DU gets `Equals(object)` / `GetHashCode()` overrides walking
// each case's fields through `EqualityComparer<F>.Default` / `System.HashCode`.
// Emission does not depend on a `=` use site, so these invoke the members directly.

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

    let typedEqualsMethod (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

    let eqTyped (ty: Type) (a: obj) (b: obj) : bool =
        (typedEqualsMethod ty).Invoke(a, [| b |]) :?> bool

    let eq (ty: Type) (a: obj) (b: obj) : bool =
        (equalsMethod ty).Invoke(a, [| b |]) :?> bool

    let hash (ty: Type) (a: obj) : int = (hashMethod ty).Invoke(a, [||]) :?> int

    // Every field shape the walk must cover: two single-`int` cases (so a tag
    // mismatch with an equal payload is the only difference), a two-field case,
    // and a nullary case.
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
                let artifact = compileSource "EqMeta" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"
                Expect.isNotNull ty "the assembly contains the union type Shape"

                Expect.isNotNull (equalsMethod ty) "Shape declares its own Equals(object) override"
                Expect.isNotNull (hashMethod ty) "Shape declares its own GetHashCode() override"
                Expect.isTrue (equalsMethod ty).IsVirtual "Equals is virtual (overrides Object.Equals)"
                Expect.isTrue (hashMethod ty).IsVirtual "GetHashCode is virtual (overrides Object.GetHashCode)"
            }

            test "DU equality does not pin an FSharp.Core dependency" {
                let artifact = compileSource "EqNoDep" shapeSrc

                expectNoFSharpCore artifact "generated Equals/GetHashCode reference only the BCL"
            }

            test "nullary cases are equal once their tags match; unequal across cases" {
                let artifact = compileSource "EqNullary" shapeSrc
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
                let artifact = compileSource "EqPayload" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let c3a = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c3b = (factory ty "Circle").Invoke(null, [| box 3 |])
                let c5 = (factory ty "Circle").Invoke(null, [| box 5 |])

                Expect.isTrue (eq ty c3a c3b) "Circle 3 = Circle 3"
                Expect.isFalse (eq ty c3a c5) "Circle 3 <> Circle 5"
            }

            test "two cases with the same payload are distinguished by tag" {
                let artifact = compileSource "EqTag" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let circle3 = (factory ty "Circle").Invoke(null, [| box 3 |])
                let square3 = (factory ty "Square").Invoke(null, [| box 3 |])

                Expect.isFalse (eq ty circle3 square3) "Circle 3 <> Square 3 (different case, same int payload)"
            }

            test "a multi-field case compares every field" {
                let artifact = compileSource "EqMultiField" shapeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                let b23a = (factory ty "Box").Invoke(null, [| box 2; box 3 |])
                let b23b = (factory ty "Box").Invoke(null, [| box 2; box 3 |])
                let b24 = (factory ty "Box").Invoke(null, [| box 2; box 4 |])

                Expect.isTrue (eq ty b23a b23b) "Box(2,3) = Box(2,3)"
                Expect.isFalse (eq ty b23a b24) "Box(2,3) <> Box(2,4)"
            }

            test "equal values hash equal; distinct cases / payloads hash apart" {
                let artifact = compileSource "EqHash" shapeSrc
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
                let artifact = compileSource "EqNested" treeSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tree"

                let leaf n =
                    (factory ty "Leaf").Invoke(null, [| box (n: int) |])

                let branch l r =
                    (factory ty "Branch").Invoke(null, [| l; r |])

                // `Equals(object)` reuses `Object`'s slot, so a boxed `.Equals(obj)`
                // routes to it; the `EqualityComparer<Tree>.Default` recursion goes
                // through the typed `IEquatable<Tree>::Equals` instead.
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
                let artifact = compileSource "EqIEquatable" shapeSrc
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
                let artifact = compileSource "EqTyped" shapeSrc
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
                let artifact = compileSource "EqComparer" shapeSrc
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
                    "comparer.Equals(Circle 3, Circle 3) is true on the typed path"

                Expect.isFalse
                    (comparerEquals.Invoke(defaultComparer, [| c3a; c5 |]) :?> bool)
                    "comparer.Equals(Circle 3, Circle 5) is false on the typed path"
            }
        ]

// A generic user DU emits the same triple written in its own `!0`: field/tag
// access through `MemberRef`s on the type's `TypeSpec`, `EqualityComparer<!0>` /
// `HashCode.Add<!0>` for a typar-typed field, `IEquatable<Box<!0>>` as the interface.
[<Tests>]
let genericTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let factory (ty: Type) (name: string) =
        ty.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static)

    let equalsObjMethod (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

    let typedEqualsMethod (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

    let hashMethod (ty: Type) =
        ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

    let eq (ty: Type) (a: obj) (b: obj) : bool =
        (equalsObjMethod ty).Invoke(a, [| b |]) :?> bool

    let eqTyped (ty: Type) (a: obj) (b: obj) : bool =
        (typedEqualsMethod ty).Invoke(a, [| b |]) :?> bool

    let hash (ty: Type) (a: obj) : int = (hashMethod ty).Invoke(a, [||]) :?> int

    // A single-field generic DU whose field *is* the declaring typar `'T`, so the
    // generated triple compares and hashes it through `EqualityComparer<!0>`.
    let boxSrc = String.concat "\n" [ "type Box<'T> ="; "    | Box of 'T" ]

    // A self-recursive generic DU (the canonical cons-list): the `Cons` tail field
    // is `Lst<'T>`, so the structural recursion runs through `EqualityComparer<Lst<!0>>`.
    let lstSrc =
        String.concat "\n" [ "type Lst<'T> ="; "    | Nil"; "    | Cons of 'T * Lst<'T>" ]

    testList
        "GenericStructuralEquality"
        [
            test "a generic DU emits the equality triple + IEquatable<Self> on its `1 type" {
                let artifact = compileSource "GenEqMeta" boxSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                Expect.isNotNull boxTy "the assembly contains the generic union Box`1"
                Expect.isTrue boxTy.IsGenericTypeDefinition "Box`1 is a generic type definition"

                let boxInt = boxTy.MakeGenericType typeof<int>
                Expect.isNotNull (equalsObjMethod boxInt) "Box<int> declares its own Equals(object) override"
                Expect.isNotNull (hashMethod boxInt) "Box<int> declares its own GetHashCode() override"
                Expect.isNotNull (typedEqualsMethod boxInt) "Box<int> declares a typed Equals(Box<int>)"

                Expect.isTrue (equalsObjMethod boxInt).IsVirtual "Equals(object) is virtual"
                Expect.isTrue (hashMethod boxInt).IsVirtual "GetHashCode() is virtual"

                // The override reuses Object's slot (no new vtable slot), so a boxed
                // `.Equals(obj)` and the comparer's nested-DU path both find it.
                Expect.equal
                    ((equalsObjMethod boxInt).GetBaseDefinition().DeclaringType)
                    typeof<obj>
                    "Equals(object) overrides Object.Equals (reuses its slot)"

                let iface = typedefof<IEquatable<_>>.MakeGenericType boxInt
                Expect.isTrue (iface.IsAssignableFrom boxInt) "Box<int> implements IEquatable<Box<int>>"
            }

            test "a typar-typed field (`'T`) compares via EqualityComparer<!0> at int and string instantiations" {
                let artifact = compileSource "GenEqField" boxSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"

                let mk (t: Type) (v: obj) =
                    (factory (boxTy.MakeGenericType t) "Box").Invoke(null, [| v |])

                let boxInt = boxTy.MakeGenericType typeof<int>
                let bi3a = mk typeof<int> (box 3)
                let bi3b = mk typeof<int> (box 3)
                let bi5 = mk typeof<int> (box 5)

                Expect.isTrue (eqTyped boxInt bi3a bi3b) "Box 3 = Box 3 (int field via EqualityComparer<!0>)"
                Expect.isFalse (eqTyped boxInt bi3a bi5) "Box 3 <> Box 5 (int field)"
                Expect.equal (hash boxInt bi3a) (hash boxInt bi3b) "equal Box<int> hash equal"

                // The *same* generated members, at a different instantiation: the `!0`
                // field encoding works for any element type, not just int.
                let boxStr = boxTy.MakeGenericType typeof<string>
                let bsa = mk typeof<string> (box "hi")
                let bsb = mk typeof<string> (box "hi")
                let bsc = mk typeof<string> (box "yo")

                Expect.isTrue (eqTyped boxStr bsa bsb) "Box \"hi\" = Box \"hi\" (string field via EqualityComparer<!0>)"
                Expect.isFalse (eqTyped boxStr bsa bsc) "Box \"hi\" <> Box \"yo\" (string field)"
                Expect.equal (hash boxStr bsa) (hash boxStr bsb) "equal Box<string> hash equal"
            }

            test "Equals(object) rejects null and a different instantiation (isinst on the type's own TypeSpec)" {
                let artifact = compileSource "GenEqIsinst" boxSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"

                let mk (t: Type) (v: obj) =
                    (factory (boxTy.MakeGenericType t) "Box").Invoke(null, [| v |])

                let boxInt = boxTy.MakeGenericType typeof<int>
                let bi3 = mk typeof<int> (box 3)
                let bs3 = mk typeof<string> (box "3")

                Expect.isFalse (eq boxInt bi3 null) "Box<int> 3 <> null"
                Expect.isFalse (eq boxInt bi3 bs3) "Box<int> 3 <> Box<string> \"3\" (isinst Box<int> fails)"
            }

            test "a self-recursive generic DU compares + hashes structurally through EqualityComparer<Lst<!0>>" {
                let artifact = compileSource "GenEqLst" lstSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let lstTy = asm.GetType "Lst`1"
                let lstInt = lstTy.MakeGenericType typeof<int>

                let nil = (factory lstInt "Nil").Invoke(null, [||])

                let cons (h: int) (t: obj) =
                    (factory lstInt "Cons").Invoke(null, [| box h; t |])

                let a = cons 1 (cons 2 (cons 3 nil))
                let b = cons 1 (cons 2 (cons 3 nil))
                let c = cons 1 (cons 2 (cons 9 nil))

                Expect.isTrue
                    (eqTyped lstInt a b)
                    "deep-equal lists are equal (tail recurses via EqualityComparer<Lst<!0>>)"

                Expect.isFalse (eqTyped lstInt a c) "lists differing in a deep element are unequal"
                Expect.isFalse (eqTyped lstInt a nil) "Cons(…) <> Nil (tag distinguishes)"
                Expect.equal (hash lstInt a) (hash lstInt b) "deep-equal lists hash equal"
            }

            test "EqualityComparer<Box<int>>.Default selects the IEquatable-based comparer" {
                let artifact = compileSource "GenEqComparer" boxSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let boxTy = asm.GetType "Box`1"
                let boxInt = boxTy.MakeGenericType typeof<int>

                let comparerTy =
                    typedefof<System.Collections.Generic.EqualityComparer<_>>.MakeGenericType boxInt

                let defaultComparer = comparerTy.GetProperty("Default").GetValue null

                Expect.stringContains
                    (defaultComparer.GetType().Name)
                    "GenericEqualityComparer"
                    "EqualityComparer<Box<int>>.Default is the IEquatable-based GenericEqualityComparer"
            }

            test "generic DU equality does not pin an FSharp.Core dependency" {
                let artifact = compileSource "GenEqNoDep" lstSrc

                expectNoFSharpCore artifact "generated generic triple references only the BCL"
            }

            test "a `=` use site on a generic-DU instantiation reaches the structural triple via the comparer" {
                // `Box<int>` is ground, so `=` routes to
                // `EqualityComparer<Box<int>>.Default`. The union implements
                // `IEquatable<Box<int>>`, so two distinct-but-equal `Box 1` compare true.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box<'T> ="
                            "    | Box of 'T"
                            "let x = Box 1"
                            "let y = Box 1"
                            "printfn \"%d\" (if x = y then 1 else 0)"
                            "printfn \"%d\" (if x <> y then 1 else 0)"
                        ]

                let artifact = compileSource "GenEqUseSite" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n0"
                    "distinct-but-equal generic-DU pair compares structurally (true), not by reference"
            }
        ]
