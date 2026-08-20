module XParsec.FSharp.Codegen.Clr.Tests.StructuralComparisonTests

open System
open System.Collections.Generic
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// Ordering on records and unions, opt-in via `[<StructuralComparison>]`: the emit
// side ships `int CompareTo(Self)` + `int CompareTo(object)` with `IComparable<Self>`
// + `IComparable`, and un-annotated ordering use sites fail the constraint check.

[<Tests>]
let tests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let compareToObj (ty: Type) =
        ty.GetMethod("CompareTo", declaredInstance, null, [| typeof<obj> |], null)

    let typedCompareTo (ty: Type) =
        ty.GetMethod("CompareTo", declaredInstance, null, [| ty |], null)

    let implementsIComparable (ty: Type) =
        let generic = typedefof<IComparable<_>>.MakeGenericType ty
        generic.IsAssignableFrom ty && typeof<IComparable>.IsAssignableFrom ty

    /// The typed `CompareTo(Self)`, so the test sees the raw `int` the body returns.
    let compareTyped (ty: Type) (a: obj) (b: obj) : int =
        (typedCompareTo ty).Invoke(a, [| b |]) :?> int

    /// The `CompareTo(object)` boxing entry, including its `ArgumentException` branch.
    let compareObj (ty: Type) (a: obj) (b: obj) : int =
        (compareToObj ty).Invoke(a, [| b |]) :?> int

    let signOf (n: int) : int =
        if n < 0 then -1
        elif n > 0 then 1
        else 0

    testList
        "structural comparison"
        [
            test "record CompareTo compares fields in declaration order" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Pair = { X: int; Y: int }"
                            "let p = { X = 0; Y = 0 }"
                        ]

                let artifact = compileSource "StructCmpRecField" src

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"

                Expect.isNotNull (typedCompareTo ty) "CompareTo(Pair) emitted"
                Expect.isNotNull (compareToObj ty) "CompareTo(object) emitted"
                Expect.isTrue (implementsIComparable ty) "Pair declares IComparable<Pair> + IComparable"

                let mk x y =
                    Activator.CreateInstance(ty, [| box (x: int); box (y: int) |])

                let p12 = mk 1 2
                let p13 = mk 1 3
                let p11 = mk 1 1

                Expect.equal (signOf (compareTyped ty p12 p13)) -1 "{1;2} < {1;3}"
                Expect.equal (signOf (compareTyped ty p13 p12)) 1 "{1;3} > {1;2}"
                Expect.equal (signOf (compareTyped ty p12 p12)) 0 "{1;2} = {1;2}"
                Expect.equal (signOf (compareTyped ty p13 p11)) 1 "{1;3} > {1;1}"
            }

            test "record CompareTo is lexicographic (first differing field wins)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Pair = { X: int; Y: int }"
                            "let p = { X = 0; Y = 0 }"
                        ]

                let artifact = compileSource "StructCmpRecLex" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"

                let mk x y =
                    Activator.CreateInstance(ty, [| box (x: int); box (y: int) |])

                Expect.equal (signOf (compareTyped ty (mk 1 99) (mk 2 0))) -1 "X differs ⇒ Y ignored"

                Expect.equal (signOf (compareTyped ty (mk 2 0) (mk 1 99))) 1 "X differs (reverse) ⇒ Y ignored"
            }

            test "union CompareTo compares tags first, then payload fields" {
                // Cases take tags in source order, so Square = 0, Box = 1, Circle = 2.
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Shape ="
                            "    | Square of int"
                            "    | Box of int * int"
                            "    | Circle of int"
                            "let s = Square 0"
                        ]

                let artifact = compileSource "StructCmpUnionTag" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Shape"

                Expect.isNotNull (typedCompareTo ty) "CompareTo(Shape) emitted"
                Expect.isTrue (implementsIComparable ty) "Shape declares IComparable<Shape> + IComparable"

                // Factories: `static Shape Square(int)`, `Box(int,int)`, `Circle(int)`.
                let square (n: int) =
                    ty.GetMethod("Square", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [| box n |])

                let mkBox (a: int) (b: int) =
                    ty.GetMethod("Box", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [| box a; box b |])

                let circle (n: int) =
                    ty.GetMethod("Circle", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [| box n |])

                Expect.equal (signOf (compareTyped ty (square 99) (mkBox 0 0))) -1 "Square < Box regardless of payload"
                Expect.equal (signOf (compareTyped ty (mkBox 0 0) (circle 0))) -1 "Box < Circle"
                Expect.equal (signOf (compareTyped ty (circle 1) (square 99))) 1 "Circle > Square"

                Expect.equal (signOf (compareTyped ty (circle 3) (circle 5))) -1 "Circle 3 < Circle 5"
                Expect.equal (signOf (compareTyped ty (circle 5) (circle 3))) 1 "Circle 5 > Circle 3"
                Expect.equal (signOf (compareTyped ty (circle 3) (circle 3))) 0 "Circle 3 = Circle 3"
                Expect.equal (signOf (compareTyped ty (mkBox 1 2) (mkBox 1 3))) -1 "Box(1,2) < Box(1,3)"

                Expect.equal
                    (signOf (compareTyped ty (mkBox 1 9) (mkBox 2 0)))
                    -1
                    "Box(1,9) < Box(2,0), because comparison is lexicographic on the payload"
            }

            test "CompareTo(object) boxing entry routes through the typed CompareTo" {
                let src =
                    String.concat "\n" [ "[<StructuralComparison>]"; "type Holder = { N: int }"; "let h = { N = 0 }" ]

                let artifact = compileSource "StructCmpBoxedEntry" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"

                let mk n =
                    Activator.CreateInstance(ty, [| box (n: int) |])

                Expect.equal (signOf (compareObj ty (mk 1) (mk 2))) -1 "CompareTo(object) field-walks"

                // `Comparer<obj>.Default.Compare` is the BCL's path for boxed
                // comparison, and it reaches `IComparable` once the type declares it.
                let cmp = Comparer<obj>.Default
                Expect.equal (signOf (cmp.Compare(mk 3, mk 4))) -1 "Comparer<obj>.Default sees IComparable on Holder"
                Expect.equal (signOf (cmp.Compare(mk 4, mk 3))) 1 "Comparer<obj>.Default reverse"
            }

            test "CompareTo(object) throws ArgumentException on a mismatched type" {
                // A non-`Self` argument throws `ArgumentException`, which reflection
                // surfaces wrapped in a `TargetInvocationException`.
                let src =
                    String.concat "\n" [ "[<StructuralComparison>]"; "type Holder = { N: int }"; "let h = { N = 0 }" ]

                let artifact = compileSource "StructCmpBoxedThrow" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"

                let mk n =
                    Activator.CreateInstance(ty, [| box (n: int) |])

                let caught =
                    try
                        compareObj ty (mk 1) (box "not a Holder") |> ignore
                        None
                    with
                    | :? TargetInvocationException as ex -> Some ex.InnerException
                    | ex -> Some ex

                match caught with
                | None -> failtest "expected an exception on a non-Self obj, got none"
                | Some inner ->
                    Expect.isTrue (inner :? ArgumentException) (sprintf "expected ArgumentException, got %A" inner)
            }

            test "CompareTo(object) returns 1 for a null argument (null sorts first)" {
                // Null sorts first, so any value compares positive against it.
                let src =
                    String.concat "\n" [ "[<StructuralComparison>]"; "type Holder = { N: int }"; "let h = { N = 0 }" ]

                let artifact = compileSource "StructCmpNullArg" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"
                let h = Activator.CreateInstance(ty, [| box 7 |])

                Expect.equal (signOf (compareObj ty h null)) 1 "CompareTo(null) = 1"
                Expect.equal (signOf (compareTyped ty h null)) 1 "Typed CompareTo(null) = 1"
            }

            test "[<NoComparison>] on a record skips the pair AND has no IComparable" {
                let src =
                    String.concat "\n" [ "[<NoComparison>]"; "type Sealed = { X: int }"; "let s = { X = 0 }" ]

                let artifact = compileSource "StructCmpNoCmp" src

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Sealed"

                Expect.isNull (typedCompareTo ty) "no typed CompareTo on [<NoComparison>] record"
                Expect.isNull (compareToObj ty) "no CompareTo(object) on [<NoComparison>] record"
                Expect.isFalse (implementsIComparable ty) "Sealed does NOT declare IComparable<Sealed>"
            }

            test "default (no attribute) on a record skips the pair (opt-in)" {
                // `[<NoComparison>]` and no attribute are identical at the emit boundary.
                let src =
                    String.concat "\n" [ "type Pair = { X: int; Y: int }"; "let p = { X = 0; Y = 0 }" ]

                let artifact = compileSource "StructCmpDefault" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"

                Expect.isNull (typedCompareTo ty) "no typed CompareTo on un-annotated record"
                Expect.isNull (compareToObj ty) "no CompareTo(object) on un-annotated record"
                Expect.isFalse (implementsIComparable ty) "no IComparable on un-annotated record"
            }

            test "default on a union also skips the pair (opt-in)" {
                // Comparison is opt-in for unions too, unlike the equality verdict,
                // where a union defaults to `Structural`.
                let src =
                    String.concat "\n" [ "type Tag ="; "    | A"; "    | B of int"; "let t = A" ]

                let artifact = compileSource "StructCmpUnionDefault" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNull (typedCompareTo ty) "no typed CompareTo on un-annotated union"
                Expect.isNull (compareToObj ty) "no CompareTo(object) on un-annotated union"
                Expect.isFalse (implementsIComparable ty) "no IComparable on un-annotated union"
            }

            test "[<StructuralComparison>] on a union emits the pair" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Tag ="
                            "    | A"
                            "    | B of int"
                            "let t = A"
                        ]

                let artifact = compileSource "StructCmpUnionOptIn" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNotNull (typedCompareTo ty) "typed CompareTo emitted on opt-in union"
                Expect.isNotNull (compareToObj ty) "CompareTo(object) emitted on opt-in union"
                Expect.isTrue (implementsIComparable ty) "Tag declares IComparable<Tag> + IComparable"
            }

            test "the decoder accepts the `Attribute` suffix and qualified paths" {
                // Both spellings resolve to `Vesper.StructuralComparisonAttribute`.
                let suffixSrc =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparisonAttribute>]"
                            "type Pair = { X: int }"
                            "let p = { X = 0 }"
                        ]

                let artifact = compileSource "StructCmpAttrSuffix" suffixSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"

                Expect.isNotNull (typedCompareTo ty) "suffix variant routes to Structural"

                let qualifiedSrc =
                    String.concat
                        "\n"
                        [
                            "[<Vesper.StructuralComparison>]"
                            "type Pair2 = { X: int }"
                            "let p = { X = 0 }"
                        ]

                let artifact2 = compileSource "StructCmpAttrQualified" qualifiedSrc
                let asm2 = loadAssembly (Codegen.toBytes artifact2)
                let ty2 = asm2.GetType "Pair2"

                Expect.isNotNull (typedCompareTo ty2) "qualified StructuralComparison resolved to the Vesper marker"
            }

            test "the < operator routes through Comparer<T>.Default.Compare for a [<StructuralComparison>] record" {
                // The operator's base clause routes to `Comparer<Pair>.Default.Compare`,
                // which dispatches to the generated `IComparable<Pair>::CompareTo`.
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Pair = { X: int; Y: int }"
                            "let a = { X = 1; Y = 2 }"
                            "let b = { X = 1; Y = 3 }"
                            "printfn \"%b\" (a < b)" // true
                            "printfn \"%b\" (a > b)" // false
                            "printfn \"%b\" (a <= b)" // true
                            "printfn \"%b\" (a >= b)" // false
                            "printfn \"%b\" (a <= a)" // true — equal sorts <=
                            "printfn \"%b\" (a >= a)" // true — equal sorts >=
                        ]

                let artifact = compileSource "StructCmpOpRouting" src

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "true\nfalse\ntrue\nfalse\ntrue\ntrue"
                    "the four ordering ops route through Comparer<Pair>.Default.Compare"
            }

            test "the < operator on an unannotated record raises a Comparison diagnostic" {
                // An un-annotated record's `ComparisonSupport` is `NoComparison`.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Pair = { X: int }"
                            "let a = { X = 1 }"
                            "let b = { X = 2 }"
                            "let r = a < b"
                        ]

                let diagnostics = diagnoseSourceErrors "StructCmpNoCmpDiag" src
                let cmpErrors = mentioning "comparison" diagnostics

                Expect.isNonEmpty
                    cmpErrors
                    (sprintf
                        "expected a 'Comparison' constraint error for `<` on unannotated record; got %A"
                        diagnostics)
            }

            test "generic record with [<StructuralComparison>] emits the pair via Comparer<!0>" {
                // A generic record gets `CompareTo(Box<!0>)` whose body reaches
                // `Comparer<!0>.Default.Compare` for its lone field.
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralComparison>]"
                            "type Box<'T> = { Value: 'T }"
                            "let b = { Value = 0 }"
                        ]

                let artifact = compileSource "StructCmpGeneric" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "Box`1"

                Expect.isNotNull openTy "Box`1 emitted as a generic type"

                let closed = openTy.MakeGenericType(typeof<int>)

                // The closed type's `CompareTo` parameter is `Box<int>`, so the typed
                // overload is found by matching on parameter shape.
                let typedCmp =
                    closed.GetMethods(declaredInstance)
                    |> Array.tryFind (fun m ->
                        m.Name = "CompareTo"
                        && m.GetParameters().Length = 1
                        && m.GetParameters().[0].ParameterType = closed
                    )

                let objCmp =
                    closed.GetMethods(declaredInstance)
                    |> Array.tryFind (fun m ->
                        m.Name = "CompareTo"
                        && m.GetParameters().Length = 1
                        && m.GetParameters().[0].ParameterType = typeof<obj>
                    )

                Expect.isSome typedCmp "typed CompareTo(Box<int>) emitted on the closed generic"
                Expect.isSome objCmp "CompareTo(object) emitted on the closed generic"

                let mk (v: int) =
                    let ctor = closed.GetConstructors().[0]
                    ctor.Invoke([| box v |])

                Expect.equal (signOf (typedCmp.Value.Invoke(mk 1, [| mk 2 |]) :?> int)) -1 "Box<int> 1 < 2"
                Expect.equal (signOf (typedCmp.Value.Invoke(mk 5, [| mk 5 |]) :?> int)) 0 "Box<int> 5 = 5"

                let iface = typedefof<IComparable<_>>.MakeGenericType(closed)
                Expect.isTrue (iface.IsAssignableFrom closed) "Box`1 declares IComparable<Box<!0>>"
                Expect.isTrue (typeof<IComparable>.IsAssignableFrom closed) "Box`1 declares IComparable"
            }
        ]
