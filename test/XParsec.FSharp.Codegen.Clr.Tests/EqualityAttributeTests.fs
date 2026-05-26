module XParsec.FSharp.Codegen.Clr.Tests.EqualityAttributeTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// C-Attr (docs/records-plan.md §B4): the equality-triple emission gate on a
// record / union comes from the type's attributes (filled by
// `NameResolution.registerRecordTypeDefn` / `registerUnionTypeDefn` via the
// `Passes.Attributes` decoder). These tests pin every verdict path against
// the same record / union shapes used by `RecordTests.fs` /
// `StructuralEqualityTests.fs`.

[<Tests>]
let tests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let equalsObj (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

    let typedEquals (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

    let getHash (ty: Type) =
        ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

    let implementsIEquatable (ty: Type) =
        let iface = typedefof<IEquatable<_>>.MakeGenericType ty
        iface.IsAssignableFrom ty

    let errors (tast: TastFile) =
        tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    testList
        "C-Attr equality verdicts"
        [
            test "[<StructuralEquality>] on a mutable record emits the triple + IEquatable<Self>" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<StructuralEquality>]"
                            "type Counter = { mutable Count: int }"
                            "let c = { Count = 0 }"
                        ]

                let _, artifact = compileSource "EqAttrMutStruct" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"

                Expect.isNotNull (equalsObj ty) "Equals(object) emitted on opt-in mutable record"
                Expect.isNotNull (typedEquals ty) "typed Equals(Counter) emitted"
                Expect.isNotNull (getHash ty) "GetHashCode emitted"
                Expect.isTrue (implementsIEquatable ty) "Counter declares IEquatable<Counter>"

                // The triple compares fields, so two equal-field instances compare equal.
                let mk count =
                    Activator.CreateInstance(ty, [| box (count: int) |])

                let r1 = mk 7
                let r2 = mk 7
                let r3 = mk 8

                Expect.isTrue ((equalsObj ty).Invoke(r1, [| r2 |]) :?> bool) "same field ⇒ Equals(object) true"
                Expect.isFalse ((equalsObj ty).Invoke(r1, [| r3 |]) :?> bool) "different field ⇒ Equals(object) false"
            }

            test "[<ReferenceEquality>] on an immutable record skips the triple (reference identity)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<ReferenceEquality>]"
                            "type Point = { X: int; Y: int }"
                            "let p = { X = 0; Y = 0 }"
                        ]

                let _, artifact = compileSource "EqAttrImmRef" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                Expect.isNull (equalsObj ty) "no Equals(object) override on a [<ReferenceEquality>] record"
                Expect.isNull (typedEquals ty) "no typed Equals on a [<ReferenceEquality>] record"
                Expect.isNull (getHash ty) "no GetHashCode override on a [<ReferenceEquality>] record"
                Expect.isFalse (implementsIEquatable ty) "Point does NOT declare IEquatable<Point>"
            }

            test "[<NoEquality>] on a record skips the triple AND diagnoses use-sites of `=`" {
                // Decl alone (no use site): triple is skipped, no diagnostic.
                let src =
                    String.concat "\n" [ "[<NoEquality>]"; "type Sealed = { X: int }"; "let s = { X = 0 }" ]

                let tast, artifact = compileSource "EqAttrNoEq" src
                Expect.isEmpty (errors tast) "decl alone ⇒ no equality diagnostic"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Sealed"

                Expect.isNull (equalsObj ty) "no Equals(object) override on a [<NoEquality>] record"
                Expect.isNull (typedEquals ty) "no typed Equals on a [<NoEquality>] record"
                Expect.isNull (getHash ty) "no GetHashCode override on a [<NoEquality>] record"
                Expect.isFalse (implementsIEquatable ty) "Sealed does NOT declare IEquatable<Sealed>"

                // Now wire a use site of `=` against the type — the
                // `Equality` typar-constraint check sees the `NoEquality`
                // verdict and emits a diagnostic (`Unification.checkConstraint`).
                let useSrc =
                    String.concat
                        "\n"
                        [
                            "[<NoEquality>]"
                            "type Sealed = { X: int }"
                            "let a = { X = 1 }"
                            "let b = { X = 1 }"
                            "let r = a = b"
                        ]

                let useTast, _ = compileSource "EqAttrNoEqUse" useSrc

                let eqErrors =
                    errors useTast |> List.filter (fun d -> d.Message.Contains "equality")

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "expected an 'Equality' constraint error for `=` on Sealed; got %A" (errors useTast))
            }

            test "default verdict for a union is unchanged (triple emitted)" {
                // Reasserts the records-plan §B4 default: a union with no
                // attribute still emits the triple.
                let src =
                    String.concat "\n" [ "type Tag ="; "    | A"; "    | B of int"; "let t = A" ]

                let _, artifact = compileSource "EqAttrUnionDefault" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNotNull (equalsObj ty) "default union emits Equals(object)"
                Expect.isNotNull (typedEquals ty) "default union emits typed Equals"
                Expect.isNotNull (getHash ty) "default union emits GetHashCode"
                Expect.isTrue (implementsIEquatable ty) "default union declares IEquatable<Self>"
            }

            test "[<NoEquality>] on a union skips the triple AND diagnoses use-sites of `=`" {
                let src =
                    String.concat "\n" [ "[<NoEquality>]"; "type Tag ="; "    | A"; "    | B of int"; "let t = A" ]

                let tast, artifact = compileSource "EqAttrUnionNoEq" src
                Expect.isEmpty (errors tast) "decl alone ⇒ no equality diagnostic"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNull (equalsObj ty) "no Equals(object) override on [<NoEquality>] union"
                Expect.isNull (typedEquals ty) "no typed Equals on [<NoEquality>] union"
                Expect.isNull (getHash ty) "no GetHashCode override on [<NoEquality>] union"
                Expect.isFalse (implementsIEquatable ty) "Tag does NOT declare IEquatable<Tag>"

                let useSrc =
                    String.concat "\n" [ "[<NoEquality>]"; "type Tag ="; "    | A"; "    | B of int"; "let r = A = A" ]

                let useTast, _ = compileSource "EqAttrUnionNoEqUse" useSrc

                let eqErrors =
                    errors useTast |> List.filter (fun d -> d.Message.Contains "equality")

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "expected an 'Equality' constraint error for `=` on Tag; got %A" (errors useTast))
            }

            test "the decoder accepts the `Attribute` suffix" {
                // `[<NoEqualityAttribute>]` is the F# rule (suffix optional);
                // the decoder must treat it identically to `[<NoEquality>]`.
                let src =
                    String.concat "\n" [ "[<NoEqualityAttribute>]"; "type Sealed = { X: int }"; "let s = { X = 0 }" ]

                let _, artifact = compileSource "EqAttrSuffix" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Sealed"

                Expect.isNull (equalsObj ty) "no triple ⇒ the suffix variant resolved to NoEquality"
            }

            test "a fully-qualified attribute path still resolves on its leaf segment" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Microsoft.FSharp.Core.ReferenceEquality>]"
                            "type Point = { X: int }"
                            "let p = { X = 0 }"
                        ]

                let _, artifact = compileSource "EqAttrQualified" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                Expect.isNull (equalsObj ty) "qualified ReferenceEquality resolved by leaf segment"
                Expect.isFalse (implementsIEquatable ty) "no IEquatable<Point> on qualified ReferenceEquality"
            }
        ]
