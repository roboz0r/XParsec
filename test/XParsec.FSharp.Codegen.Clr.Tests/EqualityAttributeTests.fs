module XParsec.FSharp.Codegen.Clr.Tests.EqualityAttributeTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The equality-triple emission gate on a
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

    let errors (tast: TastFile) = tast.Diagnostics |> Diagnostic.errors

    testList
        "Equality verdicts"
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
                // Reasserts the default: a union with no
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

            // Phase 3 — the `Custom` posture's semantic requirement (the type must
            // implement the matching self-instantiated BCL interface), the
            // coherence rule (custom comparison ⇒ custom equality), and the
            // record/union scope diagnostic.

            test "[<CustomEquality>] class WITH IEquatable<Self> ⇒ no diagnostic" {
                let classSrc =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<ById> with"
                            "        member this.Equals(other: ById) = false"
                        ]

                let tast, _ = compileSource "EqAttrCustomEqOk" classSrc

                let customErrs =
                    errors tast
                    |> List.filter (fun d -> d.Message.Contains "IEquatable" || d.Message.Contains "CustomEquality")

                Expect.isEmpty
                    customErrs
                    (sprintf "class implementing IEquatable<Self> ⇒ no custom-eq diagnostic; got %A" (errors tast))
            }

            test "[<CustomEquality>] class WITHOUT IEquatable<Self> ⇒ must-implement error" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                        ]

                let tast, _ = compileSource "EqAttrCustomEqMissing" src

                let customErrs =
                    errors tast |> List.filter (fun d -> d.Message.Contains "IEquatable")

                Expect.isNonEmpty
                    customErrs
                    (sprintf "missing IEquatable<Self> ⇒ must-implement error; got %A" (errors tast))
            }

            // The custom-eq/comp conformance sweep visits each host ONCE. It iterates the
            // class/union/record registries, which are keyed by `TypeKey` — one entry per
            // type. A GENERIC host is the regression guard: while the registries were
            // string-keyed, a single-arity generic type sat under BOTH its bare name and
            // its ``name`N`` arity-key, so the sweep visited it twice and every diagnostic
            // it raised was emitted twice.
            test "[<CustomEquality>] generic class WITHOUT IEquatable<Self> ⇒ exactly one error of each kind" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById<'a>(id: 'a) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                        ]

                let tast, _ = compileSource "EqAttrCustomEqGenericMissing" src

                let count (s: string) =
                    errors tast |> List.filter (fun d -> d.Message.Contains s) |> List.length

                Expect.equal (count "IEquatable") 1 "one must-implement-IEquatable error, not one per registry entry"

                Expect.equal
                    (count "must override 'Object.GetHashCode()'")
                    1
                    "one must-override-GetHashCode error, not one per registry entry"
            }

            test "[<CustomComparison>] without [<CustomEquality>] ⇒ coherence error" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    interface System.IComparable<ById> with"
                            "        member this.CompareTo(other: ById) = 0"
                        ]

                let tast, _ = compileSource "EqAttrCustomCmpIncoherent" src

                let coherenceErrs =
                    errors tast
                    |> List.filter (fun d -> d.Message.Contains "must also have [<CustomEquality>]")

                Expect.isNonEmpty
                    coherenceErrs
                    (sprintf "custom comparison without custom equality ⇒ coherence error; got %A" (errors tast))
            }

            test "[<CustomEquality>] on a record ⇒ wrap-in-a-class scope error" {
                let src =
                    String.concat "\n" [ "[<CustomEquality; NoComparison>]"; "type R = { x: int }" ]

                let tast, _ = compileSource "EqAttrCustomRecordScope" src

                let scopeErrs =
                    errors tast
                    |> List.filter (fun d -> d.Message.Contains "wrap the type in a class")

                Expect.isNonEmpty scopeErrs (sprintf "custom equality on a record ⇒ scope error; got %A" (errors tast))
            }

            // Phase 4 — the front-end constraint gate now honours the class
            // verdict at `=` use sites (instead of unconditionally deferring).

            test "[<CustomEquality>] class supports `=` at a use site (no constraint diagnostic)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<ById> with"
                            "        member this.Equals(other: ById) = false"
                            "let a = ById(1)"
                            "let b = ById(1)"
                            "let _ = (a = b)"
                        ]

                let tast, _ = compileSource "EqAttrClassCustomUse" src

                let eqErrors = errors tast |> List.filter (fun d -> d.Message.Contains "equality")

                Expect.isEmpty
                    eqErrors
                    (sprintf "Custom class supports `=` ⇒ no equality diagnostic; got %A" (errors tast))
            }

            test "[<NoEquality>] class at a `=` use site is rejected" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<NoEquality; NoComparison>]"
                            "type Opaque(id: int) ="
                            "    member _.Id = id"
                            "let a = Opaque(1)"
                            "let b = Opaque(1)"
                            "let _ = (a = b)"
                        ]

                let tast, _ = compileSource "EqAttrClassNoEqUse" src

                let eqErrors = errors tast |> List.filter (fun d -> d.Message.Contains "equality")

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "[<NoEquality>] class at `=` ⇒ equality constraint error; got %A" (errors tast))
            }
        ]
