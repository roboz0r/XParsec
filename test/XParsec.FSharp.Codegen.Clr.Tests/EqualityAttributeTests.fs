module XParsec.FSharp.Codegen.Clr.Tests.EqualityAttributeTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.ReflectionHarness

// The equality-triple emission gate on a record / union is the type's decoded
// attribute verdict. One test per verdict path, decl side and `=` use-site side.

[<Tests>]
let tests =
    testList
        "Equality verdicts"
        [
            test "[<StructuralEquality>] on a mutable record emits the triple + IEquatable<Self>" {
                let src =
                    lines
                        [
                            "[<StructuralEquality>]"
                            "type Counter = { mutable Count: int }"
                            "let c = { Count = 0 }"
                        ]

                let artifact = compileSource "EqAttrMutStruct" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"

                Expect.isNotNull (equalsObj ty) "Equals(object) emitted on opt-in mutable record"
                Expect.isNotNull (typedEquals ty) "typed Equals(Counter) emitted"
                Expect.isNotNull (getHash ty) "GetHashCode emitted"
                Expect.isTrue (implementsIEquatable ty) "Counter declares IEquatable<Counter>"

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
                    lines
                        [
                            "[<ReferenceEquality>]"
                            "type Point = { X: int; Y: int }"
                            "let p = { X = 0; Y = 0 }"
                        ]

                let artifact = compileSource "EqAttrImmRef" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                Expect.isNull (equalsObj ty) "no Equals(object) override on a [<ReferenceEquality>] record"
                Expect.isNull (typedEquals ty) "no typed Equals on a [<ReferenceEquality>] record"
                Expect.isNull (getHash ty) "no GetHashCode override on a [<ReferenceEquality>] record"
                Expect.isFalse (implementsIEquatable ty) "Point does NOT declare IEquatable<Point>"
            }

            test "[<NoEquality>] on a record skips the triple AND diagnoses use-sites of `=`" {
                let src =
                    lines [ "[<NoEquality>]"; "type Sealed = { X: int }"; "let s = { X = 0 }" ]

                let artifact = compileSource "EqAttrNoEq" src

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Sealed"

                Expect.isNull (equalsObj ty) "no Equals(object) override on a [<NoEquality>] record"
                Expect.isNull (typedEquals ty) "no typed Equals on a [<NoEquality>] record"
                Expect.isNull (getHash ty) "no GetHashCode override on a [<NoEquality>] record"
                Expect.isFalse (implementsIEquatable ty) "Sealed does NOT declare IEquatable<Sealed>"

                // A use site of `=` is what makes the equality typar-constraint check
                // see the `NoEquality` verdict and diagnose.
                let useSrc =
                    lines
                        [
                            "[<NoEquality>]"
                            "type Sealed = { X: int }"
                            "let a = { X = 1 }"
                            "let b = { X = 1 }"
                            "let r = a = b"
                        ]

                let diagnostics = diagnoseSourceErrors "EqAttrNoEqUse" useSrc

                let eqErrors = mentioning "equality" diagnostics

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "expected an 'Equality' constraint error for `=` on Sealed; got %A" diagnostics)
            }

            test "default verdict for a union is unchanged (triple emitted)" {
                let src = lines [ "type Tag ="; "    | A"; "    | B of int"; "let t = A" ]

                let artifact = compileSource "EqAttrUnionDefault" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNotNull (equalsObj ty) "default union emits Equals(object)"
                Expect.isNotNull (typedEquals ty) "default union emits typed Equals"
                Expect.isNotNull (getHash ty) "default union emits GetHashCode"
                Expect.isTrue (implementsIEquatable ty) "default union declares IEquatable<Self>"
            }

            test "[<NoEquality>] on a union skips the triple AND diagnoses use-sites of `=`" {
                let src =
                    lines [ "[<NoEquality>]"; "type Tag ="; "    | A"; "    | B of int"; "let t = A" ]

                let artifact = compileSource "EqAttrUnionNoEq" src

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tag"

                Expect.isNull (equalsObj ty) "no Equals(object) override on [<NoEquality>] union"
                Expect.isNull (typedEquals ty) "no typed Equals on [<NoEquality>] union"
                Expect.isNull (getHash ty) "no GetHashCode override on [<NoEquality>] union"
                Expect.isFalse (implementsIEquatable ty) "Tag does NOT declare IEquatable<Tag>"

                let useSrc =
                    lines [ "[<NoEquality>]"; "type Tag ="; "    | A"; "    | B of int"; "let r = A = A" ]

                let diagnostics = diagnoseSourceErrors "EqAttrUnionNoEqUse" useSrc

                let eqErrors = mentioning "equality" diagnostics

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "expected an 'Equality' constraint error for `=` on Tag; got %A" diagnostics)
            }

            test "the decoder accepts the `Attribute` suffix" {
                // The `Attribute` suffix is optional in F#, so both spellings decode alike.
                let src =
                    lines [ "[<NoEqualityAttribute>]"; "type Sealed = { X: int }"; "let s = { X = 0 }" ]

                let artifact = compileSource "EqAttrSuffix" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Sealed"

                Expect.isNull (equalsObj ty) "no triple ⇒ the suffix variant resolved to NoEquality"
            }

            test "a qualified attribute path resolves through the type it identifies" {
                // The marker type is `Vesper.ReferenceEqualityAttribute`, so the
                // qualified spelling reaches the same identity as the bare one.
                let src =
                    lines
                        [
                            "[<Vesper.ReferenceEquality>]"
                            "type Point = { X: int }"
                            "let p = { X = 0 }"
                        ]

                let artifact = compileSource "EqAttrQualified" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                Expect.isNull (equalsObj ty) "qualified ReferenceEquality resolved to the Vesper marker"
                Expect.isFalse (implementsIEquatable ty) "no IEquatable<Point> on qualified ReferenceEquality"
            }

            test "a qualified path that does not resolve to a type is silently ignored" {
                // `Microsoft.FSharp.Core` declares nothing here, so the attribute decodes
                // to nothing and the record keeps its default structural equality
                // instead of taking the meaning of a name with the same final segment.
                let src =
                    lines
                        [
                            "[<Microsoft.FSharp.Core.ReferenceEquality>]"
                            "type Point = { X: int }"
                            "let p = { X = 0 }"
                        ]

                let artifact = compileSource "EqAttrUnresolvedQualified" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Point"

                Expect.isNotNull (equalsObj ty) "an unresolved attribute leaves the structural default"
            }

            test "[<CustomEquality>] class WITH IEquatable<Self> ⇒ no diagnostic" {
                let classSrc =
                    lines
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<ById> with"
                            "        member this.Equals(other: ById) = false"
                        ]

                // The clean compile IS the assertion: a class declaring `IEquatable<Self>`
                // draws no custom-equality finding.
                compileSource "EqAttrCustomEqOk" classSrc |> ignore
            }

            test "[<CustomEquality>] class WITHOUT IEquatable<Self> ⇒ must-implement error" {
                let src =
                    lines
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                        ]

                let diagnostics = diagnoseSourceErrors "EqAttrCustomEqMissing" src

                let customErrs = mentioning "IEquatable" diagnostics

                Expect.isNonEmpty
                    customErrs
                    (sprintf "missing IEquatable<Self> ⇒ must-implement error; got %A" diagnostics)
            }

            // The registries are keyed by `TypeKey`, one entry per type, so the
            // conformance sweep visits a generic host once. A string key would enter it
            // under both its bare name and its ``name`N`` arity-key, doubling every error.
            test "[<CustomEquality>] generic class WITHOUT IEquatable<Self> ⇒ exactly one error of each kind" {
                let src =
                    lines
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type ById<'a>(id: 'a) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                        ]

                let diagnostics = diagnoseSourceErrors "EqAttrCustomEqGenericMissing" src

                let count (s: string) = mentioning s diagnostics |> List.length

                Expect.equal (count "IEquatable") 1 "one must-implement-IEquatable error, not one per registry entry"

                Expect.equal
                    (count "must override 'Object.GetHashCode()'")
                    1
                    "one must-override-GetHashCode error, not one per registry entry"
            }

            test "[<CustomComparison>] without [<CustomEquality>] ⇒ coherence error" {
                let src =
                    lines
                        [
                            "[<CustomComparison>]"
                            "type ById(id: int) ="
                            "    member _.Id = id"
                            "    interface System.IComparable<ById> with"
                            "        member this.CompareTo(other: ById) = 0"
                        ]

                let diagnostics = diagnoseSourceErrors "EqAttrCustomCmpIncoherent" src

                let coherenceErrs = mentioning "must also have [<CustomEquality>]" diagnostics

                Expect.isNonEmpty
                    coherenceErrs
                    (sprintf "custom comparison without custom equality ⇒ coherence error; got %A" diagnostics)
            }

            test "[<CustomEquality>] on a record ⇒ wrap-in-a-class scope error" {
                let src = lines [ "[<CustomEquality; NoComparison>]"; "type R = { x: int }" ]

                let diagnostics = diagnoseSourceErrors "EqAttrCustomRecordScope" src

                let scopeErrs = mentioning "wrap the type in a class" diagnostics

                Expect.isNonEmpty scopeErrs (sprintf "custom equality on a record ⇒ scope error; got %A" diagnostics)
            }

            test "[<CustomEquality>] class supports `=` at a use site (no constraint diagnostic)" {
                let src =
                    lines
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

                // The clean compile IS the assertion: `=` over a class with custom equality
                // draws no constraint finding at the use site.
                compileSource "EqAttrClassCustomUse" src |> ignore
            }

            test "[<NoEquality>] class at a `=` use site is rejected" {
                let src =
                    lines
                        [
                            "[<NoEquality; NoComparison>]"
                            "type Opaque(id: int) ="
                            "    member _.Id = id"
                            "let a = Opaque(1)"
                            "let b = Opaque(1)"
                            "let _ = (a = b)"
                        ]

                let diagnostics = diagnoseSourceErrors "EqAttrClassNoEqUse" src

                let eqErrors = mentioning "equality" diagnostics

                Expect.isNonEmpty
                    eqErrors
                    (sprintf "[<NoEquality>] class at `=` ⇒ equality constraint error; got %A" diagnostics)
            }
        ]
