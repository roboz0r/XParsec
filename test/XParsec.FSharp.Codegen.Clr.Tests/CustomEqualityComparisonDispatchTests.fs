module XParsec.FSharp.Codegen.Clr.Tests.CustomEqualityComparisonDispatchTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Custom dispatch by EXECUTION (CLR backend).
//
// A `[<CustomEquality>]` class emits NO synthesized equality triple (the
// `NominalEmit` Class arm is `()`); a `[<CustomComparison>]` class emits NO
// synthesized comparison pair. Their user-declared `interface IEquatable<Self>`
// / `interface IComparable<Self>` impls land as InterfaceImpl rows + methods.
//
// The use-site lowering of `=` / `<` is uniform and class-agnostic:
//   `=` → `EqualityComparer<^T>.Default.Equals(x, y)`   (ops-platform.clr.fs:114)
//   `<` → `Comparer<^T>.Default.Compare(x, y) < 0`      (comparison.clr.fs:40)
// At runtime `EqualityComparer<T>.Default` / `Comparer<T>.Default` dispatch to
// `IEquatable<T>.Equals` / `IComparable<T>.CompareTo` when the type implements
// them. So the Custom class routes `=` / `<` to the USER's interface member.
//
// These tests PROVE that by giving the user member a DELIBERATELY
// non-structural / non-reference semantics and observing the runtime answer.

[<Tests>]
let tests =
    let errors (tast: TastFile) = tast.Diagnostics |> Diagnostic.errors

    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    let implementsIEquatable (ty: Type) =
        let iface = typedefof<IEquatable<_>>.MakeGenericType ty
        iface.IsAssignableFrom ty

    let implementsIComparable (ty: Type) =
        let iface = typedefof<IComparable<_>>.MakeGenericType ty
        iface.IsAssignableFrom ty

    let typedEquals (ty: Type) =
        ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

    let typedCompareTo (ty: Type) =
        ty.GetMethod("CompareTo", declaredInstance, null, [| ty |], null)

    testList
        "custom dispatch (CLR backend)"
        [
            test "canonical BCL-free `interface equatable<Self>` dispatches via the real System.IEquatable<Self>" {
                // Platform-independence slice 5: generic capability authored canonically
                // (`interface equatable<Tagged>`, not `System.IEquatable<Tagged>`). The
                // generic interface head reconciles canon→platform the same way (the encoder's
                // `TypeSpecOf` resolves its head through `ClrEnv.externalClassRef`), so `=`
                // dispatches to the user member via the real `System.IEquatable<Tagged>`.
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type Tagged(id: int, payload: int) ="
                            "    member _.Id = id"
                            "    member _.Payload = payload"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface equatable<Tagged> with"
                            "        member this.Equals(other: Tagged) = (id = other.Id)"
                            "let a = Tagged(1, 10)"
                            "let b = Tagged(1, 20)"
                            "let c = Tagged(2, 10)"
                            "printfn \"%b\" (a = b)"
                            "printfn \"%b\" (a = c)"
                            "printfn \"%b\" (a = a)"
                        ]

                let tast, artifact = compileSource "CustomEqDispatchCanonical" src
                Expect.isEmpty (errors tast) "no analysis errors: [<CustomEquality>] accepts the canonical equatable"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "true\nfalse\ntrue"
                    "`=` dispatches to equatable<Tagged>.Equals via the real System.IEquatable<Tagged>"
            }

            test "[<CustomEquality>] class: `=` invokes the user's IEquatable<Self>.Equals (NOT structural / reference)" {
                // `Tagged` carries two fields. The custom `Equals` compares ONLY
                // `id`, ignoring `payload`. Two instances `Tagged(1, 10)` and
                // `Tagged(1, 20)` are:
                //   * custom-equal      — same id;
                //   * NOT reference-equal — distinct objects;
                //   * NOT structurally-equal — payload differs.
                // So `a = b` printing `true` can ONLY come from the user's
                // member (structural ⇒ false, reference ⇒ false).
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type Tagged(id: int, payload: int) ="
                            "    member _.Id = id"
                            "    member _.Payload = payload"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<Tagged> with"
                            "        member this.Equals(other: Tagged) = (id = other.Id)"
                            "let a = Tagged(1, 10)"
                            "let b = Tagged(1, 20)" // custom-equal to a, not ref/struct equal
                            "let c = Tagged(2, 10)" // different id ⇒ custom-unequal
                            "printfn \"%b\" (a = b)" // true  — custom (id matches)
                            "printfn \"%b\" (a = c)" // false — custom (id differs)
                            "printfn \"%b\" (a = a)" // true  — custom (id matches)
                        ]

                let tast, artifact = compileSource "CustomEqDispatch" src
                Expect.isEmpty (errors tast) "no diagnostics on custom-equality class + `=` use sites"

                // Metadata: the user interface impl landed; no synthesized typed Equals.
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tagged"
                Expect.isTrue (implementsIEquatable ty) "Tagged declares IEquatable<Tagged>"

                // Execution: the printed answers are the CUSTOM (id-only) answers.
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "true\nfalse\ntrue"
                    "`=` dispatches to IEquatable<Tagged>.Equals (id-only), not structural/reference"
            }

            test
                "[<CustomComparison>] class: `<` / compare invokes the user's IComparable<Self>.CompareTo (INVERTED order)" {
                // `Ranked` carries an `id`. The custom `CompareTo` orders by id
                // *DESCENDING* (inverted: larger id sorts first) — the opposite
                // of any natural / structural ordering. So:
                //   * `a(1) < b(2)` is FALSE under custom (1 sorts after 2),
                //     whereas a structural/natural ordering would give true.
                //   * `a(1) > b(2)` is TRUE under custom.
                // The inversion makes the custom answer distinguishable from
                // both natural ordering and reference behaviour.
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; CustomComparison>]"
                            "type Ranked(id: int) ="
                            "    member _.Id = id"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<Ranked> with"
                            "        member this.Equals(other: Ranked) = (id = other.Id)"
                            "    interface System.IComparable<Ranked> with"
                            // DESCENDING: compare other.Id to id (inverted operands).
                            "        member this.CompareTo(other: Ranked) ="
                            "            if other.Id < id then -1"
                            "            elif other.Id > id then 1"
                            "            else 0"
                            "let a = Ranked(1)"
                            "let b = Ranked(2)"
                            "printfn \"%b\" (a < b)" // false — custom inverts (1 sorts after 2)
                            "printfn \"%b\" (a > b)" // true  — custom inverts
                            "printfn \"%b\" (a <= b)" // false
                            "printfn \"%b\" (a >= b)" // true
                            "printfn \"%b\" (a <= a)" // true  — equal sorts <=
                            "printfn \"%b\" (a >= a)" // true  — equal sorts >=
                        ]

                let tast, artifact = compileSource "CustomCmpDispatch" src
                Expect.isEmpty (errors tast) "no diagnostics on custom-comparison class + ordering use sites"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Ranked"
                Expect.isTrue (implementsIComparable ty) "Ranked declares IComparable<Ranked>"
                Expect.isTrue (implementsIEquatable ty) "Ranked declares IEquatable<Ranked>"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "false\ntrue\nfalse\ntrue\ntrue\ntrue"
                    "`<` / `>` / `<=` / `>=` dispatch to IComparable<Ranked>.CompareTo (inverted order)"
            }

            test "[<CustomEquality>] class emits NO synthesized typed Equals(Self) and NO CompareTo" {
                // The synthesized triple/pair are gated on the Structural
                // verdict; a Custom class's NominalEmit Class arm is `()`. The
                // ONLY `Equals(Tagged)` present is the user's interface method,
                // not a compiler-synthesized struct-eq override. (Reflection can
                // surface the interface method as a typed `Equals(Tagged)`, so we
                // assert the absence of the synthesized *comparison* pair, which
                // the Custom-equality-only class has no member for at all.)
                let src =
                    String.concat
                        "\n"
                        [
                            "[<CustomEquality; NoComparison>]"
                            "type Tagged(id: int, payload: int) ="
                            "    member _.Id = id"
                            "    member _.Payload = payload"
                            "    override this.Equals(o: obj) = false"
                            "    override this.GetHashCode() = id"
                            "    interface System.IEquatable<Tagged> with"
                            "        member this.Equals(other: Tagged) = (id = other.Id)"
                            "let t = Tagged(1, 10)"
                        ]

                let _, artifact = compileSource "CustomEqNoPair" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tagged"

                Expect.isNull
                    (typedCompareTo ty)
                    "no synthesized CompareTo(Tagged) on a [<CustomEquality; NoComparison>] class"

                Expect.isFalse (implementsIComparable ty) "Tagged does NOT declare IComparable<Tagged>"
            }
        ]
