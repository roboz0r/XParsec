module XParsec.FSharp.Codegen.Clr.Tests.CustomEqualityComparisonDispatchTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// `=` / `<` lower to `EqualityComparer<^T>.Default.Equals` /
// `Comparer<^T>.Default.Compare`, which dispatch to a `[<CustomEquality>]` /
// `[<CustomComparison>]` class's own `IEquatable<Self>` / `IComparable<Self>`.

[<Tests>]
let tests =
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
                // The source spells the canonical `interface equatable<Tagged>`; the
                // encoder reconciles it to the platform `System.IEquatable<Tagged>`,
                // which is what `EqualityComparer<Tagged>.Default` dispatches through.
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

                let artifact = compileSource "CustomEqDispatchCanonical" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "true\nfalse\ntrue"
                    "`=` dispatches to equatable<Tagged>.Equals via the real System.IEquatable<Tagged>"
            }

            test "[<CustomEquality>] class: `=` invokes the user's IEquatable<Self>.Equals (NOT structural / reference)" {
                // The custom `Equals` compares only `id`. `Tagged(1, 10)` and
                // `Tagged(1, 20)` are neither reference-equal nor structurally equal,
                // so `a = b` printing `true` can only come from the user's member.
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

                let artifact = compileSource "CustomEqDispatch" src

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
                // The custom `CompareTo` sorts by id DESCENDING, so `Ranked(1) <
                // Ranked(2)` is false where any natural ordering would give true.
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

                let artifact = compileSource "CustomCmpDispatch" src

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

            test "[<CustomEquality>] class does not emit a synthesized typed Equals(Self) or a CompareTo" {
                // Reflection surfaces the user's interface method as a typed
                // `Equals(Tagged)`, so the absence of synthesis is asserted on the
                // comparison pair: this class has no `CompareTo` member at all.
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

                let artifact = compileSource "CustomEqNoPair" src
                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Tagged"

                Expect.isNull
                    (typedCompareTo ty)
                    "no synthesized CompareTo(Tagged) on a [<CustomEquality; NoComparison>] class"

                Expect.isFalse (implementsIComparable ty) "Tagged does NOT declare IComparable<Tagged>"
            }
        ]
