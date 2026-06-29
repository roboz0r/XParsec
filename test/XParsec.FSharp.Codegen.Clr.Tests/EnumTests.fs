module XParsec.FSharp.Codegen.Clr.Tests.EnumTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Numeric enum emission (step 5a): a real `System.Enum` subclass with the
// integral underlying type and one `static literal` field per case. These tests
// reflect over the emitted PE (`.IsEnum`, `Enum.GetUnderlyingType`, the literal
// fields' raw constant values) and drive `match`/`E.Ci` access at runtime so a
// bad IL shape (wrong base, missing `value__`, wrong constant) surfaces.

[<Tests>]
let enumTests =
    testList
        "Enum"
        [
            test "a numeric enum emits as a sealed System.Enum subclass with int underlying type" {
                let _, artifact =
                    compileSource
                        "EnumIntShape"
                        (String.concat "\n" [ "type Color = | Red = 0 | Green = 1 | Blue = 2"; "let c = Color.Green" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Color"
                Expect.isNotNull ty "the assembly contains the enum type Color"
                Expect.isTrue ty.IsEnum "Color emits as a System.Enum subclass (.IsEnum)"
                Expect.isTrue ty.IsValueType "an enum is a value type"
                Expect.isTrue ty.IsSealed "an enum is sealed"
                Expect.equal (Enum.GetUnderlyingType ty) typeof<int> "the underlying type is System.Int32"

                // The per-case `static literal` fields carry their constant integers.
                let raw (n: string) =
                    ty.GetField(n, BindingFlags.Public ||| BindingFlags.Static).GetRawConstantValue()

                Expect.equal (raw "Red") (box 0) "Red = 0"
                Expect.equal (raw "Green") (box 1) "Green = 1"
                Expect.equal (raw "Blue") (box 2) "Blue = 2"
            }

            test "an authored byte-width enum emits System.Byte as its underlying type" {
                let _, artifact =
                    compileSource
                        "EnumByteShape"
                        (String.concat "\n" [ "type Flags = | A = 1uy | B = 2uy"; "let f = Flags.B" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Flags"
                Expect.isTrue ty.IsEnum "Flags is a System.Enum subclass"
                Expect.equal (Enum.GetUnderlyingType ty) typeof<byte> "the authored 1uy/2uy width emits System.Byte"

                // The literal's runtime value is the boxed byte (B = 2uy).
                let bField = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Static)
                Expect.equal (bField.GetRawConstantValue()) (box 2uy) "B = 2uy (boxed byte)"
            }

            // ---- runtime behaviour: match + E.Ci access ----------------------
            // `match E.A with | E.A -> 1 | E.B -> 2 | _ -> 0` returns 1: `E.Ci` pushes
            // the case's underlying integer constant (its `literal` field is
            // metadata-only) and the `EnumCase` pattern compares the scrutinee against
            // it (underlying-int equality).
            test "match on the matching case returns its arm (E.A)" {
                runs
                    "1"
                    (String.concat
                        "\n"
                        [
                            "type E = | A = 1 | B = 2"
                            "let r = match E.A with | E.A -> 1 | E.B -> 2 | _ -> 0"
                            "printfn \"%d\" r"
                        ])
            }

            test "match falls through to the second case (E.B), proving its underlying value (2)" {
                runs
                    "2"
                    (String.concat
                        "\n"
                        [
                            "type E = | A = 1 | B = 2"
                            "let r = match E.B with | E.A -> 1 | E.B -> 2 | _ -> 0"
                            "printfn \"%d\" r"
                        ])
            }

            // `=` on enums falls out of the existing equality path: an enum value is
            // its underlying integer, so `x = E.A` is integer equality at runtime.
            test "enum value equality (=) compares the underlying integers" {
                runsLines
                    [ "eq"; "ne" ]
                    (String.concat
                        "\n"
                        [
                            "type E = | A = 1 | B = 2"
                            "let x = E.A"
                            "if x = E.A then printfn \"eq\" else printfn \"NE\""
                            "if x = E.B then printfn \"EQ\" else printfn \"ne\""
                        ])
            }

            test "an enum-typed function parameter branches on its cases at runtime" {
                runsLines
                    [ "10"; "20"; "0" ]
                    (String.concat
                        "\n"
                        [
                            "type E = | A = 1 | B = 2 | C = 3"
                            "let describe (e: E) ="
                            "    match e with"
                            "    | E.A -> 10"
                            "    | E.B -> 20"
                            "    | _ -> 0"
                            "printfn \"%d\" (describe E.A)"
                            "printfn \"%d\" (describe E.B)"
                            "printfn \"%d\" (describe E.C)"
                        ])
            }
        ]
