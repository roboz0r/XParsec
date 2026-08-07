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

            // ---- string / mixed enum struct-wrapper emission -------------------
            // SEQUENCING CHECK: a string enum is a `[<Struct>]`
            // wrapper whose per-case `public static initonly` field is `.cctor`-init'd
            // by constructing the wrapper from the case's string literal. Reflect over
            // the loaded PE to prove the field emitted, the type is a value type, and
            // the cctor RAN (the field's wrapped string round-trips) — no use site,
            // pure emission.
            test "SEQUENCING: a string enum emits a static-initonly struct field, cctor-initialised, readable" {
                let _, artifact =
                    compileSource
                        "StringEnumSeq"
                        (String.concat "\n" [ "type Dir = | Up = \"up\" | Down = \"down\""; "let x = 1" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Dir"
                Expect.isNotNull ty "the assembly contains the string enum type Dir"
                Expect.isTrue ty.IsValueType "a string enum emits as a [<Struct>] value type"
                Expect.isFalse ty.IsEnum "a string enum is NOT a System.Enum (no integral underlying type)"

                // The per-case field is a public static field OF the enum type.
                let upField = ty.GetField("Up", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull upField "the case 'Up' is a static field"
                Expect.equal upField.FieldType ty "the case field's type is the enum struct itself"

                // The `.cctor` constructed it: read the wrapped string off the backing
                // instance field of the boxed struct value.
                let readWrapped (caseName: string) : string =
                    let boxed =
                        ty.GetField(caseName, BindingFlags.Public ||| BindingFlags.Static).GetValue(null)

                    let backing = ty.GetField("value", BindingFlags.Public ||| BindingFlags.Instance)
                    backing.GetValue(boxed) :?> string

                Expect.equal (readWrapped "Up") "up" "Dir.Up wraps the string \"up\" (cctor + ctor ran)"
                Expect.equal (readWrapped "Down") "down" "Dir.Down wraps the string \"down\""
            }

            // E.Up round-trips its string through a match arm + printf.
            test "a string enum case round-trips its string through a match" {
                runs
                    "up"
                    (String.concat
                        "\n"
                        [
                            "type Dir = | Up = \"up\" | Down = \"down\""
                            "let render (d: Dir) ="
                            "    match d with"
                            "    | Dir.Up -> \"up\""
                            "    | Dir.Down -> \"down\""
                            "    | _ -> \"?\""
                            "printfn \"%s\" (render Dir.Up)"
                        ])
            }

            test "a string enum match distinguishes the cases at runtime" {
                runsLines
                    [ "U"; "D"; "U" ]
                    (String.concat
                        "\n"
                        [
                            "type Dir = | Up = \"up\" | Down = \"down\""
                            "let tag (d: Dir) ="
                            "    match d with"
                            "    | Dir.Up -> \"U\""
                            "    | Dir.Down -> \"D\""
                            "    | _ -> \"?\""
                            "printfn \"%s\" (tag Dir.Up)"
                            "printfn \"%s\" (tag Dir.Down)"
                            "let x = Dir.Up"
                            "printfn \"%s\" (tag x)"
                        ])
            }

            test "string enum equality compares the wrapped string (= and the unequal case)" {
                runsLines
                    [ "eq"; "ne" ]
                    (String.concat
                        "\n"
                        [
                            "type Dir = | Up = \"up\" | Down = \"down\""
                            "let x = Dir.Up"
                            "if x = Dir.Up then printfn \"eq\" else printfn \"NE\""
                            "if x = Dir.Down then printfn \"EQ\" else printfn \"ne\""
                        ])
            }

            test "a string enum loads as a value type (.IsValueType)" {
                let _, artifact =
                    compileSource
                        "StringEnumIsValue"
                        (String.concat "\n" [ "type Dir = | Up = \"up\" | Down = \"down\""; "let d = Dir.Up" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Dir"
                Expect.isTrue ty.IsValueType "a string enum is a value type"
                Expect.isTrue ty.IsSealed "a string enum struct is sealed"
            }

            // ---- mixed enum (int + string), obj-wrapper repr ------------------
            test "a mixed enum match distinguishes int and string cases at runtime" {
                runsLines
                    [ "isA"; "isB"; "isA" ]
                    (String.concat
                        "\n"
                        [
                            "type M = | A = 1 | B = \"x\""
                            "let tag (m: M) ="
                            "    match m with"
                            "    | M.A -> \"isA\""
                            "    | M.B -> \"isB\""
                            "    | _ -> \"?\""
                            "printfn \"%s\" (tag M.A)"
                            "printfn \"%s\" (tag M.B)"
                            "let x = M.A"
                            "printfn \"%s\" (tag x)"
                        ])
            }

            test "mixed enum equality compares the boxed values structurally" {
                runsLines
                    [ "eq"; "ne" ]
                    (String.concat
                        "\n"
                        [
                            "type M = | A = 1 | B = \"x\""
                            "let x = M.A"
                            "if x = M.A then printfn \"eq\" else printfn \"NE\""
                            "if x = M.B then printfn \"EQ\" else printfn \"ne\""
                        ])
            }

            test "a mixed enum wraps obj and round-trips its boxed int / string" {
                let _, artifact =
                    compileSource "MixedEnumSeq" (String.concat "\n" [ "type M = | A = 1 | B = \"x\""; "let m = M.A" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "M"
                Expect.isTrue ty.IsValueType "a mixed enum is a value type"
                let backing = ty.GetField("value", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal backing.FieldType typeof<obj> "the mixed wrapper's backing field is obj"

                let readWrapped (caseName: string) : obj =
                    let boxed =
                        ty.GetField(caseName, BindingFlags.Public ||| BindingFlags.Static).GetValue(null)

                    backing.GetValue(boxed)

                Expect.equal (readWrapped "A") (box 1) "M.A wraps the boxed int 1"
                Expect.equal (readWrapped "B") (box "x") "M.B wraps the string \"x\""
            }
        ]
