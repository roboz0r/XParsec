module XParsec.FSharp.Codegen.Clr.Tests.ComparisonModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// `< > <= >=` from `Vesper.Comparison`. Every declaration there is `inline`, so
// the operators splice at each use site and there is no DLL to reflect: one CIL
// clause per primitive over a `Comparer<^T>.Default.Compare` base, one row each.

[<Tests>]
let primitiveTests =
    testList
        "ComparisonPrimitives"
        [
            for src, expected in
                [
                    // byte: 0..255 are all positive as i4, so signed `clt`/`cgt` is correct
                    """printfn "%b" (1uy < 2uy)""", "true"
                    """printfn "%b" (2uy < 1uy)""", "false"
                    """printfn "%b" (2uy > 1uy)""", "true"
                    """printfn "%b" (1uy > 2uy)""", "false"
                    """printfn "%b" (1uy <= 1uy)""", "true"
                    """printfn "%b" (2uy <= 1uy)""", "false"
                    """printfn "%b" (1uy >= 1uy)""", "true"
                    """printfn "%b" (1uy >= 2uy)""", "false"

                    // char: `clt`/`cgt` over the char's int value
                    """printfn "%b" ('a' < 'b')""", "true"
                    """printfn "%b" ('b' < 'a')""", "false"
                    """printfn "%b" ('a' > 'b')""", "false"
                    """printfn "%b" ('b' > 'a')""", "true"
                    """printfn "%b" ('a' <= 'a')""", "true"
                    """printfn "%b" ('b' <= 'a')""", "false"
                    """printfn "%b" ('b' >= 'a')""", "true"
                    """printfn "%b" ('a' >= 'b')""", "false"

                    // bool: ordered as i4, so `false` = 0 and `true` = 1
                    """printfn "%b" (false < true)""", "true"
                    """printfn "%b" (true < false)""", "false"
                    """printfn "%b" (true > false)""", "true"
                    """printfn "%b" (false > true)""", "false"
                    """printfn "%b" (false <= false)""", "true"
                    """printfn "%b" (true <= false)""", "false"
                    """printfn "%b" (true >= true)""", "true"
                    """printfn "%b" (false >= true)""", "false"

                    // float32: IEEE `clt`/`cgt`
                    """printfn "%b" (1.5f < 2.5f)""", "true"
                    """printfn "%b" (2.5f < 1.5f)""", "false"
                    """printfn "%b" (2.5f > 1.5f)""", "true"
                    """printfn "%b" (1.5f > 2.5f)""", "false"
                    """printfn "%b" (2.5f <= 2.5f)""", "true"
                    """printfn "%b" (2.5f <= 1.5f)""", "false"
                    """printfn "%b" (2.5f >= 2.5f)""", "true"
                    """printfn "%b" (1.5f >= 2.5f)""", "false"

                    // float: IEEE `clt`/`cgt`
                    """printfn "%b" (2.5 > 1.5)""", "true"
                    """printfn "%b" (1.5 > 2.5)""", "false"
                    """printfn "%b" (2.5 <= 2.5)""", "true"
                    """printfn "%b" (2.5 <= 1.5)""", "false"
                    """printfn "%b" (2.5 >= 2.5)""", "true"
                    """printfn "%b" (1.5 >= 2.5)""", "false"

                    // int64: `clt`/`cgt` over 64-bit operands
                    """printfn "%b" (1L < 2L)""", "true"
                    """printfn "%b" (2L < 1L)""", "false"
                    """printfn "%b" (2L > 1L)""", "true"
                    """printfn "%b" (1L > 2L)""", "false"
                    """printfn "%b" (2L <= 2L)""", "true"
                    """printfn "%b" (2L <= 1L)""", "false"
                    """printfn "%b" (2L >= 2L)""", "true"
                    """printfn "%b" (1L >= 2L)""", "false"
                ] -> test src { runs expected src }
        ]

// The base clause: an operand type with no `when ^T:` clause routes through
// `Comparer<^T>.Default.Compare`. `string` probes it with no codegen-generated
// `CompareTo` involved, because the BCL supplies `IComparable<string>`.
[<Tests>]
let baseTests =
    testList
        "ComparisonBase"
        [
            for src, expected in
                [
                    """printfn "%b" ("a" < "b")""", "true"
                    """printfn "%b" ("b" < "a")""", "false"
                    """printfn "%b" ("b" > "a")""", "true"
                    """printfn "%b" ("a" <= "a")""", "true"
                    """printfn "%b" ("b" <= "a")""", "false"
                    """printfn "%b" ("b" >= "a")""", "true"
                    """printfn "%b" ("a" >= "b")""", "false"
                ] -> test src { runs expected src }
        ]

// Analysis only: a generic use infers `when 'T: comparison`, and ordering an
// un-annotated record is rejected because comparison support is opt-in.
[<Tests>]
let frontEndTests =
    testList
        "ComparisonFrontEnd"
        [
            test "generic `<` infers a comparison constraint and type-checks" { typeChecks "let lt a b = a < b" }
            test "generic `>` infers a comparison constraint and type-checks" { typeChecks "let gt a b = a > b" }
            test "generic `<=` infers a comparison constraint and type-checks" { typeChecks "let le a b = a <= b" }
            test "generic `>=` infers a comparison constraint and type-checks" { typeChecks "let ge a b = a >= b" }

            test "`<` on an un-annotated record raises a comparison constraint error" {
                failsWith
                    "comparison"
                    (String.concat
                        "\n"
                        [
                            "type Pair = { X: int }"
                            "let a = { X = 1 }"
                            "let b = { X = 2 }"
                            "let r = a < b"
                        ])
            }
        ]
