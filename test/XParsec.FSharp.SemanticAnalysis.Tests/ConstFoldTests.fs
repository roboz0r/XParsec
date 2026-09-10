module XParsec.FSharp.SemanticAnalysis.Tests.ConstFoldTests

open Expecto
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.ConstExprCheckTests

// The operator folds of `ConstExprCheck` beyond `|||` / `&&&` / `^^^` and unary minus:
// arithmetic, string concatenation, shifts, `bool` and the complement, over the harness of
// `ConstExprCheckTests`.

[<Tests>]
let tests =
    testList
        "ConstExprCheck folds"
        [
            testList
                "arithmetic"
                [
                    test "+" { Expect.equal (check "1 + 2") (Ok(plainC (int32 3))) "1 + 2" }

                    test "-" { Expect.equal (check "10 - 3") (Ok(plainC (int32 7))) "10 - 3" }

                    test "*" { Expect.equal (check "3 * 4") (Ok(plainC (int32 12))) "3 * 4" }

                    test "nested arithmetic follows the parser's precedence" {
                        Expect.equal (check "1 + 2 * 3") (Ok(plainC (int32 7))) "1 + 2 * 3"
                    }

                    test "a suffixed width wraps as the unchecked runtime operator does" {
                        Expect.equal
                            (check "100y + 100y")
                            (Ok(plainC (TConstValue.Integral(IntValue.SByte -56y))))
                            "100y + 100y"
                    }

                    // fsc refuses an overflowing literal expression with FS3177; the wrapped
                    // value is what both targets compute, so it folds here.
                    test "overflow wraps rather than refusing" {
                        Expect.equal
                            (check "2147483647 + 1")
                            (Ok(plainC (int32 System.Int32.MinValue)))
                            "2147483647 + 1 is Int32.MinValue"
                    }

                    test "mismatched widths are rejected" {
                        Expect.equal (check "1 + 2L") (Error [ ConstExprCheck.Rejection.arithmeticOperands ]) "1 + 2L"
                    }

                    // `+` on an enum is undefined in fsc; `|||` is the flags combinator.
                    test "arithmetic on an enum's cases is rejected" {
                        Expect.equal
                            (checkWith localEnums "E.A + E.B")
                            (Error [ ConstExprCheck.Rejection.arithmeticOperands ])
                            "E.A + E.B"
                    }

                    test "a mixed string and integral operand is rejected" {
                        Expect.equal
                            (check "\"a\" + 1")
                            (Error [ ConstExprCheck.Rejection.arithmeticOperands ])
                            "\"a\" + 1"
                    }

                    // fsc folds these; the targets compute them at different precisions, so
                    // only the literal is representable here.
                    test "float, float32 and decimal arithmetic is deferred" {
                        for src in [ "1.0 + 2.0"; "1.5f * 2.0f"; "1.5M + 2.0M"; "1 * 2.0" ] do
                            Expect.equal (check src) (Error [ ConstExprCheck.Rejection.inexactArithmetic ]) src

                        Expect.equal (check "1.5") (Ok(plainC (TConstValue.Float 1.5))) "the literal alone still folds"
                    }

                    // Division faults at zero and overflows at `Int32.MinValue / -1`, and the
                    // targets differ on both, so neither operator is admitted.
                    test "division and modulus are not constant expressions" {
                        Expect.equal (check "10 / 2") notConstant "10 / 2"
                        Expect.equal (check "10 % 3") notConstant "10 % 3"
                    }

                    test "pointer-width arithmetic is rejected" {
                        Expect.equal
                            (check "1n + 1n")
                            (Error [ ConstExprCheck.Rejection.targetWidth ])
                            "1n + 1n: the width is the target's"

                        Expect.equal
                            (check "1n ||| 2n")
                            (Ok(plainC (TConstValue.Integral(IntValue.NativeInt 3L))))
                            "a per-bit combination reads the operands' bits alone, so it folds"
                    }
                ]

            testList
                "string concatenation"
                [
                    test "+ over two strings" {
                        Expect.equal (check "\"a\" + \"b\"") (Ok(plainC (TConstValue.String "ab"))) "\"a\" + \"b\""
                    }

                    test "+ over a literal reference" {
                        Expect.equal
                            (checkWith [ "[<Literal>]"; "let Prefix = \"pre-\"" ] "Prefix + \"fix\"")
                            (Ok(plainC (TConstValue.String "pre-fix")))
                            "Prefix + \"fix\""
                    }

                    // `Color.Red` is a string-valued enum case, so its scalar is a string while
                    // its type is the enum.
                    test "a string-valued enum case is rejected" {
                        Expect.equal
                            (check "Color.Red + \"x\"")
                            (Error [ ConstExprCheck.Rejection.arithmeticOperands ])
                            "Color.Red + \"x\""
                    }
                ]

            testList
                "shifts"
                [
                    test "<<<" { Expect.equal (check "1 <<< 4") (Ok(plainC (int32 16))) "1 <<< 4" }

                    test ">>>" { Expect.equal (check "256 >>> 4") (Ok(plainC (int32 16))) "256 >>> 4" }

                    test ">>> is arithmetic on a signed kind" {
                        Expect.equal (check "-16 >>> 2") (Ok(plainC (int32 -4))) "-16 >>> 2"
                    }

                    test ">>> is logical on an unsigned kind" {
                        Expect.equal
                            (check "2147483648u >>> 31")
                            (Ok(plainC (TConstValue.Integral(IntValue.UInt32 1u))))
                            "2147483648u >>> 31"
                    }

                    test "the shifted value keeps its own width, the count is an int" {
                        Expect.equal
                            (check "1L <<< 40")
                            (Ok(plainC (TConstValue.Integral(IntValue.Int64(1L <<< 40)))))
                            "1L <<< 40"
                    }

                    // fsc masks the count by one below the width (`1 <<< 32` is `1`); the
                    // targets mask differently, so a constant shift takes a count in range.
                    test "a count outside the width is rejected" {
                        Expect.equal (check "1 <<< 32") (Error [ ConstExprCheck.Rejection.shiftCount ]) "1 <<< 32"

                        Expect.equal
                            (check "1y <<< 8")
                            (Error [ ConstExprCheck.Rejection.shiftCount ])
                            "1y <<< 8: sbyte is 8 bits wide"

                        Expect.equal (check "1 <<< -1") (Error [ ConstExprCheck.Rejection.shiftCount ]) "1 <<< -1"
                    }

                    test "a count of another width is rejected" {
                        Expect.equal (check "1 <<< 4L") (Error [ ConstExprCheck.Rejection.shiftOperands ]) "1 <<< 4L"
                    }

                    test "an enum operand is rejected" {
                        Expect.equal
                            (checkWith localEnums "E.A <<< 1")
                            (Error [ ConstExprCheck.Rejection.shiftOperands ])
                            "E.A <<< 1"
                    }

                    test "a pointer-width operand is rejected" {
                        Expect.equal
                            (check "1n <<< 3")
                            (Error [ ConstExprCheck.Rejection.targetWidth ])
                            "1n <<< 3: the count's mask is the target's"
                    }
                ]

            testList
                "bool"
                [
                    test "&&" {
                        Expect.equal (check "true && false") (Ok(plainC (TConstValue.Bool false))) "true && false"
                        Expect.equal (check "true && true") (Ok(plainC (TConstValue.Bool true))) "true && true"
                    }

                    test "||" {
                        Expect.equal (check "true || false") (Ok(plainC (TConstValue.Bool true))) "true || false"
                    }

                    test "not" { Expect.equal (check "not true") (Ok(plainC (TConstValue.Bool false))) "not true" }

                    test "not applied without a space" {
                        Expect.equal (check "not(false)") (Ok(plainC (TConstValue.Bool true))) "not(false)"
                    }

                    test "non-bool operands are rejected" {
                        Expect.equal
                            (check "1 && true")
                            (Error [ ConstExprCheck.Rejection.logicalOperands ])
                            "1 && true"

                        Expect.equal (check "not 1") (Error [ ConstExprCheck.Rejection.logicalOperands ]) "not 1"
                    }

                    test "a shadowed not is not a constant expression" {
                        Expect.equal
                            (checkWith [ "let not (b: bool) = b" ] "not true")
                            notConstant
                            "a `let not` of the file denotes another binding"
                    }
                ]

            testList
                "complement"
                [
                    test "~~~" {
                        Expect.equal (check "~~~0") (Ok(plainC (int32 -1))) "~~~0"

                        Expect.equal
                            (check "~~~0uy")
                            (Ok(plainC (TConstValue.Integral(IntValue.Byte 255uy))))
                            "~~~0uy inverts at the byte's width"
                    }

                    // fsc answers `int` for `~~~E.A`, dropping the enum; a constant expression
                    // stays within the operand's type or refuses.
                    test "an enum operand is rejected" {
                        Expect.equal
                            (checkWith localEnums "~~~E.A")
                            (Error [ ConstExprCheck.Rejection.complementOperand ])
                            "~~~E.A"
                    }

                    test "a non-integral operand is rejected" {
                        Expect.equal (check "~~~1.5") (Error [ ConstExprCheck.Rejection.complementOperand ]) "~~~1.5"
                    }

                    test "a pointer-width operand is rejected" {
                        Expect.equal (check "~~~1n") (Error [ ConstExprCheck.Rejection.targetWidth ]) "~~~1n"
                    }
                ]
        ]
