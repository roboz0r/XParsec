module XParsec.FSharp.SemanticAnalysis.Tests.ConstFoldTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `ConstFold.tryConstant` over parsed expressions, with the named-constant lookup stubbed
// by dotted spelling: resolution is the caller's job, so no pipeline runs here.

/// Fold `exprSrc`, resolving a named-constant reference through `named` keyed by the
/// reference's dotted spelling.
let private foldWith (named: string -> FoldedConst voption) (exprSrc: string) =
    let lexed, file = parseFile ("let x = " + exprSrc)
    let nameOf = SyntaxToken.nameIn lexed

    let tryNamedConstant (idents: System.Collections.Immutable.ImmutableArray<SyntaxToken>) =
        named (idents |> Seq.map nameOf |> String.concat ".")

    ConstFold.tryConstant nameOf (fun _ _ -> ()) tryNamedConstant (firstBindingExpr file)

let private fold (exprSrc: string) = foldWith (fun _ -> ValueNone) exprSrc

let private int32 (v: int) = TConstValue.Integral(IntValue.Int32 v)

let private plainC (v: TConstValue) : FoldedConst = { Value = v; EnumKey = ValueNone }

let private enumC (key: TypeKey) (v: TConstValue) : FoldedConst = { Value = v; EnumKey = ValueSome key }

let private eKey = SymbolKeyOps.typeKeyOf "Test" "E"
let private directionKey = SymbolKeyOps.typeKeyOf "Test" "Direction"
let private colorKey = SymbolKeyOps.typeKeyOf "Test" "Color"

/// `AttributeTargets`-style local cases, as the registry serves them: `E.A = 1`, `E.B = 4`.
let private localEnum (spelling: string) : FoldedConst voption =
    match spelling with
    | "E.A" -> ValueSome(enumC eKey (int32 1))
    | "E.B" -> ValueSome(enumC eKey (int32 4))
    | _ -> ValueNone

/// External cases at the width the caller picked, a string case as a string literal.
let private externalEnum (spelling: string) : FoldedConst voption =
    match spelling with
    | "Direction.Up" -> ValueSome(enumC directionKey (TConstValue.Integral(IntValue.Int64 3L)))
    | "Color.Red" -> ValueSome(enumC colorKey (TConstValue.String "red"))
    // Same width as `E.*`, a different enum: for the mixed-identity test.
    | "E.A" -> ValueSome(enumC eKey (int32 1))
    | "Other.Bit" -> ValueSome(enumC colorKey (int32 8))
    | _ -> ValueNone

/// `[<Literal>]` values as the caller serves them: a bare and a module-qualified spelling.
let private literals (spelling: string) : FoldedConst voption =
    match spelling with
    | "MASK" -> ValueSome(plainC (int32 3))
    | "M.Bit" -> ValueSome(plainC (int32 8))
    | _ -> ValueNone

[<Tests>]
let tests =
    testList
        "ConstFold"
        [
            testList
                "literals"
                [
                    test "unsuffixed int" { Expect.equal (fold "1") (Ok(plainC (int32 1))) "1" }

                    test "suffixed byte" {
                        Expect.equal (fold "255uy") (Ok(plainC (TConstValue.Integral(IntValue.Byte 255uy)))) "255uy"
                    }

                    test "int64" {
                        Expect.equal (fold "5L") (Ok(plainC (TConstValue.Integral(IntValue.Int64 5L)))) "5L"
                    }

                    test "float" { Expect.equal (fold "1.5") (Ok(plainC (TConstValue.Float 1.5))) "1.5" }

                    test "float32" { Expect.equal (fold "1.5f") (Ok(plainC (TConstValue.Float32 1.5f))) "1.5f" }

                    test "decimal" { Expect.equal (fold "2.5m") (Ok(plainC (TConstValue.Decimal 2.5m))) "2.5m" }

                    test "string" { Expect.equal (fold "\"abc\"") (Ok(plainC (TConstValue.String "abc"))) "\"abc\"" }

                    test "verbatim string" {
                        Expect.equal (fold "@\"a\\b\"") (Ok(plainC (TConstValue.String "a\\b"))) "@\"a\\b\""
                    }

                    test "char" { Expect.equal (fold "'c'") (Ok(plainC (TConstValue.Char 'c'))) "'c'" }

                    test "escaped char" { Expect.equal (fold "'\\n'") (Ok(plainC (TConstValue.Char '\n'))) "'\\n'" }

                    test "bool true" { Expect.equal (fold "true") (Ok(plainC (TConstValue.Bool true))) "true" }

                    test "bool false" { Expect.equal (fold "false") (Ok(plainC (TConstValue.Bool false))) "false" }

                    test "unit" { Expect.equal (fold "()") (Ok(plainC TConstValue.Unit)) "()" }

                    test "custom numeric literal is rejected" {
                        Expect.equal (fold "52I") (Error ConstRejection.CustomLiteral) "52I"
                    }

                    test "out-of-range literal is rejected" {
                        Expect.equal (fold "300uy") (Error ConstRejection.OutOfRange) "300uy"
                    }

                    test "interpolated string is rejected" {
                        Expect.equal (fold "$\"a\"") (Error ConstRejection.InterpolatedString) "$\"a\""
                    }
                ]

            testList
                "enum references"
                [
                    test "a local int case folds to its underlying constant with its enum's key" {
                        Expect.equal (foldWith localEnum "E.A") (Ok(enumC eKey (int32 1))) "E.A"
                    }

                    test "an external int case folds at the caller's width" {
                        Expect.equal
                            (foldWith externalEnum "Direction.Up")
                            (Ok(enumC directionKey (TConstValue.Integral(IntValue.Int64 3L))))
                            "Direction.Up"
                    }

                    test "an external string case folds to a string" {
                        Expect.equal
                            (foldWith externalEnum "Color.Red")
                            (Ok(enumC colorKey (TConstValue.String "red")))
                            "Color.Red"
                    }

                    test "an unresolved long ident is rejected" {
                        Expect.equal (foldWith localEnum "E.Missing") (Error ConstRejection.NotConstant) "E.Missing"
                    }
                ]

            testList
                "literal-value references"
                [
                    test "a bare literal folds to its constant" {
                        Expect.equal (foldWith literals "MASK") (Ok(plainC (int32 3))) "MASK"
                    }

                    test "a qualified literal folds to its constant" {
                        Expect.equal (foldWith literals "M.Bit") (Ok(plainC (int32 8))) "M.Bit"
                    }

                    test "||| over literal references" {
                        Expect.equal (foldWith literals "MASK ||| M.Bit") (Ok(plainC (int32 11))) "MASK ||| M.Bit"
                    }

                    test "an unresolved bare ident is rejected" {
                        Expect.equal (foldWith literals "Nope") (Error ConstRejection.NotConstant) "Nope"
                    }
                ]

            testList
                "bitwise"
                [
                    test "|||" { Expect.equal (fold "1 ||| 2") (Ok(plainC (int32 3))) "1 ||| 2" }

                    test "&&&" { Expect.equal (fold "3 &&& 2") (Ok(plainC (int32 2))) "3 &&& 2" }

                    test "^^^" { Expect.equal (fold "3 ^^^ 1") (Ok(plainC (int32 2))) "3 ^^^ 1" }

                    test "||| over one enum's cases keeps the enum's key" {
                        Expect.equal (foldWith localEnum "E.A ||| E.B") (Ok(enumC eKey (int32 5))) "E.A ||| E.B"
                    }

                    test "||| across two enums drops the identity" {
                        Expect.equal
                            (foldWith externalEnum "E.A ||| Other.Bit")
                            (Ok(plainC (int32 9)))
                            "E.A ||| Other.Bit"
                    }

                    test "||| of an enum case and a bare literal drops the identity" {
                        Expect.equal (foldWith localEnum "E.A ||| 2") (Ok(plainC (int32 3))) "E.A ||| 2"
                    }

                    test "nested ops" { Expect.equal (fold "1 ||| 2 ||| 4") (Ok(plainC (int32 7))) "1 ||| 2 ||| 4" }

                    test "mismatched widths are rejected" {
                        Expect.equal (fold "1 ||| 2L") (Error ConstRejection.KindMismatch) "1 ||| 2L"
                    }

                    test "signedness mismatch is rejected" {
                        Expect.equal (fold "1 ||| 2u") (Error ConstRejection.KindMismatch) "1 ||| 2u"
                    }

                    test "non-integral operands are rejected" {
                        Expect.equal (fold "\"a\" ||| \"b\"") (Error ConstRejection.KindMismatch) "\"a\" ||| \"b\""
                    }

                    test "an operand's rejection propagates" {
                        Expect.equal (fold "52I ||| 2") (Error ConstRejection.CustomLiteral) "52I ||| 2"
                    }
                ]

            testList
                "unary minus"
                [
                    test "merged negative literal" { Expect.equal (fold "-1") (Ok(plainC (int32 -1))) "-1" }

                    test "spaced negation" { Expect.equal (fold "- 1") (Ok(plainC (int32 -1))) "- 1" }

                    test "int min value" {
                        Expect.equal (fold "-2147483648") (Ok(plainC (int32 System.Int32.MinValue))) "-2147483648"
                    }

                    test "negation wraps at the width" {
                        Expect.equal
                            (fold "-(-128y)")
                            (Ok(plainC (TConstValue.Integral(IntValue.SByte(-128y)))))
                            "-(-128y) stays -128y"
                    }

                    test "negated float" { Expect.equal (fold "- 1.5") (Ok(plainC (TConstValue.Float -1.5))) "- 1.5" }

                    test "negated unsigned is rejected" {
                        Expect.equal (fold "- 1u") (Error ConstRejection.NegativeUnsigned) "- 1u"
                    }

                    test "negated string is rejected" {
                        Expect.equal (fold "- \"a\"") (Error ConstRejection.NotConstant) "- \"a\""
                    }
                ]

            testList
                "grouping"
                [
                    test "parens peel" { Expect.equal (fold "(1)") (Ok(plainC (int32 1))) "(1)" }

                    test "nested parens peel" {
                        Expect.equal (fold "((1 ||| (2)))") (Ok(plainC (int32 3))) "((1 ||| (2)))"
                    }
                ]

            testList
                "outside the domain"
                [
                    test "a function call is rejected" {
                        Expect.equal (fold "id 1") (Error ConstRejection.NotConstant) "id 1"
                    }

                    test "arithmetic is rejected" {
                        Expect.equal (fold "1 + 1") (Error ConstRejection.NotConstant) "1 + 1"
                    }

                    test "string concatenation is rejected" {
                        Expect.equal (fold "\"a\" + \"b\"") (Error ConstRejection.NotConstant) "\"a\" + \"b\""
                    }
                ]
        ]
