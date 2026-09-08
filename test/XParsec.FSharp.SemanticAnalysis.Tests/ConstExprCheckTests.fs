module XParsec.FSharp.SemanticAnalysis.Tests.ConstExprCheckTests

open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `ConstExprCheck.check` over the last `let` of a file analysed through NameResolution
// against the real `Vesper.Core` contract: the intrinsic operators, a `[<Literal>]` declared
// above, a local enum, and a published one.

/// `Tests.Direction` (`Up = 3L`, at `int64`) and `Tests.Color` (`Red = "red"`, a string
/// enum) published beside `Vesper.Core`, in the ambient namespace `Tests`.
let private published: IExternalSymbolProvider =
    let enum (underlying: TypeKey) (cases: (string * ExternalEnumCaseValue) list) =
        ExternalTypeShape.Enum
            {
                Cases =
                    cases
                    |> List.map (fun (n, v) -> { Name = n; Value = v }: ExternalEnumCaseShape)
                    |> Block.ofList
                Underlying = underlying
                Origin = SymbolOrigin.Empty
            }

    providerOfSurface (fun b ->
        PublishedSurfaceBuilder.addType
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Direction" 0)
            (enum RuntimeNames.int64Key [ "Up", ExternalEnumCaseValue.IntVal(IntValue.Int64 3L) ])

        PublishedSurfaceBuilder.addType
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Color" 0)
            (enum RuntimeNames.stringKey [ "Red", ExternalEnumCaseValue.StringVal "red" ])

        b.ImplicitOpens <- [ SymbolKeyOps.assemblyAutoOpen "Tests" ]
    )

let private provider: Lazy<IExternalSymbolProvider> =
    lazy ExternalSymbolProviders.composite [ realProvider.Value; published ]

let private eKey = SymbolKeyOps.typeKeyOf "" "E"
let private directionKey = SymbolKeyOps.qualifiedTypeKeyOf "Tests.Direction" 0
let private colorKey = SymbolKeyOps.qualifiedTypeKeyOf "Tests.Color" 0

/// A check's outcome by what it denotes: two trees carrying equal values differ by their
/// sites.
let private plainC (v: TConstValue) : TConstDenotation =
    {
        Result = TConstResult.Scalar v
        Ty = FTConst(TConstValue.canonKey v, Block.empty)
    }

let private enumC (key: TypeKey) (v: TConstValue) : TConstDenotation =
    {
        Result = TConstResult.Scalar v
        Ty = FTEnum key
    }

/// The file's last top-level `let` binding.
let private lastBinding (file: ImplementationFile<SyntaxToken>) : Binding<SyntaxToken> =
    CstModuleTree.implFileElems file
    |> Seq.choose (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[bindings.Length - 1]
        | _ -> None
    )
    |> Seq.last

/// Check `exprSrc` as the RHS of a `let` written below `preamble`, against `provider`.
/// `Error` carries every diagnostic the check itself reported, in order.
let private checkAgainst
    (provider: IExternalSymbolProvider)
    (preamble: string list)
    (exprSrc: string)
    : Result<TConstDenotation, Kind list> =
    let ctx, file =
        analyseNameRes provider (String.concat "\n" (preamble @ [ "let x = " + exprSrc ]))

    let b = lastBinding file
    let before = Seq.length ctx.Diagnostics

    match ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofBinding b)) b.expr with
    | ValueSome node -> Ok(TConstExpr.denotation node)
    | ValueNone -> Error [ for d in Seq.skip before ctx.Diagnostics -> d.Kind ]

let private checkWith (preamble: string list) (exprSrc: string) =
    checkAgainst provider.Value preamble exprSrc

let private check (exprSrc: string) = checkWith [] exprSrc

let private notConstant = Error [ Kind.NotConstantExpression ]

let private int32 (v: int) = TConstValue.Integral(IntValue.Int32 v)

/// `E.A = 1`, `E.B = 4`, and a second enum `F.Bit = 8` at the same width.
let private localEnums = [ "type E = | A = 1 | B = 4"; "type F = | Bit = 8" ]

/// A bare and a module-qualified `[<Literal>]`.
let private literals =
    [
        "[<Literal>]"
        "let MASK = 3"
        "module M ="
        "    [<Literal>]"
        "    let Bit = 8"
    ]

[<Tests>]
let tests =
    testList
        "ConstExprCheck"
        [
            testList
                "literals"
                [
                    test "unsuffixed int" { Expect.equal (check "1") (Ok(plainC (int32 1))) "1" }

                    test "suffixed byte" {
                        Expect.equal (check "255uy") (Ok(plainC (TConstValue.Integral(IntValue.Byte 255uy)))) "255uy"
                    }

                    test "int64" {
                        Expect.equal (check "5L") (Ok(plainC (TConstValue.Integral(IntValue.Int64 5L)))) "5L"
                    }

                    test "float" { Expect.equal (check "1.5") (Ok(plainC (TConstValue.Float 1.5))) "1.5" }

                    test "float32" { Expect.equal (check "1.5f") (Ok(plainC (TConstValue.Float32 1.5f))) "1.5f" }

                    test "decimal" { Expect.equal (check "2.5m") (Ok(plainC (TConstValue.Decimal 2.5m))) "2.5m" }

                    test "string" { Expect.equal (check "\"abc\"") (Ok(plainC (TConstValue.String "abc"))) "\"abc\"" }

                    test "verbatim string" {
                        Expect.equal (check "@\"a\\b\"") (Ok(plainC (TConstValue.String "a\\b"))) "@\"a\\b\""
                    }

                    test "char" { Expect.equal (check "'c'") (Ok(plainC (TConstValue.Char 'c'))) "'c'" }

                    test "escaped char" { Expect.equal (check "'\\n'") (Ok(plainC (TConstValue.Char '\n'))) "'\\n'" }

                    test "bool true" { Expect.equal (check "true") (Ok(plainC (TConstValue.Bool true))) "true" }

                    test "bool false" { Expect.equal (check "false") (Ok(plainC (TConstValue.Bool false))) "false" }

                    test "unit" { Expect.equal (check "()") (Ok(plainC TConstValue.Unit)) "()" }

                    test "custom numeric literal is rejected" {
                        Expect.equal (check "52I") (Error [ ConstExprCheck.Rejection.customLiteral ]) "52I"
                    }

                    test "out-of-range literal is rejected" {
                        Expect.equal (check "300uy") (Error [ ConstExprCheck.Rejection.outOfRange ]) "300uy"
                    }

                    test "interpolated string is rejected" {
                        Expect.equal (check "$\"a\"") (Error [ ConstExprCheck.Rejection.interpolatedString ]) "$\"a\""
                    }
                ]

            testList
                "enum references"
                [
                    test "a local int case checks to its underlying constant with its enum's key" {
                        Expect.equal (checkWith localEnums "E.A") (Ok(enumC eKey (int32 1))) "E.A"
                    }

                    test "a published int case checks at its declared width" {
                        Expect.equal
                            (check "Direction.Up")
                            (Ok(enumC directionKey (TConstValue.Integral(IntValue.Int64 3L))))
                            "Direction.Up"
                    }

                    test "a published string case checks to a string" {
                        Expect.equal (check "Color.Red") (Ok(enumC colorKey (TConstValue.String "red"))) "Color.Red"
                    }

                    test "an unresolved long ident is rejected" {
                        Expect.equal (checkWith localEnums "E.Missing") notConstant "E.Missing"
                    }
                ]

            testList
                "literal-value references"
                [
                    test "a bare literal checks to its constant" {
                        Expect.equal (checkWith literals "MASK") (Ok(plainC (int32 3))) "MASK"
                    }

                    test "a qualified literal checks to its constant" {
                        Expect.equal (checkWith literals "M.Bit") (Ok(plainC (int32 8))) "M.Bit"
                    }

                    test "||| over literal references" {
                        Expect.equal (checkWith literals "MASK ||| M.Bit") (Ok(plainC (int32 11))) "MASK ||| M.Bit"
                    }

                    test "a non-literal value is rejected" {
                        Expect.equal (checkWith [ "let v = 3" ] "v") notConstant "v"
                    }

                    test "an unresolved bare ident is rejected" {
                        Expect.equal (checkWith literals "Nope") notConstant "Nope"
                    }
                ]

            testList
                "bitwise"
                [
                    test "|||" { Expect.equal (check "1 ||| 2") (Ok(plainC (int32 3))) "1 ||| 2" }

                    test "&&&" { Expect.equal (check "3 &&& 2") (Ok(plainC (int32 2))) "3 &&& 2" }

                    test "^^^" { Expect.equal (check "3 ^^^ 1") (Ok(plainC (int32 2))) "3 ^^^ 1" }

                    test "||| over one enum's cases keeps the enum's key" {
                        Expect.equal (checkWith localEnums "E.A ||| E.B") (Ok(enumC eKey (int32 5))) "E.A ||| E.B"
                    }

                    // fsc reports FS0001 for both: `|||` takes two operands of one type.
                    test "||| across two enums is rejected" {
                        Expect.equal
                            (checkWith localEnums "E.A ||| F.Bit")
                            (Error [ ConstExprCheck.Rejection.kindMismatch ])
                            "E.A ||| F.Bit"
                    }

                    test "||| of an enum case and a bare literal is rejected" {
                        Expect.equal
                            (checkWith localEnums "E.A ||| 2")
                            (Error [ ConstExprCheck.Rejection.kindMismatch ])
                            "E.A ||| 2"
                    }

                    test "nested ops" { Expect.equal (check "1 ||| 2 ||| 4") (Ok(plainC (int32 7))) "1 ||| 2 ||| 4" }

                    test "mismatched widths are rejected" {
                        Expect.equal (check "1 ||| 2L") (Error [ ConstExprCheck.Rejection.kindMismatch ]) "1 ||| 2L"
                    }

                    test "signedness mismatch is rejected" {
                        Expect.equal (check "1 ||| 2u") (Error [ ConstExprCheck.Rejection.kindMismatch ]) "1 ||| 2u"
                    }

                    test "non-integral operands are rejected" {
                        Expect.equal
                            (check "\"a\" ||| \"b\"")
                            (Error [ ConstExprCheck.Rejection.kindMismatch ])
                            "\"a\" ||| \"b\""
                    }

                    test "an operand's rejection propagates, reported once" {
                        Expect.equal (check "52I ||| 2") (Error [ ConstExprCheck.Rejection.customLiteral ]) "52I ||| 2"
                    }
                ]

            testList
                "enum conversions"
                [
                    test "enum<E> n carries the named enum's type" {
                        Expect.equal (checkWith localEnums "enum<E> 1") (Ok(enumC eKey (int32 1))) "enum<E> 1"
                    }

                    test "LanguagePrimitives.EnumOfValue takes the enum from its second type argument" {
                        Expect.equal
                            (checkWith localEnums "LanguagePrimitives.EnumOfValue<int, E> 12")
                            (Ok(enumC eKey (int32 12)))
                            "EnumOfValue<int, E> 12"
                    }

                    test "a published enum converts too" {
                        Expect.equal (check "enum<Direction> 7") (Ok(enumC directionKey (int32 7))) "enum<Direction> 7"
                    }

                    test "the operand may be any constant expression" {
                        Expect.equal
                            (checkWith localEnums "enum<E> (1 ||| 2)")
                            (Ok(enumC eKey (int32 3)))
                            "enum<E> (1 ||| 2)"
                    }

                    test "a type argument that is no enum is not a constant expression" {
                        Expect.equal (check "enum<Nope> 1") notConstant "enum<Nope> 1"
                    }

                    test "a type argument that is a non-enum type is not a constant expression" {
                        Expect.equal (check "enum<int> 1") notConstant "enum<int> 1"
                    }

                    test "a non-integral operand is rejected" {
                        Expect.equal
                            (checkWith localEnums "enum<E> \"x\"")
                            (Error [ ConstExprCheck.Rejection.enumOperand ])
                            "enum<E> \"x\""
                    }

                    test "a shadowed enum is not a constant expression" {
                        Expect.equal
                            (checkWith (localEnums @ [ "let enum (v: int) = v" ]) "enum<E> 1")
                            notConstant
                            "a `let enum` of the file denotes another binding"
                    }
                ]

            testList
                "operator resolution"
                [
                    test "a shadowed ||| is not a constant expression" {
                        Expect.equal (checkWith [ "let (|||) (a: int) (b: int) = 999" ] "1 ||| 2") notConstant "1 ||| 2"
                    }

                    test "an out-of-scope ||| is not a constant expression" {
                        Expect.equal
                            (checkAgainst published [] "1 ||| 2")
                            notConstant
                            "fsc answers FS0043 for an operator the position cannot resolve"
                    }

                    test "a shadowed unary minus is not a constant expression" {
                        Expect.equal (checkWith [ "let (~-) (a: int) = a" ] "- 1") notConstant "- 1"
                    }

                    test "a shadowed unary minus leaves the MERGED negative literal alone" {
                        Expect.equal
                            (checkWith [ "let (~-) (a: int) = a" ] "-1")
                            (Ok(plainC (int32 -1)))
                            "the lexer merged `-1` into one literal, so no operator applies"
                    }
                ]

            testList
                "unary minus"
                [
                    test "merged negative literal" { Expect.equal (check "-1") (Ok(plainC (int32 -1))) "-1" }

                    test "spaced negation" { Expect.equal (check "- 1") (Ok(plainC (int32 -1))) "- 1" }

                    test "int min value" {
                        Expect.equal (check "-2147483648") (Ok(plainC (int32 System.Int32.MinValue))) "-2147483648"
                    }

                    test "negation wraps at the width" {
                        Expect.equal
                            (check "-(-128y)")
                            (Ok(plainC (TConstValue.Integral(IntValue.SByte(-128y)))))
                            "-(-128y) stays -128y"
                    }

                    test "negated float" { Expect.equal (check "- 1.5") (Ok(plainC (TConstValue.Float -1.5))) "- 1.5" }

                    test "negated unsigned is rejected" {
                        Expect.equal (check "- 1u") (Error [ ConstExprCheck.Rejection.negativeUnsigned ]) "- 1u"
                    }

                    test "negated string is rejected" { Expect.equal (check "- \"a\"") notConstant "- \"a\"" }
                ]

            testList
                "grouping"
                [
                    test "parens peel" { Expect.equal (check "(1)") (Ok(plainC (int32 1))) "(1)" }

                    test "nested parens peel" {
                        Expect.equal (check "((1 ||| (2)))") (Ok(plainC (int32 3))) "((1 ||| (2)))"
                    }
                ]

            testList
                "outside the domain"
                [
                    test "a function call is rejected" { Expect.equal (check "id 1") notConstant "id 1" }

                    test "arithmetic is rejected" { Expect.equal (check "1 + 1") notConstant "1 + 1" }

                    test "string concatenation is rejected" {
                        Expect.equal (check "\"a\" + \"b\"") notConstant "\"a\" + \"b\""
                    }
                ]
        ]
