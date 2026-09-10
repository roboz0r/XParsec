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

/// The `System.ValueTuple` family member a tuple of `arity` instantiates, capped at the
/// `ValueTuple`8` that nests the rest. Mirrors `ClrTuples.typeKey`.
let private valueTupleKey (arity: int) : TypeKey =
    SymbolKeyOps.typeKeyOfArity "System" "ValueTuple" (min arity 8)

/// The CLR backend's platform facts, which `realProvider` omits by composing over
/// `PackageProviders.noPlatformMetadata`.
let private clrPlatform: IExternalSymbolProvider =
    ExternalSymbolProviders.ofKeyIndexedChannels
        { ExternalSymbolProviders.KeyIndexedChannels.empty with
            Platform =
                ValueSome
                    { new IPlatformFacts with
                        member _.IsValueType _ = ValueNone

                        member _.TupleType arity =
                            if arity < 2 then
                                ValueNone
                            else
                                ValueSome(valueTupleKey arity)

                        member _.ConstEncoding(_, _) = ConstEncoding.Encodable
                    }
        }

let private provider: Lazy<IExternalSymbolProvider> =
    lazy ExternalSymbolProviders.composite [ realProvider.Value; published; clrPlatform ]

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

/// A `typeof<T>` / `typedefof<T>` outcome: the reified type, at `Vesper.Type`.
let private typeC (operand: FrozenType) : TConstDenotation =
    {
        Result = TConstResult.TypeVal operand
        Ty = FTConst(RuntimeNames.runtimeTypeKey, Block.empty)
    }

let private ftPrim (key: TypeKey) : FrozenType = FTConst(key, Block.empty)

/// A `[| … |]` outcome: the items' results at the array of `elemTy`.
let private arrayC (elemTy: FrozenType) (items: TConstResult list) : TConstDenotation =
    {
        Result = TConstResult.ArrayVal(Block.ofList items)
        Ty = ftArray elemTy
    }

let private scalarR (v: TConstValue) : TConstResult = TConstResult.Scalar v

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

/// Check `exprSrc` as the RHS of a `let` written below `preamble`, against `provider`, at a
/// position declaring `expected`. `Error` carries every diagnostic the check itself
/// reported, in order.
let private checkExpectingAgainst
    (provider: IExternalSymbolProvider)
    (expected: FrozenType voption)
    (preamble: string list)
    (exprSrc: string)
    : Result<TConstDenotation, Kind list> =
    let ctx, file =
        analyseNameRes provider (String.concat "\n" (preamble @ [ "let x = " + exprSrc ]))

    let b = lastBinding file
    let before = Seq.length ctx.Diagnostics

    match ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofBinding b)) expected b.expr with
    | ValueSome node -> Ok(TConstExpr.denotation node)
    | ValueNone -> Error [ for d in Seq.skip before ctx.Diagnostics -> d.Kind ]

let private checkAgainst (provider: IExternalSymbolProvider) (preamble: string list) (exprSrc: string) =
    checkExpectingAgainst provider ValueNone preamble exprSrc

let private checkWith (preamble: string list) (exprSrc: string) =
    checkAgainst provider.Value preamble exprSrc

let private check (exprSrc: string) = checkWith [] exprSrc

/// `check` at a position declaring `expected`.
let private checkExpecting (expected: FrozenType) (exprSrc: string) =
    checkExpectingAgainst provider.Value (ValueSome expected) [] exprSrc

let private notConstant = Error [ Kind.NotConstantExpression ]

let private int32 (v: int) = TConstValue.Integral(IntValue.Int32 v)

/// `E.A = 1`, `E.B = 4`, and a second enum `F.Bit = 8` at the same width.
let private localEnums = [ "type E = | A = 1 | B = 4"; "type F = | Bit = 8" ]

/// An arity-1 record, for the reified generic instantiation and its definition.
let private genericBox = [ "type Box<'T> = { v: 'T }" ]

let private boxKey = SymbolKeyOps.typeKeyOfArity "" "Box" 1
let private rKey = SymbolKeyOps.typeKeyOf "" "R"

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
                "reified types"
                [
                    test "typeof<int>" {
                        Expect.equal (check "typeof<int>") (Ok(typeC (ftPrim RuntimeNames.intKey))) "typeof<int>"
                    }

                    test "typeof<string>" {
                        Expect.equal
                            (check "typeof<string>")
                            (Ok(typeC (ftPrim RuntimeNames.stringKey)))
                            "typeof<string>"
                    }

                    test "typeof of an array" {
                        Expect.equal
                            (check "typeof<int[]>")
                            (Ok(typeC (ftArray (ftPrim RuntimeNames.intKey))))
                            "typeof<int[]>"
                    }

                    test "typeof of a local record" {
                        Expect.equal
                            (checkWith [ "type R = { a: int }" ] "typeof<R>")
                            (Ok(typeC (FTRecord(rKey, Block.empty))))
                            "typeof<R>"
                    }

                    test "typeof of a local enum" {
                        Expect.equal (checkWith localEnums "typeof<E>") (Ok(typeC (FTEnum eKey))) "typeof<E>"
                    }

                    test "typeof of a published enum" {
                        Expect.equal (check "typeof<Direction>") (Ok(typeC (FTEnum directionKey))) "typeof<Direction>"
                    }

                    test "typeof of a generic instantiation carries the argument" {
                        Expect.equal
                            (checkWith genericBox "typeof<Box<int>>")
                            (Ok(typeC (FTRecord(boxKey, Block.singleton (ftPrim RuntimeNames.intKey)))))
                            "typeof<Box<int>>"
                    }

                    // Accepted here; fsc reifies both as BCL nominals (`System.Tuple`2`,
                    // `FSharpFunc`2`).
                    test "typeof of a tuple" {
                        Expect.equal
                            (check "typeof<int * string>")
                            (Ok(
                                typeC (
                                    FTTuple(Block.ofList [ ftPrim RuntimeNames.intKey; ftPrim RuntimeNames.stringKey ])
                                )
                            ))
                            "typeof<int * string>"
                    }

                    test "typeof of a function" {
                        Expect.equal
                            (check "typeof<int -> int>")
                            (Ok(typeC (FTFun(ftPrim RuntimeNames.intKey, ftPrim RuntimeNames.intKey))))
                            "typeof<int -> int>"
                    }

                    test "typedefof drops the written type arguments" {
                        Expect.equal
                            (checkWith genericBox "typedefof<Box<int>>")
                            (Ok(typeC (FTRecord(boxKey, Block.empty))))
                            "typedefof<Box<int>>"
                    }

                    test "typedefof takes an inferred type argument, which it discards" {
                        Expect.equal
                            (checkWith genericBox "typedefof<Box<_>>")
                            (Ok(typeC (FTRecord(boxKey, Block.empty))))
                            "typedefof<Box<_>>"
                    }

                    test "typedefof of a niladic type is that type" {
                        Expect.equal (check "typedefof<int>") (Ok(typeC (ftPrim RuntimeNames.intKey))) "typedefof<int>"
                    }

                    // `int[]`'s element type is an argument of an ARITY-0 identity, so there is
                    // nothing to drop: fsc gives `System.Int32[]` too.
                    test "typedefof of an array keeps its element type" {
                        Expect.equal
                            (check "typedefof<int[]>")
                            (Ok(typeC (ftArray (ftPrim RuntimeNames.intKey))))
                            "typedefof<int[]>"
                    }

                    // The provider supplies the identity, so the answer is the target's: fsc
                    // says `System.Tuple`2`, where a Vesper tuple IS a value tuple.
                    test "typedefof of a tuple is the target's tuple identity" {
                        Expect.equal
                            (check "typedefof<int * string>")
                            (Ok(typeC (FTClass(valueTupleKey 2, Block.empty))))
                            "typedefof<int * string>"

                        Expect.equal
                            (check "typedefof<int * string * bool>")
                            (Ok(typeC (FTClass(valueTupleKey 3, Block.empty))))
                            "typedefof<int * string * bool>"
                    }

                    test "typedefof of an anonymous union is rejected" {
                        Expect.equal
                            (check "typedefof<string | null>")
                            (Error [ ConstExprCheck.Rejection.structuralDefinition ])
                            "typedefof<string | null>"
                    }

                    // The identity is the target's to supply, so a stack with no platform facts
                    // refuses.
                    test "typedefof of a tuple is rejected without platform facts" {
                        Expect.equal
                            (checkAgainst realProvider.Value [] "typedefof<int * string>")
                            (Error [ ConstExprCheck.Rejection.structuralDefinition ])
                            "typedefof<int * string>"
                    }

                    // A 9-tuple is a `ValueTuple`8` whose 8th argument nests the rest, and
                    // `typedefof` drops every argument, so the nesting stays in the encoder.
                    test "typedefof of a tuple past the family's width is the nesting member" {
                        Expect.equal
                            (check "typedefof<int * int * int * int * int * int * int * int * int>")
                            (Ok(typeC (FTClass(valueTupleKey 8, Block.empty))))
                            "typedefof of a 9-tuple"
                    }

                    // `Vesper.Fun`2` is the interface a curried value implements on both
                    // targets, so no platform fact is asked for.
                    test "typedefof of a function is the curried interface" {
                        Expect.equal
                            (check "typedefof<int -> int>")
                            (Ok(typeC (FTClass(RuntimeNames.vesperFunKey 2, Block.empty))))
                            "typedefof<int -> int>"

                        Expect.equal
                            (check "typedefof<int -> string -> bool>")
                            (Ok(typeC (FTClass(RuntimeNames.vesperFunKey 2, Block.empty))))
                            "typedefof<int -> string -> bool>"
                    }

                    // fsc reports FS3187: a reified type is ground.
                    test "a type parameter operand is rejected" {
                        Expect.equal (check "typeof<'T>") (Error [ ConstExprCheck.Rejection.typarReified ]) "typeof<'T>"
                    }

                    test "a type parameter among the arguments is rejected under typedefof too" {
                        Expect.equal
                            (checkWith genericBox "typedefof<Box<'T>>")
                            (Error [ ConstExprCheck.Rejection.typarReified ])
                            "typedefof<Box<'T>>"
                    }

                    // fsc infers the hole (`typeof<list<_>>` is `list<obj>`); a constant
                    // position rejects it instead.
                    test "an inferred type argument is rejected under typeof" {
                        Expect.equal
                            (checkWith genericBox "typeof<Box<_>>")
                            (Error [ ConstExprCheck.Rejection.inferredReified ])
                            "typeof<Box<_>>"
                    }

                    test "a bare hole is rejected under typedefof" {
                        Expect.equal
                            (check "typedefof<_>")
                            (Error [ ConstExprCheck.Rejection.inferredReified ])
                            "typedefof<_>"
                    }

                    // The array identity has arity 0, so its element type survives `typedefof`
                    // and a hole there is read.
                    test "a hole in an array element is rejected under typedefof" {
                        Expect.equal
                            (check "typedefof<_[]>")
                            (Error [ ConstExprCheck.Rejection.inferredReified ])
                            "typedefof<_[]>"
                    }

                    // A structural form's elements are arguments of the identity it takes, and
                    // `typedefof` drops them before the type is read, as it does for `Box<_>`.
                    test "a hole in a structural form is dropped with the arguments" {
                        Expect.equal
                            (check "typedefof<_ * int>")
                            (Ok(typeC (FTClass(valueTupleKey 2, Block.empty))))
                            "typedefof<_ * int>"

                        Expect.equal
                            (check "typedefof<(_ -> int)>")
                            (Ok(typeC (FTClass(RuntimeNames.vesperFunKey 2, Block.empty))))
                            "typedefof<(_ -> int)>"

                        Expect.equal
                            (checkWith genericBox "typedefof<Box<_> * int>")
                            (Ok(typeC (FTClass(valueTupleKey 2, Block.empty))))
                            "typedefof<Box<_> * int>"
                    }

                    // Name resolution reports the written arity (FS0033) before the check runs,
                    // and the translation's recovery carries that reason, so the check refuses
                    // silently under either spelling.
                    test "a generic name written without its arguments is reported once" {
                        for src in [ "typeof<Box>"; "typedefof<Box>" ] do
                            let ctx, file =
                                analyseNameRes provider.Value ("type Box<'T> = { v: 'T }\nlet x = " + src)

                            let b = lastBinding file

                            Expect.isTrue
                                (ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofBinding b)) ValueNone b.expr).IsNone
                                (src + " is not a constant expression")

                            Expect.equal
                                [ for d in ctx.Diagnostics -> d.Kind ]
                                [ Kind.TypeArgArity("Box", 1, 0) ]
                                (src + ": one diagnostic, at the written name")
                    }

                    // Name resolution reports the written name (FS0039) before the check runs,
                    // so the check refuses without adding a second diagnostic.
                    test "an undefined type is reported once" {
                        let ctx, file = analyseNameRes provider.Value "let x = typeof<Nope>"
                        let b = lastBinding file

                        Expect.isTrue
                            (ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofBinding b)) ValueNone b.expr).IsNone
                            "typeof<Nope> is not a constant expression"

                        Expect.equal
                            [ for d in ctx.Diagnostics -> d.Kind ]
                            [ Kind.UndefinedType "Nope" ]
                            "one diagnostic, at the written name"
                    }

                    test "a shadowed typeof is not a constant expression" {
                        Expect.equal
                            (checkWith [ "let typeof (v: int) = v" ] "typeof<int>")
                            notConstant
                            "a `let typeof` of the file denotes another binding"
                    }

                    test "a reification with two type arguments is not a constant expression" {
                        Expect.equal (check "typeof<int, string>") notConstant "typeof<int, string>"
                    }
                ]

            testList
                "array literals"
                [
                    test "two ints" {
                        Expect.equal
                            (check "[| 1; 2 |]")
                            (Ok(arrayC (ftPrim RuntimeNames.intKey) [ scalarR (int32 1); scalarR (int32 2) ]))
                            "[| 1; 2 |]"
                    }

                    test "one string" {
                        Expect.equal
                            (check "[| \"a\" |]")
                            (Ok(arrayC (ftPrim RuntimeNames.stringKey) [ scalarR (TConstValue.String "a") ]))
                            "[| \"a\" |]"
                    }

                    test "enum cases and a mask keep the enum's key" {
                        Expect.equal
                            (checkWith localEnums "[| E.A; E.A ||| E.B |]")
                            (Ok(arrayC (FTEnum eKey) [ scalarR (int32 1); scalarR (int32 5) ]))
                            "[| E.A; E.A ||| E.B |]"
                    }

                    test "reified types" {
                        Expect.equal
                            (check "[| typeof<int>; typeof<string> |]")
                            (Ok(
                                arrayC
                                    (ftPrim RuntimeNames.runtimeTypeKey)
                                    [
                                        TConstResult.TypeVal(ftPrim RuntimeNames.intKey)
                                        TConstResult.TypeVal(ftPrim RuntimeNames.stringKey)
                                    ]
                            ))
                            "[| typeof<int>; typeof<string> |]"
                    }

                    test "byte items are accepted where fsc refuses byte[]" {
                        Expect.equal
                            (check "[| 1uy; 2uy |]")
                            (Ok(
                                arrayC
                                    (ftPrim RuntimeNames.byteKey)
                                    [
                                        scalarR (TConstValue.Integral(IntValue.Byte 1uy))
                                        scalarR (TConstValue.Integral(IntValue.Byte 2uy))
                                    ]
                            ))
                            "a byte[] literal is a target-neutral value; the FS0267 fsc reports is a CLR-blob gate"
                    }

                    test "a differing item is rejected at the item" {
                        Expect.equal
                            (check "[| 1; 2L |]")
                            (Error [ ConstExprCheck.Rejection.arrayItemType ])
                            "[| 1; 2L |] is FS0267 at 2L"
                    }

                    test "every differing item reports" {
                        Expect.equal
                            (check "[| 1; 2L; 3L |]")
                            (Error
                                [
                                    ConstExprCheck.Rejection.arrayItemType
                                    ConstExprCheck.Rejection.arrayItemType
                                ])
                            "[| 1; 2L; 3L |]"
                    }

                    test "a nested array is rejected at the inner array" {
                        Expect.equal
                            (check "[| [| 1 |] |]")
                            (Error [ ConstExprCheck.Rejection.nestedArray ])
                            "[| [| 1 |] |] is FS0267 at the inner [|"
                    }

                    test "a parenthesised nested array is rejected" {
                        Expect.equal
                            (check "[| ([| 1 |]) |]")
                            (Error [ ConstExprCheck.Rejection.nestedArray ])
                            "[| ([| 1 |]) |]"
                    }

                    test "a nested empty array is rejected as nested" {
                        Expect.equal (check "[| [||] |]") (Error [ ConstExprCheck.Rejection.nestedArray ]) "[| [||] |]"

                        Expect.equal
                            (check "[| ([||]) |]")
                            (Error [ ConstExprCheck.Rejection.nestedArray ])
                            "grouping parens do not turn the nested [||] into the empty-array refusal"
                    }

                    test "a nested array is rejected as nested before its items are checked" {
                        Expect.equal
                            (check "[| [| 1; 2L |] |]")
                            (Error [ ConstExprCheck.Rejection.nestedArray ])
                            "the inner mismatch at 2L is unreported: the inner [| is the error"
                    }

                    test "a rejected item reports once" {
                        Expect.equal (check "[| 1; id 2 |]") notConstant "[| 1; id 2 |]"
                    }

                    test "an empty array takes the position's array type and is refused without one" {
                        Expect.equal (check "[||]") notConstant "[||] with no expected type is FS0267"

                        Expect.equal
                            (checkExpecting (ftArray (ftPrim RuntimeNames.intKey)) "[||]")
                            (Ok(arrayC (ftPrim RuntimeNames.intKey) []))
                            "[||] against int[] is the empty int[]"

                        Expect.equal
                            (checkExpecting (ftPrim RuntimeNames.objKey) "[||]")
                            notConstant
                            "[||] against obj has no element type to take"
                    }

                    test "an expected element type governs the items" {
                        Expect.equal
                            (checkExpecting (ftArray (ftPrim RuntimeNames.int64Key)) "[| 1L; 2L |]")
                            (Ok(
                                arrayC
                                    (ftPrim RuntimeNames.int64Key)
                                    [
                                        scalarR (TConstValue.Integral(IntValue.Int64 1L))
                                        scalarR (TConstValue.Integral(IntValue.Int64 2L))
                                    ]
                            ))
                            "[| 1L; 2L |] against int64[]"

                        Expect.equal
                            (checkExpecting (ftArray (ftPrim RuntimeNames.int64Key)) "[| 1; 2 |]")
                            (Error
                                [
                                    ConstExprCheck.Rejection.arrayItemType
                                    ConstExprCheck.Rejection.arrayItemType
                                ])
                            "[| 1; 2 |] against int64[] is FS0267 at each item: a literal keeps its own type"
                    }

                    test "an obj element type admits any constant and null" {
                        Expect.equal
                            (checkExpecting (ftArray (ftPrim RuntimeNames.objKey)) "[| 1; \"a\"; null; typeof<int> |]")
                            (Ok(
                                arrayC
                                    (ftPrim RuntimeNames.objKey)
                                    [
                                        scalarR (int32 1)
                                        scalarR (TConstValue.String "a")
                                        TConstResult.Null
                                        TConstResult.TypeVal(ftPrim RuntimeNames.intKey)
                                    ]
                            ))
                            "[| 1; \"a\"; null; typeof<int> |] against obj[]"
                    }

                    test "a list literal is not a constant expression" {
                        Expect.equal (check "[1]") notConstant "[1] is not the constant 1"
                        Expect.equal (check "[1; 2]") notConstant "[1; 2]"
                    }
                ]

            testList
                "null"
                [
                    test "null takes the position's type" {
                        Expect.equal
                            (checkExpecting (ftPrim RuntimeNames.stringKey) "null")
                            (Ok
                                {
                                    Result = TConstResult.Null
                                    Ty = ftPrim RuntimeNames.stringKey
                                })
                            "null against string"

                        Expect.equal
                            (checkExpecting (ftArray (ftPrim RuntimeNames.intKey)) "(null)")
                            (Ok
                                {
                                    Result = TConstResult.Null
                                    Ty = ftArray (ftPrim RuntimeNames.intKey)
                                })
                            "null against int[], through grouping parens"

                        Expect.equal
                            (checkExpecting (ftPrim RuntimeNames.objKey) "null")
                            (Ok
                                {
                                    Result = TConstResult.Null
                                    Ty = ftPrim RuntimeNames.objKey
                                })
                            "null against obj"
                    }

                    test "null is refused without a position and against a type with no null value" {
                        Expect.equal (check "null") notConstant "null with no expected type is FS0267"

                        Expect.equal
                            (checkExpecting (ftPrim RuntimeNames.intKey) "null")
                            (Error [ Kind.NullNotProperValue "int" ])
                            "null against int is FS0043"
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
