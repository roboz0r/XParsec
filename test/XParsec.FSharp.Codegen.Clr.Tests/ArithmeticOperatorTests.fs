module XParsec.FSharp.Codegen.Clr.Tests.ArithmeticOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `let inline (+) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (+) …))` in
// `ops-platform.clr.fs` is a bare trait call, so the types DECLARING the member are the
// supported ones and any other operand is a compile error, never IL over a fallback.

/// Names `pick` reads off the analysed tree, outermost first. Annotated parameters need
/// no literal, so this reaches widths the front end cannot yet write a literal for
/// (`uint64` / `nativeint` fold to a `TConstValue.Int`; `-1y` does not project at all).
let private splicedNames (pick: TExpr -> string voption) (src: string) : string list =
    let tast = analyse src

    Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) (sprintf "no errors for: %s" src)

    let acc = ResizeArray<string>()

    let it =
        { TastWalk.identityIter with
            VisitExpr =
                fun _ e ->
                    match pick e with
                    | ValueSome name -> acc.Add name
                    | ValueNone -> ()

                    true
        }

    // Walks the specialization entries as well as the decls: a resolved operator body
    // sits in the specialization table, with only an edge left in the consuming tree.
    iterFileExprs it tast

    List.ofSeq acc

/// The inline-IL opcodes the spliced body leaves behind. For a width that IS a CIL
/// primitive these are its whole body: `byte`'s `+` is `conv.u1` over `add`.
let private opcodesOf: string -> string list =
    splicedNames (fun e ->
        match e with
        | TExpr.ILIntrinsic(opCode, _, _, _, _) -> ValueSome opCode
        | _ -> ValueNone
    )

/// The external statics the spliced body NAMES. For a width that is not a CIL primitive
/// (`decimal`, `string`) these are its whole body, because no mnemonic can carry it.
let private externalCallsOf: string -> string list =
    splicedNames (fun e ->
        match e with
        | TExpr.ExternalMember(ValueNone, key, memberName, MemberStorage.Method, _, _, _) ->
            let decl = SymbolKeyOps.declTypeKeyOf "an external static in a spliced body" key
            ValueSome(SymbolKeyOps.typeMetaName decl + "." + memberName)
        | _ -> ValueNone
    )

/// `let f (a: T) (b: T) = a <op> b` — the ground binary use site whose splice
/// `opcodesOf` / `externalCallsOf` reads.
let private binarySource (ty: string) (op: string) : string =
    String.concat "" [ "let f (a: "; ty; ") (b: "; ty; ") = a "; op; " b" ]

let private binaryOpcodes (ty: string) (op: string) : string list = opcodesOf (binarySource ty op)

[<Tests>]
let tests =
    testList
        "Arithmetic"
        [
            // The unsigned widths take `div.un` / `rem.un`; the sub-int32 widths add a
            // `conv.*` over the int32-on-stack result. A width missing its declaration
            // altogether fails `splicedNames`' no-errors assertion instead.
            test "each supported primitive selects its own clause with its own opcodes" {
                let signedWide = [ "int"; "int64"; "float"; "float32"; "nativeint" ]
                let unsignedWide = [ "uint32"; "uint64"; "unativeint" ]

                let narrow =
                    [
                        // type, conv, whether `/` and `%` are unsigned
                        "byte", "conv.u1", true
                        "sbyte", "conv.i1", false
                        "int16", "conv.i2", false
                        "uint16", "conv.u2", true
                    ]

                for ty in signedWide do
                    for op, opcode in [ "+", "add"; "-", "sub"; "*", "mul"; "/", "div"; "%", "rem" ] do
                        Expect.equal (binaryOpcodes ty op) [ opcode ] (sprintf "%s %s" ty op)

                for ty in unsignedWide do
                    for op, opcode in [ "+", "add"; "-", "sub"; "*", "mul"; "/", "div.un"; "%", "rem.un" ] do
                        Expect.equal (binaryOpcodes ty op) [ opcode ] (sprintf "%s %s" ty op)

                for ty, conv, unsignedDiv in narrow do
                    for op, opcode in [ "+", "add"; "-", "sub"; "*", "mul" ] do
                        Expect.equal (binaryOpcodes ty op) [ conv; opcode ] (sprintf "%s %s" ty op)

                    let div = if unsignedDiv then "div.un" else "div"
                    let rem = if unsignedDiv then "rem.un" else "rem"
                    Expect.equal (binaryOpcodes ty "/") [ conv; div ] (sprintf "%s /" ty)
                    Expect.equal (binaryOpcodes ty "%") [ conv; rem ] (sprintf "%s %%" ty)

                // `~-` is the SIGNED widths only: negating an unsigned value has no answer
                // the width can hold, so no unsigned type declares it. The narrow signed
                // widths still `conv`: bare `neg` gives 128 for `-(-128y)`, not -128y.
                for ty in [ "int"; "int64"; "float"; "float32"; "nativeint" ] do
                    Expect.equal (opcodesOf (sprintf "let f (a: %s) = -a" ty)) [ "neg" ] (sprintf "%s ~-" ty)

                for ty, conv in [ "sbyte", "conv.i1"; "int16", "conv.i2" ] do
                    Expect.equal (opcodesOf (sprintf "let f (a: %s) = -a" ty)) [ conv; "neg" ] (sprintf "%s ~-" ty)

                for ty in unsignedWide @ [ "byte"; "uint16" ] do
                    failsWith
                        (sprintf "The type '%s' does not support the operator '~-'" ty)
                        (sprintf "let f (a: %s) = -a" ty)
            }

            // `~+` is the identity, so its body splices to the operand and no opcode. It
            // is still a DECLARED member at each width, because that declaration is the
            // only thing admitting the operand, as the `char` row shows.
            test "prefix plus is the identity and leaves no opcode, at every numeric width" {
                for ty in
                    [
                        "int"
                        "byte"
                        "sbyte"
                        "int16"
                        "uint16"
                        "uint32"
                        "int64"
                        "uint64"
                        "float"
                    ] do
                    Expect.isEmpty
                        (opcodesOf (sprintf "let f (a: %s) = +a" ty))
                        (sprintf "%s ~+ splices its operand and nothing else" ty)

                runs "42" "let f (a: int) = +a\nprintfn \"%d\" (f 42)"

                failsWith "The type 'char' does not support the operator '~+'" "let f (a: char) = +a\nignore f"
            }

            test "arithmetic + unary bindings freeze from Vesper.Core and are collected as cross-package inlines" {
                let inlines = ClrSymbolProviders.contractInlineBodies defaultManifests

                let arithmeticOps =
                    [
                        "op_Addition"
                        "op_Subtraction"
                        "op_Multiply"
                        "op_Division"
                        "op_Modulus"
                        "op_UnaryNegation"
                        "op_UnaryPlus"
                    ]

                for name in arithmeticOps do
                    Expect.isTrue
                        (Map.containsKey name inlines)
                        (sprintf "%s body sourced from ops-platform.clr.fs" name)

                // Binary ops abstract twice, `~-` / `~+` once, so strip lambdas to reach
                // the trait call. The static-opt arm catches a body that regains a
                // `when ^T : …` wrapper, which would give the operator a non-trait base.
                let rec traitBase (e: Wire.TExpr) : Wire.TExpr =
                    match e with
                    | TExprG.Lambda(_, b, _, _) -> traitBase b
                    | TExprG.StaticOptimization(_, b, _, _) -> traitBase b
                    | e -> e

                for name in arithmeticOps do
                    match inlines.[name].Decl with
                    | TDeclG.Let(_, v, true, _) ->
                        match traitBase v with
                        | TExprG.TraitCall(_, traitName, _, _, _) ->
                            Expect.equal traitName name (sprintf "%s's body dispatches to its own trait" name)
                        | other -> failtestf "%s's body should be a TraitCall, got %A" name other
                    | other -> failtestf "%s should freeze as an inline `let`, got %A" name other
            }

            // `string`, `decimal` and `bigint` are not CIL primitives, but they declare
            // `(+)` on the type exactly as the numeric widths do; only the body differs.
            test "every width supporting `+` declares it on the type, the non-CIL widths included" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let declares (width: string) (op: string) =
                    match provider.TryLookupMember(RuntimeNames.primitiveKey width, op) with
                    | ValueSome m -> m.IsStatic
                    | ValueNone -> false

                for width in
                    [
                        "int"
                        "byte"
                        "sbyte"
                        "int16"
                        "uint16"
                        "uint32"
                        "int64"
                        "uint64"
                        "decimal"
                        "bigint"
                    ] do
                    for op in [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ] do
                        Expect.isTrue (declares width op) (sprintf "%s declares %s" width op)

                Expect.isTrue (declares "string" "op_Addition") "string declares op_Addition"

                // Concatenation and nothing else, because a declaration is the only
                // thing that would make `"a" - "b"` compile.
                for op in [ "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ] do
                    Expect.isFalse (declares "string" op) (sprintf "string declares no %s" op)
            }

            // No CIL mnemonic exists for these two, so the whole body is a BCL call. It
            // is named `Add`, not `op_Addition`, because the metadata walk filters
            // `SpecialName` methods and the BCL's operator methods are exactly those.
            for width, declaring in [ "decimal", "System.Decimal"; "bigint", "System.Numerics.BigInteger" ] do
                test (sprintf "%s's operators splice a BCL call, and no opcode" width) {
                    for op, method in
                        [
                            "+", "Add"
                            "-", "Subtract"
                            "*", "Multiply"
                            "/", "Divide"
                            "%", "Remainder"
                        ] do
                        let src = binarySource width op

                        Expect.equal
                            (externalCallsOf src)
                            [ declaring + "." + method ]
                            (sprintf "%s %s calls %s.%s" width op declaring method)

                        Expect.isEmpty (opcodesOf src) (sprintf "%s %s rides no mnemonic" width op)

                    Expect.equal
                        (externalCallsOf (sprintf "let f (a: %s) = -a" width))
                        [ declaring + ".Negate" ]
                        (sprintf "%s ~- calls %s.Negate" width declaring)
                }

            // The operands have DIFFERENT types, which only the three-typar
            // `(^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3)` admits. A single-`^T`
            // body would bind the `int` scale factor into a `Vec2`-typed `let`.
            test "a heterogeneous user operator (Vec2 * int -> Vec2) dispatches to its own static member" {
                runs
                    "6"
                    (String.concat
                        "\n"
                        [
                            "type Vec2(x: int, y: int) ="
                            "    member this.X = x"
                            "    member this.Y = y"
                            "    static member ( * ) (v: Vec2, s: int) : Vec2 = Vec2(v.X * s, v.Y * s)"
                            "let v = Vec2(2, 3)"
                            "let r = v * 3"
                            "printfn \"%d\" r.X"
                        ])
            }

            // `(+)`'s `.fsi` carries `default ^T1: int`, which grounds the operand at
            // generalisation when no use site does. Without it there is no member to
            // resolve and `let f a b = a + b` diagnoses instead.
            test "`let f a b = a + b` with no use site defaults to int and emits `add`" {
                typeChecks "let f a b = a + b"
                Expect.equal (opcodesOf "let f a b = a + b") [ "add" ] "the int clause, not the trait-call base"
                runs "42" "let f a b = a + b\nprintfn \"%d\" (f 40 2)"
            }

            test "primitive arithmetic pins no FSharp.Core dependency (no runtime library)" {
                let _, artifact = compileSource "ArithNoDep" "printfn \"%d\" (2 + 2 * 3)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive arithmetic pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "8" "2 + 2 * 3 = 8"
            }

            // `char` is a `TyConst`, not a nominal, and declares no arithmetic, so `+`
            // resolves no member and is a compile error rather than CIL `add` on chars.
            test "char arithmetic diagnoses (declares no member, and not a nominal)" {
                failsWith "The type 'char' does not support the operator '+'" "let x = 'a' + 'b'\nignore x"

                failsWith "The type 'char' does not support the operator '*'" "let x = 'a' * 'b'\nignore x"
            }

            // A CLASS typar is quantified at the type, so neither a use site nor
            // `default ^T1: int` can ground it. The operand stays a bare typar, and no
            // `add` could be emitted over two `!0` references anyway.
            test "`+` on an un-groundable class typar diagnoses instead of emitting `add` on references" {
                failsWith
                    "does not support the operator '+'"
                    (String.concat
                        "\n"
                        [
                            "type Box<'T>(v: 'T) ="
                            "    member this.V = v"
                            "    member this.Plus (other: Box<'T>) = this.V + other.V"
                        ])
            }

            // A user can WRITE a trait call, so an unresolvable one need not come from a
            // contract operator. Either way it must be reported during expansion: a
            // `TraitCall` surviving into codegen has no arm and takes out the emitter.
            test "an unresolvable trait call in a LOCAL inline diagnoses (it does not crash the emitter)" {
                failsWith
                    "The type 'char' does not support the operator '+'"
                    (String.concat
                        "\n"
                        [
                            "let inline plus (a: ^T) (b: ^T) : ^T = ((^T or ^T): (static member (+): ^T * ^T -> ^T) (a, b))"
                            "let z = plus 'a' 'b'"
                            "ignore z"
                        ])
            }

            // `int` declares `static member inline (+): x: int * y: int -> int` in
            // `prim-types-min.fsi`. The three tests below pin what that declaration must
            // NOT change: `int`'s use-site identity, and the emitted method rows.

            test "the int intrinsic publishes op_Addition through the real Vesper.Core contract" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "Vesper.int" 0, "op_Addition") with
                | ValueSome m -> Expect.isTrue m.IsStatic "the declared operator witness is static"
                | ValueNone ->
                    failtest "Vesper.int declares `static member (+)` but the contract publishes no op_Addition"
            }

            // Declaring a member must not turn the primitive into a nominal: intrinsic
            // recognisers, repr lookup and literal inference all key on `int` being
            // `TyConst`, so a `TyClass` int fails on any arithmetic program at all.
            test "declaring a member on int leaves its use-site identity a TyConst intrinsic" {
                let _, artifact = compileSource "IntStillIntrinsic" "printfn \"%d\" (40 + 2)"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "int arithmetic still computes"
            }

            // The witness is a type-level statement, not a runtime method: its body is
            // spliced at the use site, so no `op_Addition` row reaches the emitted PE.
            test "the int operator witness is spliced, never emitted as a method row" {
                let _, artifact = compileSource "IntOpNotEmitted" "printfn \"%d\" (40 + 2)"

                let emitted =
                    peMethodNames (Codegen.toBytes artifact)
                    |> List.filter (fun (_, m) -> m = "op_Addition")

                Expect.isEmpty emitted (sprintf "the intrinsic's operator witness must not be emitted (%A)" emitted)
            }
        ]
