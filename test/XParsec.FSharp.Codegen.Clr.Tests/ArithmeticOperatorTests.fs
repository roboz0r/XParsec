module XParsec.FSharp.Codegen.Clr.Tests.ArithmeticOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The arithmetic / unary-negation operator family (`+ - * / %`, `~-`) is sourced
// from the `Vesper.Core/ops-platform.fs` contract bodies. Each binary body carries
// the three typars its `.fsi` publishes (`x: ^T1 -> y: ^T2 -> ^T3`, support set
// `(^T1 or ^T2)`) and is an F# static-optimization whose *base* is the SRTP TRAIT
// CALL — a user type dispatches to its own `static member (+)` — and whose
// `when ^T1 : … and ^T2 : … and ^T3 : …` clauses carry ONE clause per supported
// primitive, each with that primitive's own IL. Those clauses return DIFFERENT types
// (`byte` / `int16` / …) than the declared `^T3`, which only type-checks because
// `inferLibraryOnlyStaticOptimization` no longer cross-unifies clause bodies.
//
// The consequence these tests have to hold down: NO primitive rides the base. A
// primitive whose clause is missing falls to the trait call, fails to resolve (it is
// not a nominal), and becomes a compile ERROR — so the per-width coverage is not
// decoration, it is the enumeration of the supported set. That set is
// `RuntimeNames.numericTypeNames` (what `Engine.tryPrimitiveTraitCandidate`
// synthesises an arithmetic trait candidate for) minus `decimal`, plus `string` for
// `(+)`.
//
// The per-(operator, width) BEHAVIOURAL rows do not live here. They are the shared
// backend conformance corpus (`test/Codegen.Conformance/ops/arith-*.fs`), which every
// backend is pointed at, judged against a golden rather than against a `=` it is
// itself under test for. What remains here is what a corpus program cannot say — the
// Layer-2/3 anchors that prove *which* emission path fired:
//   - the opcode table is the complete clause enumeration, read straight off the
//     spliced body — it reaches the widths whose LITERALS the front end cannot yet
//     represent, which no behavioural row can;
//   - the freeze test proves the bodies are collected as cross-package inlines and
//     that the static-opt BASE is the trait call;
//   - the diagnostic tests prove an operand the clause list does NOT cover (`decimal`,
//     an unpinned class typar) is REJECTED rather than emitted as garbage IL;
//   - the no-dependency test proves primitive arithmetic pins no FSharp.Core.

/// The inline-IL opcodes the spliced operator body leaves behind for one operand
/// type, outermost first (`byte`'s `+` is `conv.u1` over `add`) — a direct read of
/// WHICH static-opt clause the operand selected. This is how the widths with no
/// literal support (`uint64` / `nativeint` / `unativeint` fold to a `TConstValue.Int`;
/// a negative `sbyte` / `int16` literal does not project at all) still get pinned:
/// annotated parameters need no literal.
let private opcodesOf (src: string) : string list =
    let tast = analyse src

    Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) (sprintf "no errors for: %s" src)

    let acc = ResizeArray<string>()

    let it =
        { TastWalk.identityIter with
            VisitExpr =
                fun _ e ->
                    match e with
                    | TExpr.ILIntrinsic(opCode, _, _, _, _) -> acc.Add opCode
                    | _ -> ()

                    true
        }

    // The entries as well as the decls: a resolved operator body is no longer spliced into
    // the consuming tree, so the clause the operand selected sits in the specialization
    // table with an edge in its place.
    iterFileExprs it tast

    List.ofSeq acc

/// `let f (a: T) (b: T) = a <op> b` — the ground binary use site whose splice
/// `opcodesOf` reads.
let private binaryOpcodes (ty: string) (op: string) : string list =
    opcodesOf (String.concat "" [ "let f (a: "; ty; ") (b: "; ty; ") = a "; op; " b" ])

[<Tests>]
let tests =
    testList
        "Arithmetic"
        [
            // THE clause enumeration. Every primitive the operators support has an
            // explicit clause carrying its own IL; a primitive missing from this table
            // would fall to the trait-call base and fail to compile at all (which is
            // what `opcodesOf`'s no-errors assertion catches), and one carrying the
            // WRONG opcode is what the expected list catches. The unsigned widths take
            // `div.un`/`rem.un`; the sub-int32 widths add a `conv.*` over the
            // int32-on-stack result.
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

                // `~-` does NOT carry the same enumeration — it is the SIGNED widths only.
                // Negating an unsigned value has no answer the width can hold, and F#
                // defines none (FSharp.Core's `UnaryNegationDynamic` lists the signed
                // widths alone), so the unsigned clauses do not exist and `-a` on one is a
                // compile error. The narrow signed widths truncate like every other
                // narrow clause: without the `conv`, `neg` on the int32 stack answers 128
                // for `-(-128y)` instead of wrapping back to -128y.
                for ty in [ "int"; "int64"; "float"; "float32"; "nativeint" ] do
                    Expect.equal (opcodesOf (sprintf "let f (a: %s) = -a" ty)) [ "neg" ] (sprintf "%s ~-" ty)

                for ty, conv in [ "sbyte", "conv.i1"; "int16", "conv.i2" ] do
                    Expect.equal (opcodesOf (sprintf "let f (a: %s) = -a" ty)) [ conv; "neg" ] (sprintf "%s ~-" ty)

                for ty in unsignedWide @ [ "byte"; "uint16" ] do
                    failsWith
                        (sprintf "The type '%s' does not support the operator '~-'" ty)
                        (sprintf "let f (a: %s) = -a" ty)
            }

            test "arithmetic + unary-neg bindings freeze from Vesper.Core and are collected as cross-package inlines" {
                let inlines = ClrSymbolProviders.contractInlineBodies defaultManifests

                let arithmeticOps =
                    [
                        "op_Addition"
                        "op_Subtraction"
                        "op_Multiply"
                        "op_Division"
                        "op_Modulus"
                        "op_UnaryNegation"
                    ]

                for name in arithmeticOps do
                    Expect.isTrue (Map.containsKey name inlines) (sprintf "%s body sourced from ops-platform.fs" name)

                // Every arithmetic body IS the SRTP trait call — an operand type either
                // declares the member or does not support the operator. Nothing rides a
                // raw-IL base, which is what makes an unsupported operand diagnose.
                // Binary ops abstract twice, `~-` once. `(+)` is the one still wrapped in
                // a static-opt: its lone `string` clause (see `prim-types-string.fsi`).
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

            // Every width states its own `(+)` on the type, so the operator itself no
            // longer names any. `string` alone still needs a clause, and its declaration
            // being absent is what this pins — the `.fsi` remark says why.
            test "only `string` still rides a clause on `+`; every numeric width declares its own" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                let declares (width: string) (op: string) =
                    match provider.TryLookupMember(RuntimeNames.primitiveKey width, op) with
                    | ValueSome m -> m.IsStatic
                    | ValueNone -> false

                for width in [ "int"; "byte"; "sbyte"; "int16"; "uint16"; "uint32"; "int64"; "uint64" ] do
                    for op in [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ] do
                        Expect.isTrue (declares width op) (sprintf "%s declares %s" width op)

                Expect.isFalse (declares "string" "op_Addition") "string's `(+)` is still the operator's own clause"
            }

            // A HETEROGENEOUS user operator (`Vec2 * int -> Vec2`): the contract's
            // `(^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3)` admits distinct
            // operand types, so the `.fs` body must carry the same three typars. A
            // single-`^T` body folds both operands into one substitution slot, binding
            // the `int` scale factor into a `Vec2`-typed `let` — a type lie the CLR
            // rejects.
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

            // Eager defaulting (`default ^T1 : int`) grounds an otherwise-free operand
            // at generalisation, so the `int` clause — not the trait-call base — is what
            // a use-site-less generic `+` selects. If defaulting ever stops firing here
            // this becomes the operator diagnostic, which is the signal to fix
            // defaulting, NOT to widen the base.
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

            // ---- The trait-call base's own contract: an operand it cannot dispatch
            // to is a DIAGNOSTIC, never emitted IL.

            // `decimal` is a `TyConst`, not a nominal, and CIL `add` on a
            // `System.Decimal` is garbage — so it carries no clause and is rejected
            // until one calling `Decimal::op_Addition` lands.
            test "decimal arithmetic diagnoses (no clause, and not a nominal)" {
                failsWith "The type 'decimal' does not support the operator '+'" "let x = 1.5M + 2.5M\nignore x"

                failsWith "The type 'decimal' does not support the operator '*'" "let x = 1.5M * 2.5M\nignore x"
            }

            // A CLASS typar is quantified at the type, so no use site and no `default`
            // can ground it: the operand reaches the base as a bare typar. This is the
            // hole the inversion closes — a raw-IL base emitted `add` on two `!0`
            // references here.
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

            // The diagnostic is owed by EVERY expansion path, not just the external one.
            // A user-written SRTP trait call is ordinary source (`pStaticMemberInvocation`
            // is an alternative of `pParen`, ungated), so a LOCAL `let inline` can carry
            // one — and a local inline is spliced by the same pass through the same
            // `Inline.inlineExpand`. When the receiver is a primitive it cannot dispatch,
            // and the surviving `TraitCall` has no arm in EITHER backend: unreported, it
            // is an emitter `failwithf`, not a compile error. So the report lives in the
            // one expansion entry point every path goes through.
            test "an unresolvable trait call in a LOCAL inline diagnoses (it does not crash the emitter)" {
                failsWith
                    "The type 'decimal' does not support the operator '+'"
                    (String.concat
                        "\n"
                        [
                            "let inline plus (a: ^T) (b: ^T) : ^T = ((^T or ^T): (static member (+): ^T * ^T -> ^T) (a, b))"
                            "let z = plus 1.5M 2.5M"
                            "ignore z"
                        ])
            }

            // `int` declares its own `static member (+)` in `prim-types-min.fsi` — the trait
            // witness stated on the type instead of synthesised from an operator-name table.
            // These three pin the seam that makes such a declaration safe to write.

            test "the int intrinsic publishes op_Addition through the real Vesper.Core contract" {
                let provider = ClrSymbolProviders.buildContract defaultManifests

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "Vesper.int" 0, "op_Addition") with
                | ValueSome m -> Expect.isTrue m.IsStatic "the declared operator witness is static"
                | ValueNone ->
                    failtest "Vesper.int declares `static member (+)` but the contract publishes no op_Addition"
            }

            // Declaring a member must NOT turn the primitive into a nominal: every intrinsic
            // recogniser, repr lookup and literal-inference path keys on `int` being `TyConst`.
            // A `TyClass` int would not survive a single arithmetic program.
            test "declaring a member on int leaves its use-site identity a TyConst intrinsic" {
                let _, artifact = compileSource "IntStillIntrinsic" "printfn \"%d\" (40 + 2)"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "int arithmetic still computes"
            }

            // The witness is a type-level statement, not a runtime method: its body is spliced
            // at the use site, so no `op_Addition` row may appear in the emitted program.
            test "the int operator witness is spliced, never emitted as a method row" {
                let _, artifact = compileSource "IntOpNotEmitted" "printfn \"%d\" (40 + 2)"

                let emitted =
                    peMethodNames (Codegen.toBytes artifact)
                    |> List.filter (fun (_, m) -> m = "op_Addition")

                Expect.isEmpty emitted (sprintf "the intrinsic's operator witness must not be emitted (%A)" emitted)
            }
        ]
