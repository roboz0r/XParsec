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
// This file is the Layer-1 exemplar for docs/codegen-test-strategy-plan.md: the
// dense `(expr, result)` corpus below is the broad, cheap regression net (it would
// catch a change to operator routing / Pratt RHS silently breaking `100 % 7`), and
// the thick tests beneath it are the Layer-2/3 anchors that prove *which* emission
// path fired:
//   - the opcode table is the complete clause enumeration, read straight off the
//     spliced body — it reaches the widths whose LITERALS the front end cannot yet
//     represent, which no behavioural row can;
//   - the freeze test proves the bodies are collected as cross-package inlines and
//     that the static-opt BASE is the trait call;
//   - the byte-wraparound tests are the load-bearing proof the contract body — not a
//     bare `add` — emits at a ground use site: an untruncated `add` leaves the int32
//     sum 300 on the stack, so `200uy + 100uy` would be `300`; the contract's
//     `when ^T : byte` clause wraps it to `44` via `conv.u1`;
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

    Expect.isEmpty
        (tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error))
        (sprintf "no errors for: %s" src)

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

    for d in tast.Decls do
        match d with
        | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
        | _ -> ()

    List.ofSeq acc

/// `let f (a: T) (b: T) = a <op> b` — the ground binary use site whose splice
/// `opcodesOf` reads.
let private binaryOpcodes (ty: string) (op: string) : string list =
    opcodesOf (String.concat "" [ "let f (a: "; ty; ") (b: "; ty; ") = a "; op; " b" ])

/// Layer 1 — behavioral corpus (wide, cheap). The row sources *are* the coverage
/// map: each `(expr, expected)` is one `runs` assertion named by its source. The
/// narrow widths route through `int (…)` rather than a same-typed literal comparison:
/// a NEGATIVE `sbyte` / `int16` literal is not representable in `TConstValue` (the
/// lexer's magnitude fold), so `100y + 100y = -56y` cannot be written — but
/// `int (100y + 100y) = -56` reads the same truncation.
let private corpus: Test list =
    [
        for src, expected in
            [
                // ---- int: the four binary ops, each on its own so a routing break
                // is punctual
                """printfn "%d" (2 + 3)""", "5"
                """printfn "%d" (10 - 4)""", "6"
                """printfn "%d" (6 * 7)""", "42"
                """printfn "%d" (100 / 7)""", "14"
                """printfn "%d" (100 % 7)""", "2"
                // negative results
                """printfn "%d" (3 - 10)""", "-7"
                // left-associativity (7-3-2 = 2, not 6; 100/5/2 = 10, not 40)
                """printfn "%d" (7 - 3 - 2)""", "2"
                """printfn "%d" (100 / 5 / 2)""", "10"
                // precedence: * / % bind tighter than + -
                """printfn "%d" (2 + 3 * 4)""", "14"
                """printfn "%d" ((10 + 5) * 2 - 3)""", "27"
                """printfn "%d" (100 - 2 * 3 + 1)""", "95"

                // ---- int64: `add`/`sub`/`mul`/`div`/`rem` at 64-bit width (the
                // magnitudes exceed int32, so a 32-bit opcode would visibly wrap)
                """printfn "%d" (if 1000000000000L + 1L = 1000000000001L then 1 else 0)""", "1"
                """printfn "%d" (if 1000000000000L - 1L = 999999999999L then 1 else 0)""", "1"
                """printfn "%d" (if 1000000000000L * 3L = 3000000000000L then 1 else 0)""", "1"
                """printfn "%d" (if 3000000000000L / 3L = 1000000000000L then 1 else 0)""", "1"
                """printfn "%d" (if 1000000000001L % 10L = 1L then 1 else 0)""", "1"

                // ---- float / float32
                """printfn "%d" (if 1.5 + 2.5 = 4.0 then 1 else 0)""", "1"
                """printfn "%d" (if 3.0 - 1.5 = 1.5 then 1 else 0)""", "1"
                """printfn "%d" (if 1.5 * 2.0 = 3.0 then 1 else 0)""", "1"
                """printfn "%d" (if 7.5 / 2.5 = 3.0 then 1 else 0)""", "1"
                """printfn "%d" (if 7.5 % 2.0 = 1.5 then 1 else 0)""", "1"
                """printfn "%d" (if 1.5f + 2.5f = 4.0f then 1 else 0)""", "1"
                """printfn "%d" (if 3.0f - 1.5f = 1.5f then 1 else 0)""", "1"
                """printfn "%d" (if 1.5f * 2.0f = 3.0f then 1 else 0)""", "1"
                """printfn "%d" (if 7.5f / 2.5f = 3.0f then 1 else 0)""", "1"

                // ---- uint32: the `div.un` / `rem.un` clauses. Both operands exceed
                // int31, so a SIGNED `div`/`rem` reads them as negative and gives a
                // different answer — these rows fail if the unsigned opcode is lost.
                """printfn "%d" (if 4000000000u + 1u = 4000000001u then 1 else 0)""", "1"
                """printfn "%d" (if 4000000000u - 1u = 3999999999u then 1 else 0)""", "1"
                """printfn "%d" (if 100000u * 100000u = 1410065408u then 1 else 0)""", "1"
                """printfn "%d" (if 4000000000u / 2u = 2000000000u then 1 else 0)""", "1"
                """printfn "%d" (if 4000000000u % 3u = 1u then 1 else 0)""", "1"

                // ---- byte: `conv.u1` truncation, `div.un`/`rem.un` on `/`/`%`
                """printfn "%d" (int (10uy - 20uy))""", "246"
                """printfn "%d" (int (200uy / 3uy))""", "66"
                """printfn "%d" (int (200uy % 7uy))""", "4"

                // ---- sbyte: `conv.i1` (100 + 100 = 200 → -56)
                """printfn "%d" (int (100y + 100y))""", "-56"
                """printfn "%d" (int (0y - 100y))""", "-100"
                """printfn "%d" (int (100y * 2y))""", "-56"
                """printfn "%d" (int (100y / 3y))""", "33"

                // ---- int16: `conv.i2` (30000 + 10000 = 40000 → -25536)
                """printfn "%d" (int (30000s + 10000s))""", "-25536"
                """printfn "%d" (int (0s - 30000s))""", "-30000"
                """printfn "%d" (int (300s * 300s))""", "24464"
                """printfn "%d" (int (30000s / 3s))""", "10000"

                // ---- uint16: `conv.u2` (60000 + 10000 = 70000 → 4464)
                """printfn "%d" (int (60000us + 10000us))""", "4464"
                """printfn "%d" (int (10us - 20us))""", "65526"
                """printfn "%d" (int (300us * 300us))""", "24464"
                """printfn "%d" (int (60000us / 3us))""", "20000"
                """printfn "%d" (int (60000us % 7us))""", "3"

                // ---- string `+`: `System.String.Concat`, NOT `add` (which on two
                // string references is a garbage pointer)
                """printfn "%d" (if "ab" + "cd" = "abcd" then 1 else 0)""", "1"
            ] -> test src { runs expected src }
    ]

[<Tests>]
let tests =
    testList
        "Arithmetic"
        (corpus
         @ [
             // Unary negation through a binding routes the variable through the
             // contract's per-width `neg` clause (a literal `-5` is a negative
             // constant, not the operator — so exercise it via a function parameter).
             test "unary negation routes through the contract `neg` clauses (int / int64 / float)" {
                 runsLines
                     [ "3"; "1"; "1" ]
                     (String.concat
                         "\n"
                         [
                             "let negI (x: int) = -x"
                             "let negL (x: int64) = -x"
                             "let negF (x: float) = -x"
                             "printfn \"%d\" (negI 5 + 8)" // -5 + 8 = 3
                             "printfn \"%d\" (if negL 1000000000000L = -1000000000000L then 1 else 0)"
                             "printfn \"%d\" (if negF 2.5 = -2.5 then 1 else 0)"
                         ])
             }

             // ---- Layer 2/3: structural anchors (keep) ------------------------

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

                 // `~-` carries the same enumeration; `neg` is the two's-complement
                 // negation at every integral width and the sign flip for the floats.
                 for ty in signedWide @ unsignedWide @ [ "byte"; "sbyte"; "int16"; "uint16" ] do
                     Expect.equal
                         (opcodesOf (String.concat "" [ "let f (a: "; ty; ") = -a" ]))
                         [ "neg" ]
                         (sprintf "%s ~-" ty)
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

                 // Every arithmetic body is a static-opt whose BASE is the SRTP trait
                 // call: that inversion is what makes an unsupported operand diagnose
                 // instead of riding a raw-IL base. Binary ops abstract twice, `~-` once.
                 let staticOptBase (decl: TDecl) : TExpr voption =
                     match decl with
                     | TDecl.Let(_,
                                 TExpr.Lambda(_, TExpr.Lambda(_, TExpr.StaticOptimization(_, b, _, _), _, _), _, _),
                                 true,
                                 _)
                     | TDecl.Let(_, TExpr.Lambda(_, TExpr.StaticOptimization(_, b, _, _), _, _), true, _) -> ValueSome b
                     | _ -> ValueNone

                 for name in arithmeticOps do
                     match staticOptBase inlines.[name].Decl with
                     | ValueSome(TExpr.TraitCall(_, traitName, _, _, _)) ->
                         Expect.equal traitName name (sprintf "%s's static-opt base dispatches to its own trait" name)
                     | other -> failtestf "%s's static-opt base should be a TraitCall, got %A" name other
             }

             test "byte `+` wraps via the contract `conv.u1` clause (200uy + 100uy = 44uy, not 300)" {
                 // An untruncated `add` would leave the int32 sum 300 on the stack, so
                 // `(200uy + 100uy) = 44uy` would be false. The contract's `when ^T :
                 // byte` clause truncates to 44, so it is true.
                 let src = "printfn \"%d\" (if 200uy + 100uy = 44uy then 1 else 0)"

                 let _, artifact = compileSource "ArithByteWrap" src
                 let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                 Expect.equal exitCode 0 "Main returns 0"
                 Expect.equal (output.Trim()) "1" "byte addition wraps mod 256 (conv.u1 truncation)"
             }

             test "byte `*` wraps via the contract `conv.u1` clause (20uy * 20uy = 144uy, not 400)" {
                 // 20 * 20 = 400; 400 mod 256 = 144.
                 let src = "printfn \"%d\" (if 20uy * 20uy = 144uy then 1 else 0)"

                 let _, artifact = compileSource "ArithByteMul" src
                 let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                 Expect.equal exitCode 0 "Main returns 0"
                 Expect.equal (output.Trim()) "1" "byte multiplication wraps mod 256"
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
         ])
