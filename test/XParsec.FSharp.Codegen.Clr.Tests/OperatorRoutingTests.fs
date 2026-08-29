module XParsec.FSharp.Codegen.Clr.Tests.OperatorRoutingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// Inline expansion splices an operator's contract body at the use site before freeze.
// `(=)` is `EqualityComparer<^T>.Default.Equals(x, y)` under a `when ^T: int = (# "ceq" #)`
// clause per primitive: a ground `int` emits `ceq`, anything else keeps the comparer base.

/// Every expression reachable from `e`, itself included, so a test can assert what an
/// operator lowered TO structurally rather than string-matching a `%A` render.
let private subExprs (e: TastAccessor.ExprId) : TastAccessor.ExprId list =
    let acc = ResizeArray<TastAccessor.ExprId>()

    let rec go (x: TastAccessor.ExprId) =
        acc.Add x
        TastAccessor.iterChildren go x

    go e
    List.ofSeq acc

let private frozenExprs (decls: TastAccessor.DeclId list) : TastAccessor.ExprId list =
    [
        for d in decls do
            match d with
            | TastAccessor.DLet lv -> yield! subExprs lv.Value
            | TastAccessor.DExpression(e, _) -> yield! subExprs e
            | _ -> ()
    ]

/// `e` contains the named inline-IL opcode anywhere below it.
let private hasIlIntrinsic (op: string) (e: TastAccessor.ExprId) : bool =
    subExprs e
    |> List.exists (fun x ->
        match TastAccessor.exprKind x with
        | ExprShape.ILIntrinsic -> TastAccessor.exprILIntrinsicOpCode x = op
        | _ -> false
    )

/// The SYMBOL a node refers to, rendered; `ValueNone` for a node with no symbol. Lets a
/// test say "this body reaches `EqualityComparer<_>.Equals`" without rendering the tree.
let private symbolText (e: TastAccessor.ExprId) : string voption =
    match TastAccessor.exprKind e with
    | ExprShape.StaticMethodCall -> ValueSome(sprintf "%A" (TastAccessor.exprStaticMethodCallKey e))
    | ExprShape.StaticPropertyGet -> ValueSome(sprintf "%A" (TastAccessor.exprStaticPropertyGetKey e))
    | ExprShape.MethodCall -> ValueSome(sprintf "%A" (TastAccessor.exprMethodCall e).Key)
    | ExprShape.PropertyGet -> ValueSome(sprintf "%A" (TastAccessor.exprPropertyGet e).Key)
    | ExprShape.External -> ValueSome(sprintf "%A" (TastAccessor.exprExternal e))
    | ExprShape.ExternalMember ->
        let em = TastAccessor.exprExternalMember e
        ValueSome(sprintf "%A %s" em.Key em.MemberName)
    | ExprShape.New -> ValueSome(TastAccessor.exprNewClassName e)
    | _ -> ValueNone

/// Does any node below `e` (itself included) refer to a symbol whose rendering mentions
/// `needle`?
let private mentionsSymbol (needle: string) (e: TastAccessor.ExprId) : bool =
    subExprs e
    |> List.exists (fun x ->
        match symbolText x with
        | ValueSome t -> t.Contains needle
        | ValueNone -> false
    )

/// `Vesper.Core` alone. `<` lives in `Vesper.Comparison`, which this stack does NOT
/// reference, so `2 < 3` cannot resolve.
let private coreOnly = lazy (ClrSymbolProviders.buildContract [ vesperCorePackage ])

let private analyseCoreOnly (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling coreOnly.Value (LexedFile.ofText lexed) file

[<Tests>]
let tests =
    testList
        "OperatorRouting"
        [
            test "an operator whose contract is not referenced diagnoses by its SOURCE spelling" {
                // The user typed `<`, so the diagnostic writes `<`, not `op_LessThan`, in FS0043's
                // shape against the operand type.
                let tast = analyseCoreOnly "let b = 2 < 3"

                let messages = [ for d in tast.Diagnostics -> d.Message ]

                Expect.isTrue
                    (messages
                     |> List.exists (fun m -> m.Contains "does not support the operator '<'"))
                    (sprintf "expected the `<` not-in-scope diagnostic, got %A" messages)

                Expect.isFalse
                    (messages |> List.exists (fun m -> m.Contains "op_LessThan"))
                    (sprintf "the compiled name must not leak into a diagnostic, got %A" messages)
            }

            test "`let f a b = a = b` lowers the un-ground `=` to the comparer base (no External op_Equality survives)" {
                let ctx, tast = analyseWithCtx "let f a b = a = b"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                // `a`/`b` are never pinned, so no `when ^T : …` clause selects. A `ceq`
                // here would compare the two operands by reference.
                match Emit.lower (pooledDecls (Freeze.run ctx tast)) with
                | [ TastAccessor.DLet lv ] when
                    (TastAccessor.patBoundVar lv.Pattern).IsSome
                    && not lv.IsInline
                    && TastAccessor.exprKind lv.Value = ExprShape.Lambda
                    && TastAccessor.exprKind (TastAccessor.exprLambda lv.Value).Body = ExprShape.Lambda
                    ->
                    let body = (TastAccessor.exprLambda (TastAccessor.exprLambda lv.Value).Body).Body

                    Expect.isTrue
                        (mentionsSymbol "EqualityComparer" body)
                        "expected the `=` base (EqualityComparer.Equals)"

                    Expect.isFalse (hasIlIntrinsic "ceq" body) "an un-ground `=` must not emit a reference `ceq`"
                | other -> failtestf "expected `let f a b = a = b` to lower to a two-lambda let, got %A" other
            }

            test "`let eq a b = a = b` over a DU routes STRUCTURALLY (the comparer base over a free method typar)" {
                // `eq` is generic, so the base encodes as `EqualityComparer<!!0>` over a
                // free METHOD typar, which must emit, verify, and compare structurally.
                // Two distinct-but-equal `Tag`s give 1; a reference `ceq` would give 0.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Tag = Tag of int"
                            "let eq a b = a = b"
                            "printfn \"%d\" (if eq (Tag 1) (Tag 1) then 1 else 0)"
                            "printfn \"%d\" (if eq (Tag 1) (Tag 2) then 1 else 0)"
                        ]

                let artifact = compileSource "OpEqGenericDU" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n0"
                    "a generic `eq` compares two distinct-but-equal DU values structurally"
            }

            test "`let f a b = a = b` (the handoff target): `ceq` is true for 2=2, false for 2<>3" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f a b = a = b"
                            "printfn \"%d\" (if f 2 2 then 1 else 0)"
                            "printfn \"%d\" (if f 2 3 then 1 else 0)"
                        ]

                let artifact = compileSource "OpRoutingEq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "f 2 2 = true, f 2 3 = false"
            }

            test "primitive equality does not pin an FSharp.Core dependency" {
                // `=` on ints is a bare `ceq`, with no metadata and no comparer call.
                let artifact =
                    compileSource "OpRoutingEqNoDep" "printfn \"%d\" (if 2 = 2 then 1 else 0)"

                expectNoFSharpCore artifact "primitive `=`"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "2 = 2"
            }

            test "`<>` lowers to `not ceq` and runs: false for 2<>2, true for 2<>3" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f a b = a <> b"
                            "printfn \"%d\" (if f 2 2 then 1 else 0)"
                            "printfn \"%d\" (if f 2 3 then 1 else 0)"
                        ]

                let artifact = compileSource "OpRoutingNeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "0\n1" "f 2 2 = false, f 2 3 = true"
            }

            test "the ordering ops route to clt/cgt (and their negations) and run" {
                // `<` → clt, `>` → cgt, `<=` → not cgt, `>=` → not clt.
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (if 2 < 3 then 1 else 0)" // 1
                            "printfn \"%d\" (if 3 < 2 then 1 else 0)" // 0
                            "printfn \"%d\" (if 2 > 3 then 1 else 0)" // 0
                            "printfn \"%d\" (if 3 > 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 2 <= 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 3 <= 2 then 1 else 0)" // 0
                            "printfn \"%d\" (if 2 >= 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 2 >= 3 then 1 else 0)" // 0
                        ]

                let artifact = compileSource "OpRoutingOrdering" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0\n0\n1\n1\n0\n1\n0" "ordering ops compute correctly"
            }

            test "a nested mix of arithmetic + equality lowers and runs (one IL path for the whole surface)" {
                let artifact =
                    compileSource "OpRoutingMixed" "printfn \"%d\" (if (1 + 2) * 3 = 9 then 1 else 0)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "(1 + 2) * 3 = 9 is true"
            }

            test "`=`/`<>` freeze from the Vesper.Core contract and are collected as cross-package inlines" {
                let inlines = ClrSymbolProviders.contractInlineBodies defaultPackages

                Expect.isTrue
                    (Map.containsKey "op_Equality" inlines)
                    "op_Equality body sourced from ops-platform.clr.fs"

                Expect.isTrue
                    (Map.containsKey "op_Inequality" inlines)
                    "op_Inequality body sourced from ops-platform.clr.fs"

                let isStaticOptInline =
                    function
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.Lambda(_, TExprG.StaticOptimization _, _, _), _, _), true, _) ->
                        true
                    | _ -> false

                Expect.isTrue (isStaticOptInline inlines.["op_Equality"].Decl) "op_Equality is a static-opt inline"
                Expect.isTrue (isStaticOptInline inlines.["op_Inequality"].Decl) "op_Inequality is a static-opt inline"
            }

            test
                "DU `=` is structural: a distinct-but-equal pair returns true via the comparer (where `ceq` gives false)" {
                // `x` and `y` are distinct heap instances with equal payloads, so a
                // reference `ceq` would print 0 where the comparer prints 1.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Tag = Tag of int"
                            "let x = Tag 1"
                            "let y = Tag 1"
                            "printfn \"%d\" (if x = y then 1 else 0)" // distinct instances, equal payload → comparer → 1
                            "printfn \"%d\" (if x <> y then 1 else 0)" // → 0
                        ]

                let artifact = compileSource "OpEqDUStructural" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n0"
                    "distinct-but-equal DU pair compares structurally (true), not by reference"
            }

            // A saturated `ignore` lowers to the `let _ = expr` shape: eval, pop, reify
            // unit. The `printfn` side effect proves the argument was evaluated rather
            // than elided along with its result.
            test "`expr |> ignore` on a non-unit value evaluates the arg and discards it" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (x: int) : int ="
                            "    printfn \"side %d\" x"
                            "    x + 1"
                            "f 41 |> ignore"
                            "ignore (f 7)" // the un-piped form, same function + arguments
                            "printfn \"done\""
                        ]

                let artifact = compileSource "IgnoreNonUnit" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "side 41\nside 7\ndone"
                    "both ignore forms run the side effect and discard the int result"
            }

            test "an eta'd `(+)` splices the contract body PRE-freeze (a lambda over `add`, no External op_Addition)" {
                // `List.fold (+) 0 xs` pins `(+)` to `int -> int -> int`, so the eta
                // `fun x y -> x + y` grounds `^T := int` and `(# "add" #)` survives.
                // A surviving `External(op_Addition)` means the eta ran after the splice.
                let ctx, tast =
                    analyseWithCtx "let xs = [1; 2; 3]\nprintfn \"%d\" (List.fold (+) 0 xs)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exprs = frozenExprs (pooledDecls (Freeze.run ctx tast))

                Expect.isFalse
                    (exprs
                     |> List.exists (fun x ->
                         match x with
                         | TastAccessor.EExternal ext -> ext.CompiledName = "op_Addition"
                         | _ -> false
                     ))
                    "no `op_Addition` External survives the pre-freeze eta + splice"

                // The `add` must sit inside the eta'd closure `List.fold` receives, not
                // merely somewhere in the decl. It is BELOW the inner lambda rather than
                // directly its body: beta-reduction leaves the inline parameters as `Let`s.
                let addInLambda =
                    exprs
                    |> List.exists (fun x ->
                        match x with
                        | TastAccessor.ELambda outer when TastAccessor.exprKind outer.Body = ExprShape.Lambda ->
                            hasIlIntrinsic "add" outer.Body
                        | _ -> false
                    )

                Expect.isTrue addInLambda "the eta'd `(+)` is a two-lambda closure over an `add` ILIntrinsic"
            }

            test "a `let`-bound custom operator compiles to a call on the binding and runs" {
                runs "3" "let (>=>) (a: int) (b: int) = a + b\nprintfn \"%d\" (1 >=> 2)"
            }

            test "`List.fold (+) 0 [1; 2; 3]` runs to 6 through the eta'd contract body" {
                runs "6" "printfn \"%d\" (List.fold (+) 0 [1; 2; 3])"
            }

            test "an eta-reachable `=` over a DU is STRUCTURAL (the comparer base, not a reference `ceq`)" {
                // `(=) (Tag 1)` reaches `List.filter` as a function value, spliced with
                // `^T := Tag`, so no primitive clause selects. The list holds two DISTINCT
                // instances equal to `Tag 1`, so a reference `ceq` counts 0, not 2.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Tag = Tag of int"
                            "let xs = [Tag 1; Tag 2; Tag 1]"
                            "printfn \"%d\" (List.length (List.filter ((=) (Tag 1)) xs))"
                        ]

                runs "2" src
            }

            test "the eta'd `(+)` compiles to an ordinary `Vesper.Fun` closure with `add` inlined into Invoke" {
                // The eta'd operator closure-converts exactly as a hand-written
                // `fun x y -> x + y` would: a curried `Vesper.Fun`2` pair whose innermost
                // `Invoke` carries the spliced `add` (CIL 0x58), calling out to nothing.
                let artifact =
                    compileSource "EtaClosureShape" "printfn \"%d\" (List.fold (+) 0 [1; 2; 3])"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let invokesWithAdd =
                    [
                        for t in asm.GetTypes() do
                            if t.GetInterfaces() |> Array.exists (fun i -> i.Name = "Fun`2") then
                                match t.GetMethod "Invoke" with
                                | null -> ()
                                | m ->
                                    match m.GetMethodBody() with
                                    | null -> ()
                                    | body ->
                                        if body.GetILAsByteArray() |> Array.contains 0x58uy then
                                            yield t.Name
                    ]

                Expect.isNonEmpty
                    invokesWithAdd
                    "a `Vesper.Fun` closure's Invoke carries the spliced `add` opcode inline"
            }
        ]
