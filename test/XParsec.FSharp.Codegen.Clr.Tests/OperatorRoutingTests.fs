module XParsec.FSharp.Codegen.Clr.Tests.OperatorRoutingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// An operator use site (`a = b`, `x + y`, `a < b`) freezes to an `External(op_*)`
// call head; the pre-freeze `Passes.InlineExpansion` pass splices the operator's
// contract body there and resolves its `StaticOptimization` clauses — codegen owns no
// per-operator recipe. These tests pin the result at the TAST level (`Emit.lower`) and
// end to end (compile + run real CIL).
//
// The equality family (`=`/`<>`) is sourced from the frozen
// `Vesper.Core/ops-platform.clr.fs` contract body: the operator-named binding
// `let inline (=) …` is collected by `ClrSymbolProviders.inlineBodies` and spliced at
// each use site. A GROUND primitive operand selects the `when ^T : int` clause and
// emits `(# "ceq" #)`; every other operand — an aggregate, or a still-free `^T` in a
// generic `let f a b = a = b` — falls to the static-opt BASE,
// `EqualityComparer<^T>.Default.Equals`, which compares structurally and encodes fine
// over a free method typar. The base is spliced UNCONDITIONALLY: an inline body is
// never declined for un-ground operands.
//
// An operator used as a VALUE (`List.fold (+) 0 xs`) is not an application, so
// `InlineExpansion` eta-reifies it first (`fun x y -> x + y`) and splices the body into
// the `App` its own eta minted. The assertions below pin that against the FROZEN decls
// — before codegen runs at all — so they hold on the contract body itself, not on
// whatever a backend might make of it. Codegen has no operator dispatch to fall back on:
// an operator that reached it unspliced would emit as an unresolved `External`.

/// Every expression reachable from `e` (itself included) — so a test can assert what an
/// operator lowered TO structurally, rather than string-matching a `%A` render.
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

/// The SYMBOL a node names, rendered — `ValueNone` for a node that names none. Lets a
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

/// Does any node below `e` (itself included) name a symbol whose rendering mentions
/// `needle`?
let private mentionsSymbol (needle: string) (e: TastAccessor.ExprId) : bool =
    subExprs e
    |> List.exists (fun x ->
        match symbolText x with
        | ValueSome t -> t.Contains needle
        | ValueNone -> false
    )

/// `Vesper.Core` alone — `<` lives in `Vesper.Comparison`, which this stack does NOT
/// reference, so `2 < 3` cannot resolve.
let private coreOnly =
    lazy (ClrSymbolProviders.buildContract [ vesperCoreManifest ])

let private analyseCoreOnly (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSem coreOnly.Value (Hashing.originSourceOfText lexed) file

[<Tests>]
let tests =
    testList
        "OperatorRouting"
        [
            test "an operator whose contract is not referenced diagnoses by its SOURCE spelling" {
                // The user typed `<`, never `op_LessThan` — the compiled name is an
                // implementation detail and must not leak into a diagnostic. No package is
                // named: the declaring contract is absent from the referenced set, so
                // nothing the compiler can see knows `<` exists (naming `Vesper.Comparison`
                // would take a hardcoded operator→package table).
                let tast = analyseCoreOnly "let b = 2 < 3"

                let messages = [ for d in tast.Diagnostics -> d.Message ]

                Expect.isTrue
                    (messages |> List.exists (fun m -> m.Contains "No definition for '<' found"))
                    (sprintf "expected the `<` not-in-scope diagnostic, got %A" messages)

                Expect.isFalse
                    (messages |> List.exists (fun m -> m.Contains "op_LessThan"))
                    (sprintf "the compiled name must not leak into a diagnostic, got %A" messages)
            }

            test "`let f a b = a = b` lowers the un-ground `=` to the comparer base (no External op_Equality survives)" {
                let ctx, tast = analyseWithCtx "let f a b = a = b"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                // `a`/`b` are never pinned, so no per-primitive `when ^T : …` clause
                // selects and the body's base — `EqualityComparer<^T>.Default.Equals(a, b)`
                // — is what survives. A `ceq` ILIntrinsic here would be the reference
                // comparison the ground guard used to fall back to.
                match Emit.lower (pooledDecls (Freeze.run ctx tast)) with
                | [ TastAccessor.DLet lv ] when
                    (TastAccessor.patBinder lv.Binding).IsSome
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

            test "`let eq a b = a = b` over a DU answers STRUCTURALLY (the comparer base over a free method typar)" {
                // The static-opt base
                // `EqualityComparer<^T>.Default.Equals` must EMIT, VERIFY, and answer
                // structurally when `^T` is a free *method* typar — i.e. `EqualityComparer<!!0>`
                // is encodable. Two distinct-but-equal `Tag` instances must compare
                // equal through the generic `eq`; a reference `ceq` gives 0.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Tag = Tag of int"
                            "let eq a b = a = b"
                            "printfn \"%d\" (if eq (Tag 1) (Tag 1) then 1 else 0)"
                            "printfn \"%d\" (if eq (Tag 1) (Tag 2) then 1 else 0)"
                        ]

                let _, artifact = compileSource "OpEqGenericDU" src
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

                let _, artifact = compileSource "OpRoutingEq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "f 2 2 = true, f 2 3 = false"
            }

            test "primitive equality pins no FSharp.Core dependency (eq §4: no runtime library)" {
                // `=` on ints lowers to bare `ceq` — no metadata, no comparer call —
                // so the emitted PE is FSharp.Core-free (the happy-path `printfn` is
                // too, via Vesper.Formatter).
                let _, artifact =
                    compileSource "OpRoutingEqNoDep" "printfn \"%d\" (if 2 = 2 then 1 else 0)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive `=` pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

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

                let _, artifact = compileSource "OpRoutingNeq" src
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

                let _, artifact = compileSource "OpRoutingOrdering" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0\n0\n1\n1\n0\n1\n0" "ordering ops compute correctly"
            }

            test "a nested mix of arithmetic + equality lowers and runs (one IL path for the whole surface)" {
                // `(1 + 2) * 3 = 9` exercises add, mul, ceq nested through the same
                // `TExprG.ILIntrinsic` machinery.
                let _, artifact =
                    compileSource "OpRoutingMixed" "printfn \"%d\" (if (1 + 2) * 3 = 9 then 1 else 0)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "(1 + 2) * 3 = 9 is true"
            }

            test "`=`/`<>` freeze from the Vesper.Core contract and are collected as cross-package inlines" {
                // The operator-named bindings `let inline (=)` / `let inline (<>)` in
                // `ops-platform.clr.fs` freeze and are sourced by the codegen inline-body
                // loader — the sole supply of `=`/`<>` semantics.
                let inlines = ClrSymbolProviders.contractInlineBodies defaultManifests

                Expect.isTrue
                    (Map.containsKey "op_Equality" inlines)
                    "op_Equality body sourced from ops-platform.clr.fs"

                Expect.isTrue
                    (Map.containsKey "op_Inequality" inlines)
                    "op_Inequality body sourced from ops-platform.clr.fs"

                // Each body is an `inline` curried lambda over a static optimization
                // (the `(# \"ceq\" … #)` per-primitive clauses + the fall-clause base).
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
                // `x` and `y` are two *distinct* heap instances with equal payloads;
                // the static-opt base routes `^T = Tag` to
                // `EqualityComparer<Tag>.Default.Equals(x, y)` — structural — so
                // `x = y` is true. A reference `ceq` would give false, so this asserts
                // the comparer path, not just "doesn't crash".
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

                let _, artifact = compileSource "OpEqDUStructural" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n0"
                    "distinct-but-equal DU pair compares structurally (true), not by reference"
            }

            // `ignore` on a NON-unit value: FSharp.Core `ignore : 'T -> unit` has no
            // emit recipe, so `expr |> ignore` once crashed codegen with "no call
            // recipe for external 'ignore'". A saturated application now lowers to the
            // `let _ = expr` shape (eval + pop + reify unit). The side effect (the
            // `printfn`) must still run, proving the arg is evaluated, not elided.
            test "`expr |> ignore` on a non-unit value evaluates the arg and discards it" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (x: int) : int ="
                            "    printfn \"side %d\" x"
                            "    x + 1"
                            "f 41 |> ignore"
                            "ignore (f 7)" // the un-piped form, same head + arguments
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "IgnoreNonUnit" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "side 41\nside 7\ndone"
                    "both ignore forms run the side effect and discard the int result"
            }

            test "an eta'd `(+)` splices the contract body PRE-freeze (a lambda over `add`, no External op_Addition)" {
                // `List.fold (+) 0 xs` pins `(+)` to `int -> int -> int` from `0` and the
                // element type, so `InlineExpansion`'s eta (`fun x y -> x + y`) grounds
                // `^T := int`, the `when ^T : int` clause selects, and `(# "add" #)`
                // survives. Asserted on the FROZEN decls, so the `add` is provably the
                // contract body's own clause — a surviving `External(op_Addition)` here
                // would mean the eta ran too late for the splice to reach it.
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

                // The spliced `add` must sit inside the eta'd closure — the folder value
                // `List.fold` receives — not merely somewhere in the decl. (The inline
                // body's own parameters survive as the `Let`s beta-reduction leaves, so
                // the `add` is below the inner lambda, not directly its body.)
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

            test "`List.fold (+) 0 [1; 2; 3]` runs to 6 through the eta'd contract body" {
                runs "6" "printfn \"%d\" (List.fold (+) 0 [1; 2; 3])"
            }

            test "an eta-reachable `=` over a DU is STRUCTURAL (the comparer base, not a reference `ceq`)" {
                // `(=) (Tag 1)` reaches `List.filter` as a function value; the operator's
                // body is spliced with `^T := Tag`, which selects no primitive clause and
                // falls to `EqualityComparer<Tag>.Default.Equals`. The list holds two
                // DISTINCT heap instances equal to `Tag 1`, so a reference `ceq` would
                // count 0 — the structural base counts 2.
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
                // No new codegen mechanism: freeze hands `EmitClosures` a plain lambda,
                // which closure-converts exactly as a hand-written `fun x y -> x + y`
                // would — a curried `Vesper.Fun`2` pair whose innermost `Invoke` carries
                // the spliced `add` opcode (CIL 0x58) directly, with no call out to an
                // operator. The load-bearing half is the TAST assertion above (which pins
                // WHERE the `add` came from); this pins that the pre-freeze eta did not
                // cost the backend anything.
                let _, artifact =
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
