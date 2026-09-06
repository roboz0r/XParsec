/// The STRUCTURAL half of backend conformance: which primitives the operator contract says
/// support each arithmetic operator, diffed against the manifest without compiling anything.
/// It catches a declaration added for a width the manifest says must be REJECTED.
namespace XParsec.FSharp.Codegen.Common.Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis

/// Reading facts back off a spliced body. Shared because both backend suites read the same
/// contract through their own platform metadata, which differs where what is read off the
/// body does not.
module InlineBodies =

    /// Every `ILIntrinsic` opCode string reachable in the body — a CIL mnemonic (`add`)
    /// on the CLR contract, a `$N` JS-expression template (`($0 + $1) | 0`) on the JS one.
    let ilOpCodes (body: InlineBody) : string list =
        let acc = ResizeArray<string>()

        let rec walkExpr (e: Wire.TExpr) =
            match e with
            | TExprG.ILIntrinsic(opCode, _, args, _, _) ->
                acc.Add opCode

                for a in args do
                    walkExpr a
            | TExprG.StaticOptimization(clauses, dflt, _, _) ->
                for c in clauses do
                    walkExpr c.Body

                walkExpr dflt
            | TExprG.Lambda(_, b, _, _) -> walkExpr b
            | _ -> ()

        match body.Decl with
        | TDeclG.Let(_, v, _, _, _) -> walkExpr v
        | _ -> ()

        List.ofSeq acc

    /// The spliced body of ONE primitive's operator member, as this contract serves it.
    let operatorBody (provider: IExternalSymbolProvider) (width: string) (compiled: string) : InlineBody =
        match provider.TryLookupMember(RuntimeNames.primitiveKey width, compiled) with
        | ValueSome m ->
            match m.InlineBody with
            | ValueSome b -> b
            | ValueNone -> failtestf "`%s` declares `%s` but the contract serves no body for it" width compiled
        | ValueNone -> failtestf "`%s` declares no `%s`" width compiled

/// The guard: the declared operator surface and the conformance manifest must state the
/// same (width × operator) → backend support matrix.
module OperatorSurfaceParity =

    open XParsec.FSharp.Codegen.Common.Tests.Conformance

    /// The arithmetic operators whose declaration set is the arithmetic-support
    /// definition: the manifest's spelling → the compiled name the contract publishes.
    let operators: (string * string) list =
        [
            "+", "op_Addition"
            "-", "op_Subtraction"
            "*", "op_Multiply"
            "/", "op_Division"
            "%", "op_Modulus"
            "~-", "op_UnaryNegation"
            "~+", "op_UnaryPlus"
        ]

    /// The manifest's `operators` default: a program's obligations are about every
    /// arithmetic operator unless it narrows them.
    let private allOperators = operators |> List.map fst |> Set.ofList

    /// What the manifest owes for ONE (width, operator) pair, merged over every program
    /// that references it.
    type private Support =
        {
            /// Backends that must RUN that operator at that width — so that width must
            /// DECLARE it.
            Run: Set<string>
            /// Backends that must REJECT it — so it must declare nothing.
            Diagnose: Set<string>
        }

    /// The (width × operator) → backend arithmetic-support matrix, read off the manifest.
    /// The PAIR is the key, not the width: `byte` must run `+` on both backends and be
    /// rejected for `~-` on both, and a width-keyed matrix holds only one of those.
    let private matrix: Map<string * string, Support> =
        (Map.empty, programs)
        ||> List.fold (fun m p ->
            match p.Covers with
            | None -> m
            | Some covers ->
                let ofObligation (run, diagnose) (backend, ob) =
                    match ob with
                    // A program that must FAULT still RUNS (integer `/` by zero throws), and
                    // an `accept` one still COMPILES: each needs the operator declared at
                    // that width exactly as a completing program does.
                    | Obligation.Run _
                    | Obligation.Fault _
                    | Obligation.Accept -> Set.add backend run, diagnose
                    | Obligation.Diagnose _ -> run, Set.add backend diagnose

                let run, diagnose =
                    p.Obligations |> Map.toList |> List.fold ofObligation (Set.empty, Set.empty)

                (m, defaultArg covers.Operators allOperators)
                ||> Set.fold (fun m op ->
                    let key = covers.Width, op

                    let merged =
                        match Map.tryFind key m with
                        | None -> { Run = run; Diagnose = diagnose }
                        | Some prev ->
                            {
                                Run = Set.union prev.Run run
                                Diagnose = Set.union prev.Diagnose diagnose
                            }

                    Map.add key merged m
                )
        )

    /// The primitives that DECLARE `compiled` as a static member on this contract. The
    /// declaration is target-neutral (written once in the `.fsi`); what differs per target is
    /// which primitives it BINDS A REPR for (`nativeint` is unsupported on JS).
    let private widthsDeclaring (provider: IExternalSymbolProvider) (compiled: string) : Set<string> =
        set
            [
                for entry in IntrinsicTypeMap.entries provider.IntrinsicTypeMap do
                    match entry.Platform with
                    | IntrinsicPlatform.Bound _ ->
                        match provider.TryLookupMember(entry.Canon, compiled) with
                        | ValueSome m when m.IsStatic -> yield entry.Canon.Name
                        | _ -> ()
                    | IntrinsicPlatform.Unsupported _ -> ()
            ]

    /// One backend's operator surface against the matrix. `provider` is that backend's
    /// contract as its own platform metadata resolves it: a width is supported iff it DECLARES
    /// the member.
    let tests (backendName: string) (provider: IExternalSymbolProvider) : Test =
        testList
            (sprintf "Operator surface parity (%s)" backendName)
            [
                test "the manifest never both runs and rejects the same (width, operator)" {
                    let contradictions =
                        matrix
                        |> Map.toList
                        |> List.choose (fun (pair, s) ->
                            let both = Set.intersect s.Run s.Diagnose

                            if Set.isEmpty both then
                                None
                            else
                                Some(pair, Set.toList both)
                        )

                    Expect.isEmpty
                        contradictions
                        "an operator at a width cannot be one a backend must run AND one it must reject; the matrix would say both"
                }

                for symbol, compiled in operators do
                    test symbol {
                        let actual = widthsDeclaring provider compiled

                        // This operator's column of the matrix: what each width owes for it.
                        let column =
                            matrix
                            |> Map.toList
                            |> List.choose (fun ((width, op), s) -> if op = symbol then Some(width, s) else None)
                            |> Map.ofList

                        let expected =
                            column
                            |> Map.filter (fun _ s -> Set.contains backendName s.Run)
                            |> Map.keys
                            |> Set.ofSeq

                        let extra = Set.difference actual expected
                        let missing = Set.difference expected actual

                        let says (verdict: string) (widths: Set<string>) =
                            sprintf "the %s `%s` surface %s: %A" backendName symbol verdict (Set.toList widths)

                        // A width the backend must RUN this operator at, declaring nothing, is
                        // a COMPILE ERROR in any program that uses the pair.
                        Expect.isEmpty
                            missing
                            (says
                                "is MISSING widths the manifest says it must run — they fall to the SRTP trait call and resolve no member"
                                missing)

                        // A pair the manifest says this backend must REJECT, given a
                        // declaration: it compiles and emits whatever the body says, and the
                        // `diagnose` row now asserts on a compile error that never arrives.
                        let mustReject =
                            extra
                            |> Set.filter (fun w ->
                                match Map.tryFind w column with
                                | Some s -> Set.contains backendName s.Diagnose
                                | None -> false
                            )

                        Expect.isEmpty
                            mustReject
                            (says
                                "declares widths the manifest says it must REJECT at this operator — the declaration makes them compile, and emit"
                                mustReject)

                        // An UNPINNED declaration: emitted code no conformance program
                        // exercises and no golden judges. The fix is to add the program or
                        // delete the declaration.
                        let unpinned = extra |> Set.filter (fun w -> not (Map.containsKey w column))

                        Expect.isEmpty
                            unpinned
                            (says
                                "declares widths the manifest never pairs with this operator — it emits code no conformance program judges"
                                unpinned)

                        // What is left over: a pair where this backend is in neither the run nor
                        // the reject half, so it owes nothing there yet declares it.
                        let unsupported = extra - mustReject - unpinned

                        Expect.isEmpty
                            unsupported
                            (says "declares widths the manifest gives it no obligation at" unsupported)
                    }
            ]
