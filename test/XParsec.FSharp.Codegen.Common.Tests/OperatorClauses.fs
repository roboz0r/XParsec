/// The STRUCTURAL half of backend conformance: the operator contract's clause sets,
/// checked against the conformance manifest without compiling or running anything.
///
/// `Vesper.Core/ops-platform.fs` (CLR) and `ops-platform.js.fs` (JS) each enumerate,
/// per arithmetic operator, one static-optimization clause per supported primitive.
/// That clause list IS the definition of which types support arithmetic on that
/// backend: a width with no clause falls to the SRTP trait call in the base, fails to
/// resolve a trait member, and becomes a compile error ("does not support the
/// operator"). The two files are written independently, in two dialects (CIL mnemonics
/// / JS templates), with nothing that made them agree with each other or with reality.
///
/// The conformance corpus pins the BEHAVIOUR. What it cannot cheaply catch is a clause
/// ADDED for a width the manifest says must be rejected — the false-precision failure:
/// a clause that reads as coverage and computes a wrong answer. The corpus would only
/// notice if someone also thought to write a program for it. Reading the clause set off
/// the frozen body and diffing it against the manifest catches that mechanically, and
/// costs neither Node nor CIL.
namespace XParsec.FSharp.Codegen.Common.Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis

/// Reading facts back off a spliced `let inline` body. Shared because BOTH backend
/// suites harvest the same contract through their own symbol leaf — `Codegen.Clr.Tests`
/// via `ClrSymbolProviders.contractInlineBodiesFor` (the BCL metadata leaf),
/// `Codegen.Js.Tests` via `JsNativeSymbols.jsNativeInlineBodiesFor` (the JS-native leaf,
/// with no dependency on the CLR backend). The leaf differs; what is read off the body
/// does not.
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
        | TDeclG.Let(_, v, _, _) -> walkExpr v
        | _ -> ()

        List.ofSeq acc

    /// The primitive widths this body's static-optimization clauses select on — i.e.
    /// exactly the types the operator SUPPORTS on the backend the body came from.
    ///
    /// Each `when ^T1 : byte and ^T2 : byte and ^T3 : byte` clause carries one
    /// `TyconEquals` constraint per typar, all naming the same primitive, so a clause
    /// contributes one width however many typars its operator publishes. The width is the
    /// required type's bare intrinsic name — the same thing `Inline.staticOptTypesMatch`
    /// selects a clause by, and for the same reason it needs no canonicalisation: an
    /// intrinsic abbreviation (`int32`, `single`) is expanded at name resolution, so a
    /// clause's required type is already canonical by the time it is frozen into the body.
    let clauseWidths (body: InlineBody) : Set<string> =
        let acc = ResizeArray<string>()

        let widthOf (t: FrozenType) =
            match t with
            | FTConst(key, _) -> Some(SymbolKeyOps.intrinsicName key)
            // A clause gated on a typar (`when ^T : ^T`, the user catch-all) or on a
            // structural type pins no width; the arithmetic contract writes neither.
            | _ -> None

        let rec walkExpr (e: Wire.TExpr) =
            match e with
            | TExprG.StaticOptimization(clauses, _, _, _) ->
                for c in clauses do
                    for k in c.Constraints do
                        match k with
                        | TStaticOptConstraintG.TyconEquals(_, required) ->
                            match widthOf required with
                            | Some w -> acc.Add w
                            | None -> ()
                        | TStaticOptConstraintG.IsStruct _ -> ()
            | TExprG.Lambda(_, b, _, _) -> walkExpr b
            | _ -> ()

        match body.Decl with
        | TDeclG.Let(_, v, _, _) -> walkExpr v
        | _ -> ()

        Set.ofSeq acc

/// The guard: the clause sets and the conformance manifest must state the same
/// (width × operator) → backend support matrix.
module OperatorClauseParity =

    open XParsec.FSharp.Codegen.Common.Tests.Conformance

    /// The arithmetic operators whose clause set is the arithmetic-support definition:
    /// the manifest's spelling → the compiled name the contract publishes.
    let operators: (string * string) list =
        [
            "+", "op_Addition"
            "-", "op_Subtraction"
            "*", "op_Multiply"
            "/", "op_Division"
            "%", "op_Modulus"
            "~-", "op_UnaryNegation"
        ]

    /// The manifest's `operators` default: a program's obligations are about every
    /// arithmetic operator unless it narrows them.
    let private allOperators = operators |> List.map fst |> Set.ofList

    /// What the manifest owes for ONE (width, operator) pair, merged over every program
    /// that names it.
    type private Support =
        {
            /// Backends that must RUN that operator at that width — so they must HAVE
            /// a clause for it.
            Run: Set<string>
            /// Backends that must REJECT it — so they must have NO clause.
            Diagnose: Set<string>
        }

    /// The (width × operator) → backend arithmetic-support matrix, read off the manifest.
    /// This is the answer to "which types support which arithmetic, where" — stated in one
    /// place, by the same file that pins the behaviour, rather than inferred from two
    /// clause lists.
    ///
    /// The PAIR is the key, not the width: `byte` must run `+` on both backends and must
    /// be rejected for `~-` on both, and a width-keyed matrix could hold only one of those
    /// two facts. (It held the wrong one: `~-` defaulted to supported at every width that
    /// supported `+`, so the guard demanded the very unsigned negation clauses that
    /// answered 56 on JS and -200 on the CLR.)
    let private matrix: Map<string * string, Support> =
        (Map.empty, programs)
        ||> List.fold (fun m p ->
            match p.Covers with
            | None -> m
            | Some covers ->
                let ofObligation (run, diagnose) (backend, ob) =
                    match ob with
                    // A program that must FAULT still RUNS (integer `/` by zero throws):
                    // its pair needs a clause exactly as a completing one does.
                    | Obligation.Run
                    | Obligation.Fault _ -> Set.add backend run, diagnose
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

    /// One backend's clause sets against the matrix. `bodies` is that backend's operator
    /// contract as its own symbol leaf resolves it (compiled name → spliced body).
    let tests (backendName: string) (bodies: Map<string, InlineBody>) : Test =
        testList
            (sprintf "Operator clause parity (%s)" backendName)
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
                        let body =
                            match Map.tryFind compiled bodies with
                            | Some b -> b
                            | None -> failtestf "the %s contract publishes no `%s` inline body" backendName compiled

                        let actual = InlineBodies.clauseWidths body

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

                        // The widths ARE the content of a failure here, so every message
                        // names them: which clause to write, or which to delete.
                        let says (verdict: string) (widths: Set<string>) =
                            sprintf "the %s `%s` contract %s: %A" backendName symbol verdict (Set.toList widths)

                        // A width the backend must RUN this operator at, with no clause, is
                        // a program that is a COMPILE ERROR on that backend — the corpus
                        // would go red, but only for the pairs someone wrote a program for.
                        Expect.isEmpty
                            missing
                            (says
                                "has NO clause for widths the manifest says it must run — they fall to the SRTP base and fail to resolve"
                                missing)

                        // THE FALSE-PRECISION REGRESSION, mechanically. A pair the manifest
                        // says this backend must REJECT, given a clause: it now compiles and
                        // emits whatever that clause says, and no corpus program is looking
                        // (`diagnose` rows assert on the compile error, which just
                        // disappeared).
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
                                "has a clause for widths the manifest says it must REJECT at this operator — the clause makes them compile, and emit"
                                mustReject)

                        // An UNPINNED clause: emitted code that no conformance program
                        // exercises and no golden judges. Failing rather than reporting is
                        // the same doctrine as the rest of this corpus — a clause whose
                        // answer nothing checks is exactly the state that let ~8 wrong JS
                        // widths read as coverage, and then let unsigned `~-` do it again
                        // one axis over — and the fix is cheap either way: add the program,
                        // or delete the clause.
                        let unpinned = extra |> Set.filter (fun w -> not (Map.containsKey w column))

                        Expect.isEmpty
                            unpinned
                            (says
                                "has a clause for widths the manifest never pairs with this operator — it emits code no conformance program judges"
                                unpinned)

                        // What is left over: a pair in the matrix that this backend is named
                        // in neither half of — it owes the operator nothing at that width,
                        // yet carries a clause.
                        let unsupported = extra - mustReject - unpinned

                        Expect.isEmpty
                            unsupported
                            (says "has a clause for widths the manifest gives it no obligation at" unsupported)
                    }
            ]
