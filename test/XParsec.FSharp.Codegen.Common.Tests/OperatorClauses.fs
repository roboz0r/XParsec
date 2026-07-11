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

        let rec walkExpr (e: TExpr) =
            match e with
            | TExpr.ILIntrinsic(opCode, _, args, _, _) ->
                acc.Add opCode

                for a in args do
                    walkExpr a
            | TExpr.StaticOptimization(clauses, dflt, _, _) ->
                for c in clauses do
                    walkExpr c.Body

                walkExpr dflt
            | TExpr.Lambda(_, b, _, _) -> walkExpr b
            | _ -> ()

        match body.Decl with
        | TDecl.Let(_, v, _, _) -> walkExpr v
        | _ -> ()

        List.ofSeq acc

    /// The primitive widths this body's static-optimization clauses select on — i.e.
    /// exactly the types the operator SUPPORTS on the backend the body came from.
    ///
    /// Each `when ^T1 : byte and ^T2 : byte and ^T3 : byte` clause carries one
    /// `TyconEquals` constraint per typar, all naming the same primitive, so a clause
    /// contributes one width however many typars its operator publishes. The projection
    /// runs the required type through `RuntimeNames.canonicalPrimitiveName`, the same
    /// relation `Inline.staticOptTypesMatch` selects a clause by — asking "which widths
    /// does this clause set cover" by any other rule would answer a question the compiler
    /// is not asking.
    let clauseWidths (body: InlineBody) : Set<string> =
        let acc = ResizeArray<string>()

        let widthOf (t: SemType) =
            match t with
            | TyConst(key, _) -> Some(RuntimeNames.canonicalPrimitiveName (SymbolKeyOps.intrinsicName key))
            // A clause gated on a typar (`when ^T : ^T`, the user catch-all) or on a
            // structural type pins no width; the arithmetic contract writes neither.
            | _ -> None

        let rec walkExpr (e: TExpr) =
            match e with
            | TExpr.StaticOptimization(clauses, _, _, _) ->
                for c in clauses do
                    for k in c.Constraints do
                        match k with
                        | TStaticOptConstraint.TyconEquals(_, required) ->
                            match widthOf required with
                            | Some w -> acc.Add w
                            | None -> ()
                        | TStaticOptConstraint.IsStruct _ -> ()
            | TExpr.Lambda(_, b, _, _) -> walkExpr b
            | _ -> ()

        match body.Decl with
        | TDecl.Let(_, v, _, _) -> walkExpr v
        | _ -> ()

        Set.ofSeq acc

/// The guard: the clause sets and the conformance manifest must state the same
/// width→backend support matrix.
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

    /// The manifest's `operators` default: a width supports every arithmetic operator
    /// unless it says otherwise (`string` does).
    let private allOperators = operators |> List.map fst |> Set.ofList

    /// What the manifest says about one width, merged over every program that names it.
    type private WidthSupport =
        {
            /// Backends that must RUN a program at this width — so they must HAVE a clause.
            Run: Set<string>
            /// Backends that must REJECT one — so they must have NO clause.
            Diagnose: Set<string>
            Operators: Set<string>
        }

    /// The width→backend arithmetic-support matrix, read off the manifest. This is the
    /// answer to "which types support arithmetic, where" — stated in one place, by the
    /// same file that pins the behaviour, rather than inferred from two clause lists.
    let private matrix: Map<string, WidthSupport> =
        (Map.empty, programs)
        ||> List.fold (fun m p ->
            match p.Covers with
            | None -> m
            | Some covers ->
                let ofObligation (run, diagnose) (backend, ob) =
                    match ob with
                    // A program that must FAULT still RUNS (integer `/` by zero throws):
                    // its width needs a clause exactly as a completing one does.
                    | Obligation.Run
                    | Obligation.Fault _ -> Set.add backend run, diagnose
                    | Obligation.Diagnose _ -> run, Set.add backend diagnose

                let run, diagnose =
                    p.Obligations |> Map.toList |> List.fold ofObligation (Set.empty, Set.empty)

                let here =
                    {
                        Run = run
                        Diagnose = diagnose
                        Operators = defaultArg covers.Operators allOperators
                    }

                let merged =
                    match Map.tryFind covers.Width m with
                    | None -> here
                    | Some prev ->
                        {
                            Run = Set.union prev.Run here.Run
                            Diagnose = Set.union prev.Diagnose here.Diagnose
                            Operators = Set.union prev.Operators here.Operators
                        }

                Map.add covers.Width merged m
        )

    let private widthsWhere (predicate: WidthSupport -> bool) : Set<string> =
        matrix |> Map.filter (fun _ s -> predicate s) |> Map.keys |> Set.ofSeq

    /// One backend's clause sets against the matrix. `bodies` is that backend's operator
    /// contract as its own symbol leaf resolves it (compiled name → spliced body).
    let tests (backendName: string) (bodies: Map<string, InlineBody>) : Test =
        testList
            (sprintf "Operator clause parity (%s)" backendName)
            [
                test "the manifest never both runs and rejects the same width" {
                    let contradictions =
                        matrix
                        |> Map.toList
                        |> List.choose (fun (width, s) ->
                            let both = Set.intersect s.Run s.Diagnose

                            if Set.isEmpty both then
                                None
                            else
                                Some(width, Set.toList both)
                        )

                    Expect.isEmpty
                        contradictions
                        "a width cannot be one a backend must run AND one it must reject; the matrix would say both"
                }

                for symbol, compiled in operators do
                    test symbol {
                        let body =
                            match Map.tryFind compiled bodies with
                            | Some b -> b
                            | None -> failtestf "the %s contract publishes no `%s` inline body" backendName compiled

                        let actual = InlineBodies.clauseWidths body

                        let expected =
                            widthsWhere (fun s -> Set.contains backendName s.Run && Set.contains symbol s.Operators)

                        let extra = Set.difference actual expected
                        let missing = Set.difference expected actual

                        // The widths ARE the content of a failure here, so every message
                        // names them: which clause to write, or which to delete.
                        let says (verdict: string) (widths: Set<string>) =
                            sprintf "the %s `%s` contract %s: %A" backendName symbol verdict (Set.toList widths)

                        // A width the backend must RUN with no clause is a program that
                        // is a COMPILE ERROR on that backend — the corpus would go red,
                        // but only for the widths someone wrote a program for.
                        Expect.isEmpty
                            missing
                            (says
                                "has NO clause for widths the manifest says it must run — they fall to the SRTP base and fail to resolve"
                                missing)

                        // THE FALSE-PRECISION REGRESSION, mechanically. A width the
                        // manifest says this backend must REJECT, given a clause: it now
                        // compiles and emits whatever that clause says, and no corpus
                        // program is looking (`diagnose` rows assert on the compile error,
                        // which just disappeared).
                        let mustReject =
                            extra
                            |> Set.filter (fun w ->
                                match Map.tryFind w matrix with
                                | Some s -> Set.contains backendName s.Diagnose
                                | None -> false
                            )

                        Expect.isEmpty
                            mustReject
                            (says
                                "has a clause for widths the manifest says it must REJECT — the clause makes them compile, and emit"
                                mustReject)

                        // An UNPINNED clause: emitted code that no conformance program
                        // exercises and no golden judges. Failing rather than reporting is
                        // the same doctrine as the rest of this corpus — a clause whose
                        // answer nothing checks is exactly the state that let ~8 wrong JS
                        // widths read as coverage — and the fix is cheap either way: add
                        // the program, or delete the clause.
                        let unpinned = extra |> Set.filter (fun w -> not (Map.containsKey w matrix))

                        Expect.isEmpty
                            unpinned
                            (says
                                "has a clause for widths the manifest never mentions — it emits code no conformance program judges"
                                unpinned)

                        // What is left over: a width in the matrix, run by this backend,
                        // but whose declared operator set excludes this operator — a
                        // `string` `(-)` clause, say.
                        let unsupported = extra - mustReject - unpinned

                        Expect.isEmpty
                            unsupported
                            (says "has a clause for widths whose manifest `operators` exclude it" unsupported)
                    }
            ]
