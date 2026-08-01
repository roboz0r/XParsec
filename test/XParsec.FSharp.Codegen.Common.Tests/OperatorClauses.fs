/// The STRUCTURAL half of backend conformance: which primitives the operator contract
/// says support each arithmetic operator, checked against the conformance manifest
/// without compiling or running anything.
///
/// Each primitive DECLARES its own operator surface — `static member (+)` on
/// `prim-types-min.fsi`'s `int`, and so on across the widths. That declaration set IS the
/// definition of which types support arithmetic: a width that declares nothing falls to
/// the SRTP trait call, resolves no member, and becomes a compile error ("does not support
/// the operator"). The declarations are target-neutral; what makes the two backends
/// differ is representability, which is why this guard enumerates from the target's own
/// repr extraction.
///
/// The conformance corpus pins the BEHAVIOUR. What it cannot cheaply catch is a
/// declaration ADDED for a width the manifest says must be rejected — the false-precision
/// failure: a declaration that reads as coverage and computes a wrong answer. The corpus
/// would only notice if someone also thought to write a program for it. Reading the
/// declared set off the contract and diffing it against the manifest catches that
/// mechanically, and costs neither Node nor CIL.
namespace XParsec.FSharp.Codegen.Common.Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis

/// Reading facts back off a spliced body. Shared because BOTH backend suites read the
/// same contract through their own symbol leaf — `Codegen.Clr.Tests` via
/// `ClrSymbolProviders` (the BCL metadata leaf), `Codegen.Js.Tests` via `JsNativeSymbols`
/// (the JS-native leaf, with no dependency on the CLR backend). The leaf differs; what is
/// read off the body does not.
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

    /// The spliced body of ONE primitive's operator member, as this contract serves it —
    /// the per-width template that used to be a clause on the operator. A member on an
    /// intrinsic is never emitted, so this body is the only artifact the width's
    /// arithmetic has.
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
        ]

    /// The manifest's `operators` default: a program's obligations are about every
    /// arithmetic operator unless it narrows them.
    let private allOperators = operators |> List.map fst |> Set.ofList

    /// What the manifest owes for ONE (width, operator) pair, merged over every program
    /// that names it.
    type private Support =
        {
            /// Backends that must RUN that operator at that width — so that width must
            /// DECLARE it.
            Run: Set<string>
            /// Backends that must REJECT it — so it must declare nothing.
            Diagnose: Set<string>
        }

    /// The (width × operator) → backend arithmetic-support matrix, read off the manifest.
    /// This is the answer to "which types support which arithmetic, where" — stated in one
    /// place, by the same file that pins the behaviour, rather than inferred from two
    /// hand-written lists.
    ///
    /// The PAIR is the key, not the width: `byte` must run `+` on both backends and must
    /// be rejected for `~-` on both, and a width-keyed matrix could hold only one of those
    /// two facts. (It held the wrong one: `~-` defaulted to supported at every width that
    /// supported `+`, so the guard demanded the very unsigned negations that answered 56
    /// on JS and -200 on the CLR.)
    let private matrix: Map<string * string, Support> =
        (Map.empty, programs)
        ||> List.fold (fun m p ->
            match p.Covers with
            | None -> m
            | Some covers ->
                let ofObligation (run, diagnose) (backend, ob) =
                    match ob with
                    // A program that must FAULT still RUNS (integer `/` by zero throws):
                    // its pair needs a declaration exactly as a completing one does.
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

    /// The primitives that DECLARE `compiled` as a static member on this contract.
    ///
    /// Two axes meet here and only one is per-target. The DECLARATION is target-neutral —
    /// `static member (+)` is written once, in the `.fsi`. What varies is
    /// REPRESENTABILITY: `IntrinsicForwardRepr` is extracted from the target's own
    /// `(# … #)` bindings, so a primitive the target has no repr for (`nativeint` on JS)
    /// is not in it at all and its declarations are unreachable rather than separately
    /// gated. Enumerating from that map is therefore what makes the two backends' answers
    /// differ, and it is the ONE mechanism by which target-dependence enters.
    let private widthsDeclaring (provider: IExternalSymbolProvider) (compiled: string) : Set<string> =
        set
            [
                for kv in provider.IntrinsicForwardRepr do
                    match provider.TryLookupMember(kv.Key, compiled) with
                    | ValueSome m when m.IsStatic -> yield SymbolKeyOps.intrinsicName kv.Key
                    | _ -> ()
            ]

    /// One backend's operator surface against the matrix. `provider` is that backend's
    /// contract as its own symbol leaf resolves it: a width is supported iff it DECLARES
    /// the member. There is no second term — no operator carries a clause list any more.
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

                        // The widths ARE the content of a failure here, so every message
                        // names them: which declaration to write, or which to delete.
                        let says (verdict: string) (widths: Set<string>) =
                            sprintf "the %s `%s` surface %s: %A" backendName symbol verdict (Set.toList widths)

                        // A width the backend must RUN this operator at, declaring nothing,
                        // is a program that is a COMPILE ERROR on that backend — the corpus
                        // would go red, but only for the pairs someone wrote a program for.
                        Expect.isEmpty
                            missing
                            (says
                                "is MISSING widths the manifest says it must run — they fall to the SRTP trait call and resolve no member"
                                missing)

                        // THE FALSE-PRECISION REGRESSION, mechanically. A pair the manifest
                        // says this backend must REJECT, given a declaration: it now compiles
                        // and emits whatever the body says, and no corpus program is looking
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
                                "declares widths the manifest says it must REJECT at this operator — the declaration makes them compile, and emit"
                                mustReject)

                        // An UNPINNED declaration: emitted code that no conformance program
                        // exercises and no golden judges. Failing rather than reporting is
                        // the same doctrine as the rest of this corpus — a body whose answer
                        // nothing checks is exactly the state that let ~8 wrong JS widths read
                        // as coverage, and then let unsigned `~-` do it again one axis over —
                        // and the fix is cheap either way: add the program, or delete the
                        // declaration.
                        let unpinned = extra |> Set.filter (fun w -> not (Map.containsKey w column))

                        Expect.isEmpty
                            unpinned
                            (says
                                "declares widths the manifest never pairs with this operator — it emits code no conformance program judges"
                                unpinned)

                        // What is left over: a pair in the matrix that this backend is named
                        // in neither half of — it owes the operator nothing at that width,
                        // yet declares it.
                        let unsupported = extra - mustReject - unpinned

                        Expect.isEmpty
                            unsupported
                            (says "declares widths the manifest gives it no obligation at" unsupported)
                    }
            ]
