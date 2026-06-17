namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// CLR-specific lowering: the built-in-operator finish pass (`expandBuiltinOps`),
/// plus re-exports of the platform-neutral lowering utilities that moved down to
/// `SemanticAnalysis.TastLower` (so both codegen backends share them without
/// referencing each other). Existing CLR call sites keep using `EmitLower.*`.
module EmitLower =

    let typeOfExpr = TastLower.typeOfExpr
    let typeOfPat = TastLower.typeOfPat
    // `inline` so the call sites keep `TastLower.receiverShape`'s inlining (a plain
    // re-export `let` would demote it to an allocated function value).
    let inline receiverShape ty = TastLower.receiverShape ty
    let matchInstantiation = TastLower.matchInstantiation
    let iterChildren = TastLower.iterChildren
    let mintUnitParamKey = TastLower.mintUnitParamKey
    let mintTupleParamKey = TastLower.mintTupleParamKey
    let mintUseBinderKey = TastLower.mintUseBinderKey
    let peelLambda = TastLower.peelLambda

    /// Fallback inline-IL bodies for built-in operators, expressed as the
    /// `TExprG.ILIntrinsic` the general path emits. A saturated `External(opName)`
    /// use site is rewritten to the matching body here by `expandBuiltinOps`, so
    /// codegen owns no per-operator dispatch. Bodies are monomorphic at the
    /// use-site type (primitive clauses share an opcode; no `when ^T : …`).
    ///
    /// DELETE-WHEN-COMPLETE: operator `.fs` bodies in `ops-platform.fs` now win on
    /// the primary path (the `=`/`<>` contract body is spliced pre-freeze by
    /// `Passes.InlineExpansion` at every ground use site). This table only still
    /// serves the residue the pass leaves: an *un-ground* operator operand
    /// (`let f a b = a = b`) and an eta-reified operator value, until those route
    /// through the inline bodies too.
    module private BuiltinOps =

        let private ilBin (op: string) : EqArray<Frozen.TExpr> -> FrozenType -> SyntaxToken -> Frozen.TExpr =
            fun operands retTy tok -> TExprG.ILIntrinsic(op, ValueNone, operands, retTy, tok)

        /// `not (# op … #)`, realised as `ceq (# op … #) false` — the derived ops
        /// with no direct opcode (`<>` = `not =`, `<=` = `not >`, `>=` = `not <`).
        let private ilBinNot (op: string) : EqArray<Frozen.TExpr> -> FrozenType -> SyntaxToken -> Frozen.TExpr =
            fun operands retTy tok ->
                let inner = TExprG.ILIntrinsic(op, ValueNone, operands, retTy, tok)

                TExprG.ILIntrinsic(
                    "ceq",
                    ValueNone,
                    EqArray.ofList [ inner; TExprG.Const(TConstValue.Bool false, retTy, tok) ],
                    retTy,
                    tok
                )

        /// compiled name → (arity, body builder over the operand expressions).
        /// `&&` / `||` are intentionally absent — they short-circuit and freeze to
        /// `IfThenElse`, not an opcode. Ordering uses `clt`/`cgt` (IEEE on floats,
        /// O7); bitwise/shift use the signed/default IL form (the `ops-platform.fs`
        /// contract bodies, with narrow-int/unsigned refinements, win at a ground
        /// use site — these serve the un-ground fallback).
        let private table: Map<string, int * (EqArray<Frozen.TExpr> -> FrozenType -> SyntaxToken -> Frozen.TExpr)> =
            Map
                [
                    "op_Equality", (2, ilBin "ceq")
                    "op_Inequality", (2, ilBinNot "ceq")
                    "op_LessThan", (2, ilBin "clt")
                    "op_GreaterThan", (2, ilBin "cgt")
                    "op_LessThanOrEqual", (2, ilBinNot "cgt")
                    "op_GreaterThanOrEqual", (2, ilBinNot "clt")
                    "op_Addition", (2, ilBin "add")
                    "op_Subtraction", (2, ilBin "sub")
                    "op_Multiply", (2, ilBin "mul")
                    "op_Division", (2, ilBin "div")
                    "op_Modulus", (2, ilBin "rem")
                    "op_UnaryNegation", (1, ilBin "neg")
                    "op_BitwiseAnd", (2, ilBin "and")
                    "op_BitwiseOr", (2, ilBin "or")
                    "op_ExclusiveOr", (2, ilBin "xor")
                    "op_LeftShift", (2, ilBin "shl")
                    "op_RightShift", (2, ilBin "shr")
                    "op_LogicalNot", (1, ilBin "not")
                ]

        /// True when `name` is a built-in operator applied to exactly its arity —
        /// the saturated use site rewritten to inline IL. A partial application
        /// (`(=) 1`) is left as a call head for the eta path.
        let isSaturated (name: string) (spineLen: int) : bool =
            match Map.tryFind name table with
            | Some(arity, _) -> spineLen = arity
            | None -> false

        /// Build the operator's inline-IL body, splicing the (already-rewritten)
        /// operand expressions directly. `retTy` is the application's result type.
        let buildApp
            (name: string)
            (opArgs: EqArray<Frozen.TExpr>)
            (retTy: FrozenType)
            (tok: SyntaxToken)
            : Frozen.TExpr =
            let _, makeInner = table.[name]
            makeInner opArgs retTy tok

    /// Rewrite every saturated built-in operator application to its inline-IL
    /// body, so it emits through the single `TExprG.ILIntrinsic` path. Run as the
    /// closing phase of `lower` and over type-member bodies (which never pass
    /// through `lower`). The splice is direct — each body uses each operand exactly
    /// once — so no binder is introduced and closure/free-variable analysis is
    /// undisturbed.
    let rec expandBuiltinOps (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.App _ ->
            let head, spine = TastWalk.collectSpine [] e

            match head with
            | TExprG.External(name, _, _, _) when BuiltinOps.isSaturated name (List.length spine) ->
                let _, retTy, _ = List.last spine
                let opArgs = EqArray.ofSeq (seq { for (a, _, _) in spine -> expandBuiltinOps a })
                // The saturated operator collapses to its inline-IL body; carry the
                // application node's own token onto every synthesised IL node.
                BuiltinOps.buildApp name opArgs retTy (TastWalk.exprTok e)
            | _ -> TastLower.mapChildren expandBuiltinOps e
        | _ -> TastLower.mapChildren expandBuiltinOps e

    /// The CLR backend's `lower`: the shared `TastLower.lower` with the CLR
    /// operator-finish pass (`expandBuiltinOps`, collapsing saturated operators to
    /// stack-machine `ILIntrinsic`) supplied as `finishOps`.
    let lower (decls: EqArray<Frozen.TDecl>) : Frozen.TDecl list = TastLower.lower expandBuiltinOps decls
