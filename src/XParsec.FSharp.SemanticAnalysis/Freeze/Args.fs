namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeResolve

// Argument-peeling and small literal helpers for the Freeze pass. The `peel*`
// helpers take the recursive `translateExpr` as a parameter (dependency
// injection), so they live ahead of it; `FreezeExpr` opens this module.

module internal FreezeExprArgs =

    /// Round-paren `( … )` or `begin … end` — the only enclosures that *group a
    /// value expression* and so collapse into a call's argument list. A `[ … ]`
    /// / `[| … |]` / `{ … }` / `{| … |}` enclosure is a *literal value* (list,
    /// array, record, anon-record) — a single argument — and must translate
    /// whole, never unwrap to its inner element sequence. (Unwrapping a list
    /// literal's inner `Sequential` was the PP7a gap #3: `LCat [ a; b ]` lowered
    /// the cons-list to empty because its `[ … ]` matched the bare
    /// `EnclosedBlock` arm.)
    let private (|ValueParen|_|) (lParen: ParenKind<SyntaxToken>) =
        match lParen with
        | ParenKind.Paren _
        | ParenKind.BeginEnd _ -> Some()
        | _ -> None

    /// Peel an `Expr.App` argument that may be a single `EnclosedBlock`
    /// wrapping a `Tuple` (the F# parser shape for `Point(3, 4)`) so
    /// downstream consumers see the constructor's declared arity directly.
    let peelCtorArgs
        (translate: Expr<SyntaxToken> -> TExpr)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : EqArray<TExpr> =
        if args.Length = 1 then
            match args.[0] with
            | Expr.EnclosedBlock(lParen = ValueParen; expr = Expr.Tuple(exprs = items)) ->
                EqArray.ofSeq (seq { for a in items -> translate a })
            | Expr.Tuple(exprs = items) -> EqArray.ofSeq (seq { for a in items -> translate a })
            | Expr.EnclosedBlock(lParen = ValueParen; expr = inner) -> EqArray.singleton (translate inner)
            | Expr.EmptyBlock(lParen = ValueParen) -> EqArray.empty
            | a -> EqArray.singleton (translate a)
        else
            EqArray.ofSeq (seq { for a in args -> translate a })

    /// Same as `peelCtorArgs` but for a single argument expression
    /// (HighPrecedenceApp form / Expr.New).
    let peelOneArg (translate: Expr<SyntaxToken> -> TExpr) (arg: Expr<SyntaxToken>) : EqArray<TExpr> =
        match arg with
        | Expr.EnclosedBlock(lParen = ValueParen; expr = Expr.Tuple(exprs = items)) ->
            EqArray.ofSeq (seq { for a in items -> translate a })
        | Expr.Tuple(exprs = items) -> EqArray.ofSeq (seq { for a in items -> translate a })
        | Expr.EnclosedBlock(lParen = ValueParen; expr = Expr.EmptyBlock _) -> EqArray.empty
        | Expr.EnclosedBlock(lParen = ValueParen; expr = inner) -> EqArray.singleton (translate inner)
        | Expr.EmptyBlock(lParen = ValueParen) -> EqArray.empty
        | a -> EqArray.singleton (translate a)

    /// `()` literal. Distinct from `parseConst` because `Expr.EmptyBlock`
    /// carries `ParenKind` + closing token, not a `Constant`.
    let unitConst (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        TExpr.Const(TConstValue.Unit, typeOfKey ctx key, CstKeys.firstTokenOfExpr e)

    /// `[1; 2; 3]` parses as `EnclosedBlock(ParenKind.List, Sequential [...])`;
    /// a one-item literal `[1]` skips the Sequential wrapper.
    let listLiteralItems (body: Expr<SyntaxToken>) : Expr<SyntaxToken> list =
        match body with
        | Expr.Sequential(exprs = items) -> [ for x in items -> x ]
        | single -> [ single ]
