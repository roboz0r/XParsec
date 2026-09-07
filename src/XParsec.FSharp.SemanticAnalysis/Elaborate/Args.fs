namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals

// Argument-peeling and small literal helpers for the Elaborate pass.

module internal ElaborateExprArgs =

    /// The recursive expression translator, passed in: the `Elaborate/` helper modules
    /// compile ahead of it, so a helper that must recurse takes it as a parameter.
    type TranslateExpr = PassContext -> Expr<SyntaxToken> -> TExpr

    /// Round-paren `( … )` or `begin … end` — the only enclosures that GROUP a value
    /// expression and so collapse into a call's argument list. `[ … ]` / `[| … |]` /
    /// `{ … }` / `{| … |}` are literal values: one argument each, translated whole.
    let private (|ValueParen|_|) (lParen: ParenKind<SyntaxToken>) =
        match lParen with
        | ParenKind.Paren _
        | ParenKind.BeginEnd _ -> Some()
        | _ -> None

    /// Peel one argument expression (the `HighPrecedenceApp` / `Expr.New` form) into the
    /// declared argument list: a parenthesised `Tuple` (`Point(3, 4)`) becomes one argument
    /// per component, `Point()` becomes none, and `Point(())` becomes one unit argument, as
    /// F# reads it.
    let peelOneArg (translate: Expr<SyntaxToken> -> TExpr) (arg: Expr<SyntaxToken>) : Block<TExpr> =
        match arg with
        | Expr.EnclosedBlock(lParen = ValueParen; expr = Expr.Tuple(exprs = items)) ->
            Block.ofSeq (seq { for a in items -> translate a })
        | Expr.Tuple(exprs = items) -> Block.ofSeq (seq { for a in items -> translate a })
        | Expr.EnclosedBlock(lParen = ValueParen; expr = inner) -> Block.singleton (translate inner)
        | Expr.EmptyBlock(lParen = ValueParen) -> Block.empty
        | a -> Block.singleton (translate a)

    /// `peelOneArg` for the `Expr.App` form, whose argument list the parser may already have
    /// split.
    let peelCtorArgs (translate: Expr<SyntaxToken> -> TExpr) (args: ImmutableArray<Expr<SyntaxToken>>) : Block<TExpr> =
        match args.Length with
        | 1 -> peelOneArg translate args.[0]
        | _ -> Block.ofSeq (seq { for a in args -> translate a })

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
