namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch

module internal UnificationInferLiteralExpr =

    /// A `TokenIndex.Virtual` close token is one the parser inserted to recover from a
    /// missing or mismatched delimiter; without a report here the malformed literal
    /// types cleanly and the breakage never reaches a semantic-analysis consumer.
    let checkLiteralClose
        (ctx: PassContext)
        (tok: SyntaxToken)
        (rTok: SyntaxToken)
        (expected: Token)
        (display: string)
        : unit =
        match rTok.Index with
        | TokenIndex.Virtual ->
            ctx.Report(tok, Kind.Message(sprintf "Mismatched or missing closing delimiter: expected '%s'" display))
        | TokenIndex.Regular _ when rTok.Token <> expected ->
            ctx.Report(tok, Kind.Message(sprintf "Mismatched closing delimiter: expected '%s'" display))
        | TokenIndex.Regular _ -> ()

    let inferListLikeLiteral
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (body: Expr<SyntaxToken>)
        (isArray: bool)
        : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        let items =
            match body with
            | Expr.Sequential(exprs = items) -> items
            | single -> ImmutableArray.Create(single)

        for i = 0 to items.Length - 1 do
            let itemTy = infer ctx items.[i]
            unify ctx tok itemTy elemTy

        if isArray then
            TyConst(RuntimeNames.arrayKey 1, EqArray.singleton elemTy)
        else
            listLiteralTy ctx tok elemTy

    /// Element type stays free so context can pin it (`let xs : int list = []`).
    let emptyListLikeLiteral (ctx: PassContext) (tok: SyntaxToken) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyConst(RuntimeNames.arrayKey 1, EqArray.singleton elemTy)
        else
            listLiteralTy ctx tok elemTy

    let inferString (infer: Infer) (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : SemType =
        // Each hole's computed type is read back when the interpolation is lowered, so
        // type every hole here; a `%d{x}` specifier additionally constrains it.
        for part in parts do
            match part with
            | StringPart.Expr(formatSpecifier = fs; expr = e) ->
                let holeTy = infer ctx e

                match fs with
                | ValueSome ft ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
                    | ValueSome p ->
                        if p.Width = FormatDim.Star || p.Precision = FormatDim.Star then
                            ctx.Report(
                                (CstKeys.firstTokenOfExpr e),
                                Kind.Message
                                    "star width/precision takes its value from a printf argument; interpolated strings have none"
                            )

                        // `%a`/`%t` consume a printf callback curried from the format,
                        // not a plain value, so there is no hole value type to recover.
                        if PrintfSpec.isCallbackHole p.Type then
                            ()
                        else
                            let ts =
                                PrintfSpec.argTypes
                                    (freshHoleTy ctx (CstKeys.ofExpr e))
                                    ctx.Intrinsics.Unit
                                    ctx.Intrinsics.Unit
                                    p

                            unify ctx (CstKeys.firstTokenOfExpr e) holeTy (List.last ts)
                    | ValueNone -> ()
                | ValueNone -> ()
            | _ -> ()

        ctx.Intrinsics.String
