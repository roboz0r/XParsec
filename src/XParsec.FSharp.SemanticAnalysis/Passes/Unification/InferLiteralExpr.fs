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

    /// `pEnclosed` virtual-inserts a missing/mismatched close token with a
    /// parser-side diagnostic that isn't visible to semantic-analysis consumers,
    /// so surface the breakage on `ctx.Diagnostics` too — otherwise the malformed
    /// literal types successfully and Elaborate emits a well-shaped TAST.
    let rec checkLiteralClose
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
            // Defensive: pEnclosed only emits a real rParen when the peeked
            // token matched, so this can't trigger today — guards against a
            // future parser change letting a mismatched close-token through.
            ctx.Report(tok, Kind.Message(sprintf "Mismatched closing delimiter: expected '%s'" display))
        | TokenIndex.Regular _ -> ()

    /// The list type a `[…]` literal carries. Two cases:
    ///   1. A program that declares its own `'T list` abbreviation (the self-host
    ///      shape — `list.fs`'s `and 'T list = List<'T>`) resolves eagerly to its
    ///      RHS union.
    ///   2. A bare program (R3): the container is left *flexible* — a fresh
    ///      `TypeVar` registered in `ctx.ListLiterals`. This is the consumer-driven
    ///      typing handoff R3 calls for: `List.fold`'s `Vesper.Collections.List`
    ///      parameter flips it to the Vesper list (so the literal emits BCL-only),
    ///      while a literal nothing else pins (`printfn "%A" [1;2;3]`) defaults
    ///      back to FSharp.Core's `list` in `resolveListLiterals`.
    and listLiteralTy (ctx: PassContext) (tok: SyntaxToken) (elemTy: SemType) : SemType =
        match TypeRegistry.tryAbbrevArity ctx.Types UseSite.unbounded "list" 1 with
        | ValueSome info ->
            forceFill ctx info
            expandAbbreviation ctx tok info (EqArray.singleton elemTy)
        | ValueNone ->
            let tv = freshTyVar ctx

            ctx.RegisterListLiteral((UnionFind.find ctx.Store tv).Id, elemTy, tok)
            TyVar tv

    and inferListLikeLiteral
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
    and emptyListLikeLiteral (ctx: PassContext) (tok: SyntaxToken) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyConst(RuntimeNames.arrayKey 1, EqArray.singleton elemTy)
        else
            listLiteralTy ctx tok elemTy

    and inferString (infer: Infer) (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : SemType =
        // Elaborate lowers interpolated strings to a `TExpr.Format` (D9) and reads
        // each hole's computed type back to emit `AppendFormatted<T>`; a `%d{x}`
        // specifier additionally constrains the hole.
        for part in parts do
            match part with
            | StringPart.Expr(formatSpecifier = fs; expr = e) ->
                let holeTy = infer ctx e

                match fs with
                | ValueSome ft ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
                    | ValueSome p ->
                        // A star dimension draws its value from a printf argument;
                        // an interpolation hole has none. F# rejects this too, with a
                        // misleading FS3371 — ours is accurate. Still unify the value
                        // type (the last of `argTypes`) as best-effort recovery.
                        if p.Width = FormatDim.Star || p.Precision = FormatDim.Star then
                            ctx.Report(
                                (CstKeys.firstTokenOfExpr e),
                                Kind.Message
                                    "star width/precision takes its value from a printf argument; interpolated strings have none"
                            )

                        // `%a`/`%t` consume a printf callback curried from the
                        // format, not a plain value — an interpolation hole has
                        // none (same reason star dims are rejected above), so skip
                        // the value-type recovery for them. State/residue are
                        // irrelevant here: only the final value type is read, and
                        // it's a plain-value letter by this point.
                        if PrintfSpec.isCallbackHole p.Type then
                            ()
                        else
                            match
                                PrintfSpec.argTypes
                                    (fun () -> TyVar(freshTyVar ctx))
                                    ctx.Intrinsics.Unit
                                    ctx.Intrinsics.Unit
                                    p
                            with
                            | ValueSome ts -> unify ctx (CstKeys.firstTokenOfExpr e) holeTy (List.last ts)
                            | ValueNone -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
            | _ -> ()

        ctx.Intrinsics.String
