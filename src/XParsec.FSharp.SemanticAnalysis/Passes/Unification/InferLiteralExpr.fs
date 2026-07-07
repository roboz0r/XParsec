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
    /// literal types successfully and Freeze emits a well-shaped TAST.
    let rec checkLiteralClose
        (ctx: PassContext)
        (key: NodeKey)
        (rTok: SyntaxToken)
        (expected: Token)
        (display: string)
        : unit =
        match rTok.Index with
        | TokenIndex.Virtual -> ctx.Error(key, sprintf "Mismatched or missing closing delimiter: expected '%s'" display)
        | TokenIndex.Regular _ when rTok.Token <> expected ->
            // Defensive: pEnclosed only emits a real rParen when the peeked
            // token matched, so this can't trigger today — guards against a
            // future parser change letting a mismatched close-token through.
            ctx.Error(key, sprintf "Mismatched closing delimiter: expected '%s'" display)
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
    and listLiteralTy (ctx: PassContext) (key: NodeKey) (elemTy: SemType) : SemType =
        match ctx.Types.Abbreviation.TryGetValue "list" with
        | true, info ->
            forceFill ctx info
            expandAbbreviation ctx key info (EqArray.singleton elemTy)
        | false, _ ->
            let tv = freshTyVar ctx
            ctx.ListLiterals.Add(UnionFind.find tv, elemTy)
            TyVar tv

    and inferListLikeLiteral
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
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
            unify ctx key itemTy elemTy

        if isArray then
            TyConst(BuiltinTypes.intrinsicKey (RuntimeNames.arrayName 1), EqArray.singleton elemTy)
        else
            listLiteralTy ctx key elemTy

    /// Element type stays free so context can pin it (`let xs : int list = []`).
    and emptyListLikeLiteral (ctx: PassContext) (key: NodeKey) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyConst(BuiltinTypes.intrinsicKey (RuntimeNames.arrayName 1), EqArray.singleton elemTy)
        else
            listLiteralTy ctx key elemTy

    and inferString
        (infer: Infer)
        (ctx: PassContext)
        (_key: NodeKey)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : SemType =
        // Freeze lowers interpolated strings to a `TExpr.Format` (D9) and reads
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
                            ctx.Diagnostics.Add
                                {
                                    Key = CstKeys.ofExpr e
                                    Message =
                                        "star width/precision takes its value from a printf argument; interpolated strings have none"
                                    Code = ""
                                    Severity = Severity.Error
                                }

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
                                    BuiltinTypes.tyUnit
                                    BuiltinTypes.tyUnit
                                    p
                            with
                            | ValueSome ts -> unify ctx (CstKeys.ofExpr e) holeTy (List.last ts)
                            | ValueNone -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
            | _ -> ()

        BuiltinTypes.tyString
