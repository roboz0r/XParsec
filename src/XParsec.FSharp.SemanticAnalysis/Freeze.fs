namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// The only tree-to-tree transformation in the pipeline: projects the side-table
// annotations from previous passes into a fresh TAST.
//
// Invariant: side tables can be discarded after this returns. The TAST is
// sharable; the CST + side tables are scoped to one compilation.

module Freeze =

    let private typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk (TyVar tv)
        | ValueNone -> TyVar(TypeVar())

    /// Only strip the suffixes that map to literal kinds Unification recognises;
    /// the remainder is fed to the corresponding BCL parser.
    let private stripSuffix (suffix: string) (text: string) =
        if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// The escape set mirrors the lexer's `pCharChar` (Lexing.fs) exactly — a
    /// char literal that reaches here already lexed clean, so any unexpected
    /// shape is a broken invariant.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        elif inner.Length >= 2 && inner.[0] = '\\' then
            match inner.[1] with
            | '"' -> '"'
            | '\\' -> '\\'
            | '\'' -> '\''
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'b' -> '\b'
            | 'r' -> '\r'
            | 'a' -> '\a'
            | 'f' -> '\f'
            | 'v' -> '\v'
            | 'u' ->
                char (
                    System.UInt16.Parse(
                        inner.Substring(2, 4),
                        System.Globalization.NumberStyles.AllowHexSpecifier,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | 'x' ->
                char (
                    System.Byte.Parse(
                        inner.Substring(2, 2),
                        System.Globalization.NumberStyles.AllowHexSpecifier,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | d when System.Char.IsDigit d ->
                // Trigraph `\DDD` (decimal byte).
                char (System.Int32.Parse(inner.Substring(1, 3), System.Globalization.CultureInfo.InvariantCulture))
            | other -> failwithf "Freeze.parseCharLiteral: unsupported char escape '\\%c' in %s" other text
        else
            failwithf "Freeze.parseCharLiteral: unexpected char literal text %s" text

    let private parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        let parseLiteral (t: SyntaxToken) : TConstValue =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> TConstValue.Bool true
            | Token.KWFalse -> TConstValue.Bool false
            | Token.NumIEEE64
            | Token.NumIEEE64Hex
            | Token.NumIEEE64Octal
            | Token.NumIEEE64Binary ->
                TConstValue.Float(System.Double.Parse(text, System.Globalization.CultureInfo.InvariantCulture))
            | Token.NumInt64
            | Token.NumInt64Hex
            | Token.NumInt64Octal
            | Token.NumInt64Binary -> TConstValue.Int64(System.Int64.Parse(stripSuffix "L" text))
            | Token.NumByte
            | Token.NumByteHex
            | Token.NumByteOctal
            | Token.NumByteBinary -> TConstValue.Byte(System.Byte.Parse(stripSuffix "uy" text))
            | Token.CharLiteral -> TConstValue.Char(parseCharLiteral text)
            | Token.NumDecimal
            | Token.NumDecimalHex
            | Token.NumDecimalOctal
            | Token.NumDecimalBinary ->
                // Remainder is an invariant-culture decimal. Matches
                // `literalCarrier`'s `tyDecimal`.
                TConstValue.Decimal(
                    System.Decimal.Parse(
                        stripSuffix "M" text,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | _ ->
                // NumInt32 family and anything Unification hasn't classified are
                // treated as plain ints.
                TConstValue.Int(Int32.Parse text)

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    /// Patterns Unification doesn't understand yet fall through loudly so the
    /// gap surfaces at translation time.
    let rec private translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.Types.CtorIndex.ContainsKey n
            ->
            // Nullary ctor in pattern position. Must precede the plain
            // NamedSimple arm.
            TPat.Union(ctx.NameOf t, [], ty)
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty)
        | Pat.Wildcard _ -> TPat.Wildcard ty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) -> TPat.Tuple([ for sub in pats -> translatePat ctx sub ], ty)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty)
        | Pat.As(pat = inner) ->
            // The `as`-name isn't surfaced in TPat yet — downstream Var lookups
            // find the alias via the CST + side tables.
            translatePat ctx inner
        | Pat.Typed(pat = inner) ->
            // Annotation is consumed by Unification; runtime shape is the inner.
            translatePat ctx inner
        | Pat.Or(left = leftPat) ->
            // Both sides must bind the same names (Validation's job). Until or-
            // patterns are first-class in TPat, pick the left arm for shape.
            translatePat ctx leftPat
        | Pat.EmptyBlock _ -> TPat.Const(TConstValue.Unit, ty)
        | Pat.Record(fieldPats = fieldPats) ->
            let fields =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translatePat ctx sub
                ]

            TPat.Record(fields, ty)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                last.Length > 0
                && System.Char.IsUpper last.[0]
                && (li.Idents.Length = 1 && ctx.Types.CtorIndex.ContainsKey last
                    || li.Idents.Length = 2 && ctx.Types.Union.ContainsKey(ctx.NameOf li.Idents.[0])))
            ->
            let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            let subPats =
                if args.Length = 1 then
                    match args.[0] with
                    | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> [ for sub in pats -> translatePat ctx sub ]
                    | Pat.EnclosedBlock(pat = inner) -> [ translatePat ctx inner ]
                    | Pat.Tuple(patterns = pats) -> [ for sub in pats -> translatePat ctx sub ]
                    | sub -> [ translatePat ctx sub ]
                else
                    [ for sub in args -> translatePat ctx sub ]

            TPat.Union(caseName, subPats, ty)
        | Pat.Op _ ->
            // Operator-named binding head (`let (=) x y = …`): a single binder,
            // shaped like a `Pat.NamedSimple`. Its source name is the operator's
            // compiled name (`memberNameOfBinding` → `op_Equality`); the key
            // matches `CstKeys.ofBinding`, so the binding's `ModuleMembers` entry
            // (and thus the cross-package inline-body loader) finds it.
            TPat.NamedSimple(key, ty)
        | _ -> failwithf "Freeze.translatePat: TODO %A" p

    /// `()` literal. Distinct from `parseConst` because `Expr.EmptyBlock`
    /// carries `ParenKind` + closing token, not a `Constant`.
    let private unitConst (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        TExpr.Const(TConstValue.Unit, typeOfKey ctx key)

    /// Class-name reference only when there's no local `Binding` entry — i.e. it
    /// really is a class name, not a shadowing local.
    let private tryClassRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if ctx.Types.Class.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if ctx.Types.Class.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | _ -> ValueNone

    /// Peel an `Expr.App` argument that may be a single `EnclosedBlock`
    /// wrapping a `Tuple` (the F# parser shape for `Point(3, 4)`) so
    /// downstream consumers see the constructor's declared arity directly.
    let private peelCtorArgs
        (ctx: PassContext)
        (translate: Expr<SyntaxToken> -> TExpr)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : TExpr list =
        ignore ctx

        if args.Length = 1 then
            match args.[0] with
            | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translate a ]
            | Expr.Tuple(exprs = items) -> [ for a in items -> translate a ]
            | Expr.EnclosedBlock(expr = inner) -> [ translate inner ]
            | Expr.EmptyBlock _ -> []
            | a -> [ translate a ]
        else
            [ for a in args -> translate a ]

    /// Same as `peelCtorArgs` but for a single argument expression
    /// (HighPrecedenceApp form / Expr.New).
    let private peelOneArg (translate: Expr<SyntaxToken> -> TExpr) (arg: Expr<SyntaxToken>) : TExpr list =
        match arg with
        | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translate a ]
        | Expr.Tuple(exprs = items) -> [ for a in items -> translate a ]
        | Expr.EnclosedBlock(expr = Expr.EmptyBlock _) -> []
        | Expr.EnclosedBlock(expr = inner) -> [ translate inner ]
        | Expr.EmptyBlock _ -> []
        | a -> [ translate a ]

    /// Look up `memberName` on `typeName` — a class or (P3d.3) a union
    /// augmentation.
    let private tryClassMember (ctx: PassContext) (typeName: string) (memberName: string) : ClassMemberInfo voption =
        let pick (members: ClassMemberInfo[]) =
            match members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m -> ValueSome m
            | None -> ValueNone

        match ctx.Types.Class.TryGetValue typeName with
        | true, info -> pick info.Members
        | false, _ ->
            match ctx.Types.Union.TryGetValue typeName with
            | true, info -> pick info.Members
            | false, _ -> ValueNone

    /// Resolve `head.M` when the head is a local binding of a `TyClass`/`TyUnion`
    /// with a known member `M`. The parser folds the dot into the long ident
    /// rather than emitting `DotLookup` when the head is a regular identifier.
    let private tryLongIdentClassTail
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * ClassMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let head = li.Idents.[0]
            let headKey = NodeKey.ofToken head NodeKind.ExprIdent

            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueNone -> ValueNone
            | ValueSome rb ->
                match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                | ValueNone -> ValueNone
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyClass(typeName, _)
                    | TyUnion(typeName, _) ->
                        let memberName = ctx.NameOf li.Idents.[1]

                        match tryClassMember ctx typeName memberName with
                        | ValueSome m -> ValueSome(rb.BindingSite, Unification.zonk (TyVar tv), m)
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

    /// Resolve `ClassName.MemberName` to its static member info. `ValueNone` if
    /// either is unknown or the member is an instance member (use
    /// `tryLongIdentClassTail` for instance dispatch on a local binding).
    let private tryLongIdentStaticMember
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (string * ClassMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let className = ctx.NameOf li.Idents.[0]
            let memberName = ctx.NameOf li.Idents.[1]

            let pick (members: ClassMemberInfo[]) =
                match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = memberName) with
                | Some m -> ValueSome(className, m)
                | None -> ValueNone

            match ctx.Types.Class.TryGetValue className with
            | true, info -> pick info.Members
            | false, _ ->
                match ctx.Types.Union.TryGetValue className with
                | true, info -> pick info.Members
                | false, _ -> ValueNone

    /// DU ctor reference (`Circle`, `Result2.Ok`), returning the case name.
    /// Excludes local bindings whose names happen to match a ctor — they have a
    /// `Binding` entry.
    let private tryCtorRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if ctx.Types.CtorIndex.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if ctx.Types.CtorIndex.ContainsKey n then
                    ValueSome n
                else
                    ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 2 && ctx.Types.Union.ContainsKey(ctx.NameOf li.Idents.[0])
                ->
                let typeName = ctx.NameOf li.Idents.[0]
                let caseName = ctx.NameOf li.Idents.[1]
                let info = ctx.Types.Union.[typeName]

                if info.Cases |> Array.exists (fun c -> c.Name = caseName) then
                    ValueSome caseName
                else
                    ValueNone
            | _ -> ValueNone

    // Active patterns wrap the four `try*` helpers so each `translateExpr` arm
    // computes its guard once and binds the destructured result directly,
    // rather than re-evaluating in the body with a `ValueNone -> failwith
    // "unreachable"` fall-through.

    [<return: Struct>]
    let private (|ClassRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryClassRef ctx e

    [<return: Struct>]
    let private (|CtorRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryCtorRef ctx e

    [<return: Struct>]
    let private (|ClassTailMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * string) voption =
        match tryLongIdentClassTail ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let private (|ClassTailProperty|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * string) voption =
        match tryLongIdentClassTail ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Property ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let private (|StaticMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (string * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(className, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(className, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let private (|StaticMember|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (string * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(className, _) -> ValueSome(className, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | ValueNone -> ValueNone

    /// `[1; 2; 3]` parses as `EnclosedBlock(ParenKind.List, Sequential [...])`;
    /// a one-item literal `[1]` skips the Sequential wrapper.
    let private listLiteralItems (body: Expr<SyntaxToken>) : Expr<SyntaxToken> list =
        match body with
        | Expr.Sequential(exprs = items) -> [ for x in items -> x ]
        | single -> [ single ]

    /// Stitch a value-level `Expr.ILIntrinsic` instruction string (e.g.
    /// `(# "ceq" … #)` → `"ceq"`), trimming surrounding whitespace. Mirrors
    /// `NameResolution.ilIntrinsicString` for the type-level intrinsic.
    let private stitchIlInstruction (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString().Trim()

    let rec private translateExpr (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        let ty = typeOfKey ctx key

        match e with
        | Expr.Const c -> TExpr.Const(parseConst ctx c, ty)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            // `r.X` (or chained `r.X.Y`) parsed as a single multi-segment
            // LongIdent: head resolved as a local binding, rest field accesses.
            translateLongIdentFieldChain ctx li ty
        // Static member on an *external* type reached through a folded LongIdent
        // (`System.Console.Out`, `Console.Out`) — Unification resolved the prefix
        // as a type and recorded the member in `ExternalAccess`. Emit the same
        // keyed `TExpr.ExternalMember` as the generic `DotLookup` form; always
        // static, so the type-name receiver is dropped.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2 && ctx.Resolution.ExternalAccess.ContainsKey key
            ->
            let info =
                match ctx.Resolution.ExternalAccess.TryGetValue key with
                | ValueSome i -> i
                | ValueNone -> failwith "Freeze: unreachable (ExternalAccess membership just checked)"

            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            TExpr.ExternalMember(ValueNone, info.Key, memberName, info.IsProperty, ty)
        // `new T(args)` — Unification stamps `ty` with the `TyClass`. The CST-side
        // fallback is purely defensive for error paths where Unification couldn't
        // pin the receiver.
        | Expr.New(typ = t; expr = argExpr) ->
            let className =
                match Unification.zonk ty with
                | TyClass(n, _) -> n
                | _ ->
                    let rec nameOf t =
                        match t with
                        | Type.NamedType li
                        | Type.GenericType(longIdent = li) when li.Idents.Length >= 1 ->
                            ctx.NameOf li.Idents.[li.Idents.Length - 1]
                        | Type.ParenType(typ = inner) -> nameOf inner
                        | _ -> ""

                    nameOf t

            let args = peelOneArg (translateExpr ctx) argExpr
            TExpr.New(className, args, ty)
        // Class-name-as-function application: `Point(3, 4)` parses as
        // `Expr.App (Ident Point, [EnclosedBlock(Tuple)])`.
        | Expr.App(ClassRef ctx className, args) ->
            let argsList = peelCtorArgs ctx (translateExpr ctx) args
            TExpr.New(className, argsList, ty)
        | Expr.HighPrecedenceApp(funcExpr = ClassRef ctx className; argExpr = arg) ->
            let argsList = peelOneArg (translateExpr ctx) arg
            TExpr.New(className, argsList, ty)
        // Class instance method invocation: `r.M(args)` →
        // `App(DotLookup(r, ., M), args)`.
        | Expr.App(funcExpr = Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li); argExprs = args) when
            li.Idents.Length = 1
            && (
                match Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r)) with
                | TyClass(typeName, _)
                | TyUnion(typeName, _) ->
                    match tryClassMember ctx typeName (ctx.NameOf li.Idents.[0]) with
                    | ValueSome m -> m.Kind = ClassMemberKind.Method
                    | ValueNone -> false
                | _ -> false
            )
            ->
            let memberName = ctx.NameOf li.Idents.[0]
            let argsList = peelCtorArgs ctx (translateExpr ctx) args
            TExpr.MethodCall(translateExpr ctx r, memberName, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li); argExpr = arg) when
            li.Idents.Length = 1
            && (
                match Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r)) with
                | TyClass(typeName, _)
                | TyUnion(typeName, _) ->
                    match tryClassMember ctx typeName (ctx.NameOf li.Idents.[0]) with
                    | ValueSome m -> m.Kind = ClassMemberKind.Method
                    | ValueNone -> false
                | _ -> false
            )
            ->
            let memberName = ctx.NameOf li.Idents.[0]
            let argsList = peelOneArg (translateExpr ctx) arg
            TExpr.MethodCall(translateExpr ctx r, memberName, argsList, ty)
        // `p.M(args)` parses as `App` / `HighPrecedenceApp` whose fn is
        // `Expr.LongIdentOrOp(LongIdent [p; M])` — the parser folds the dot into
        // the long ident rather than emitting `DotLookup` when the head is a
        // regular identifier. Fold to MethodCall.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExprs = args) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            let argsList = peelCtorArgs ctx (translateExpr ctx) args
            TExpr.MethodCall(receiver, memberName, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExpr = arg) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            let argsList = peelOneArg (translateExpr ctx) arg
            TExpr.MethodCall(receiver, memberName, argsList, ty)
        // `p.X` (property) parses as `Expr.LongIdentOrOp(LongIdent[p; X])` when
        // the head is a regular identifier. Anything not a class property falls
        // to the chained FieldGet path below.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailProperty ctx (bindingSite, receiverTy, memberName))) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            TExpr.PropertyGet(receiver, memberName, ty)
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (className, memberName)))
            argExprs = args) ->
            let argsList = peelCtorArgs ctx (translateExpr ctx) args
            TExpr.StaticMethodCall(className, memberName, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (className, memberName)))
            argExpr = arg) ->
            let argsList = peelOneArg (translateExpr ctx) arg
            TExpr.StaticMethodCall(className, memberName, argsList, ty)
        // `ClassName.X` — static property read (or method-as-value).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMember ctx (className, memberName))) ->
            TExpr.StaticPropertyGet(className, memberName, ty)
        | CtorRef ctx caseName ->
            // Bare or qualified ctor reference outside an App. v1 distinguishes
            // nullary ctor (→ `UnionCons`) from ctor-as-value (`let f = Circle`,
            // typed `TyFun(_, TyUnion _)` → External) by the result type.
            match Unification.zonk ty with
            | TyUnion(_, _) -> TExpr.UnionCons(caseName, [], ty)
            // Function-typed ctor-as-value; codegen can eta-expand to a
            // UnionCons lambda.
            | _ -> TExpr.External(caseName, ValueNone, ty)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty
        | Expr.App(CtorRef ctx caseName, args) ->
            // Ctor application: `Circle 1.0` or `Rectangle(2.0, 3.0)`. F# treats
            // DU arguments as a single tuple; the TAST flattens it back to a
            // per-field list so consumers see the ctor's declared arity directly.
            let argsList =
                if args.Length = 1 then
                    match args.[0] with
                    | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translateExpr ctx a ]
                    | Expr.Tuple(exprs = items) -> [ for a in items -> translateExpr ctx a ]
                    | a -> [ translateExpr ctx a ]
                else
                    [ for a in args -> translateExpr ctx a ]

            TExpr.UnionCons(caseName, argsList, ty)
        | Expr.HighPrecedenceApp(funcExpr = CtorRef ctx caseName; argExpr = arg) ->
            let argsList =
                match arg with
                | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> [ for a in items -> translateExpr ctx a ]
                | Expr.Tuple(exprs = items) -> [ for a in items -> translateExpr ctx a ]
                | a -> [ translateExpr ctx a ]

            TExpr.UnionCons(caseName, argsList, ty)
        // Printf happy-path call, marked by `Unification.tryInferPrintfApp`. Must
        // lower to a `TExpr.Format` *before* the `App(printfn, New PrintfFormat …)`
        // projection below ever runs (vesper-printf-plan P1).
        | Expr.App(_, args) when ctx.PrintfApp.ContainsKey key -> translatePrintfFormat ctx key args ty
        | Expr.App(fn, args) -> translateApp ctx fn args
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
            TExpr.App(translateExpr ctx fn, translateExpr ctx arg, ty)
        | Expr.InfixApp(left, _, right) -> translateInfix ctx key left right ty
        | Expr.PrefixApp(_, operand) -> translatePrefix ctx key operand ty
        | Expr.Fun(argumentPats = argPats; expr = body) -> translateFun ctx argPats body
        | Expr.LetOrUse(bindings = bindings; body = body) -> translateLet ctx bindings body
        | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner) ->
            translateListLikeLiteral ctx ty false (listLiteralItems inner)
        | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner) ->
            translateListLikeLiteral ctx ty true (listLiteralItems inner)
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty
        | Expr.Tuple(exprs = items) -> TExpr.Tuple([ for x in items -> translateExpr ctx x ], ty)
        | Expr.Sequential(exprs = items) -> TExpr.Sequential([ for x in items -> translateExpr ctx x ], ty)
        // The annotation has no runtime representation — it only constrained
        // types in Unification; the TAST carries the inferred type inline.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        | Expr.EmptyBlock(lParen = ParenKind.List _) -> translateListLikeLiteral ctx ty false []
        | Expr.EmptyBlock(lParen = ParenKind.Array _) -> translateListLikeLiteral ctx ty true []
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) -> TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty)
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            TExpr.ForIn(translatePat ctx pat, translateExpr ctx src, translateExpr ctx body, ty)
        | Expr.String _ -> translateString ctx e ty
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty)
        | Expr.Function(rules = Rules(rules = rules)) ->
            // `function …` ~ `fun x -> match x with …`. The synthesised parameter
            // has no source token, so mint a synthetic key under the
            // function-keyword's offset for the Match scrutinee to reference.
            let funcKey = CstKeys.ofExpr e

            let paramKey = NodeKey.ofSynthetic funcKey.Offset NodeKind.SynthLambdaBody

            let paramTy, resultTy =
                match ty with
                | TyFun(p, r) -> p, r
                | _ -> failwithf "Freeze.Function: expected function type, got %A" ty

            let scrutinee = TExpr.Var(paramKey, paramTy)
            let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy)
            TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy), body, ty)
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            TExpr.TryWith(translateExpr ctx body, translateRules ctx rules, ty)
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
            TExpr.TryFinally(translateExpr ctx body, translateExpr ctx finallyE, ty)
        | Expr.Assignment(leftExpr = left; rightExpr = right) ->
            // `r.X <- v` folds to FieldSet; everything else to Assignment.
            let unwrapped =
                let rec unwrap e =
                    match e with
                    | Expr.EnclosedBlock(expr = inner)
                    | Expr.TypeAnnotation(expr = inner) -> unwrap inner
                    | _ -> e

                unwrap left

            match unwrapped with
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let fieldName = ctx.NameOf li.Idents.[0]
                TExpr.FieldSet(translateExpr ctx r, fieldName, translateExpr ctx right, ty)
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                // `r.X <- v` parsed as Assignment(LongIdent[r;X], <-, v). The
                // head-resolved chain peels into FieldGet for the intermediate
                // segments and a final FieldSet for the assigned slot.
                let receiverIdents = li.Idents
                let lastIdx = receiverIdents.Length - 1

                let receiverChain =
                    let head = receiverIdents.[0]
                    let headKey = NodeKey.ofToken head NodeKind.ExprIdent

                    let headBinding = ctx.Bindings.Binding.TryGetValue headKey

                    let headTy =
                        match headBinding with
                        | ValueSome rb -> typeOfKey ctx rb.BindingSite
                        | ValueNone -> typeOfKey ctx (CstKeys.ofExpr unwrapped)

                    let headExpr =
                        match headBinding with
                        | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
                        | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy)

                    let mutable curr = headExpr
                    let mutable currTy = headTy

                    for i = 1 to lastIdx - 1 do
                        let seg = receiverIdents.[i]
                        let segName = ctx.NameOf seg

                        let stepTy =
                            match Unification.zonk currTy with
                            | TyRecord(recName, args) ->
                                match ctx.Types.Record.TryGetValue recName with
                                | true, info ->
                                    match info.Fields |> Array.tryFind (fun f -> f.Name = segName) with
                                    | Some field ->
                                        Unification.zonk (
                                            Unification.instantiateMember (info.TypeParams, args) field.Type
                                        )
                                    | None -> currTy
                                | false, _ -> currTy
                            | _ -> currTy

                        curr <- TExpr.FieldGet(curr, segName, stepTy)
                        currTy <- stepTy

                    curr

                let lastName = ctx.NameOf receiverIdents.[lastIdx]
                TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty)
            | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty)
        | Expr.Record(fieldInitializers = inits) ->
            let fields =
                [
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                ]

            TExpr.RecordCons(fields, ty)
        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            let overrides =
                [
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                ]

            TExpr.RecordClone(translateExpr ctx src, overrides, ty)
        // Member access on an *external* type (static `Type.Member` or instance
        // `value.Member`) that Unification resolved through the provider — emit a
        // keyed `TExpr.ExternalMember` (symbol-resolution-plan §7.2, P3). A static
        // access drops the type-name receiver (`info.IsStatic`).
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 1 && ctx.Resolution.ExternalAccess.ContainsKey key
            ->
            let info =
                match ctx.Resolution.ExternalAccess.TryGetValue key with
                | ValueSome i -> i
                | ValueNone -> failwith "Freeze: unreachable (ExternalAccess membership just checked)"

            let memberName = ctx.NameOf li.Idents.[0]

            let receiver =
                if info.IsStatic then
                    ValueNone
                else
                    ValueSome(translateExpr ctx r)

            TExpr.ExternalMember(receiver, info.Key, memberName, info.IsProperty, ty)
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let memberName = ctx.NameOf li.Idents.[0]
            let rTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r))

            match rTy with
            | TyClass(typeName, _)
            | TyUnion(typeName, _) ->
                match tryClassMember ctx typeName memberName with
                | ValueSome m when m.Kind = ClassMemberKind.Property ->
                    TExpr.PropertyGet(translateExpr ctx r, memberName, ty)
                | _ ->
                    // Method-as-value or unresolved member.
                    TExpr.PropertyGet(translateExpr ctx r, memberName, ty)
            | _ -> TExpr.FieldGet(translateExpr ctx r, memberName, ty)
        | Expr.Null _ -> TExpr.Null ty
        | Expr.Range(fromExpr = a; toExpr = b) -> TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty)
        | Expr.ILIntrinsic(instrParts = parts; args = args) ->
            let opCode = stitchIlInstruction ctx parts
            TExpr.ILIntrinsic(opCode, [ for a in args -> translateExpr ctx a ], ty)
        | Expr.LibraryOnlyStaticOptimization _ ->
            // The clause chain nests left-fold (outermost = the last `when` in
            // source order). Peel it into a flat source-ordered clause list plus
            // the leading default expr, reading each clause's resolved constraints
            // from the side table Unification keyed by that clause node's key.
            // Visiting outermost→innermost and prepending yields source order.
            let rec peel (node: Expr<SyntaxToken>) (acc: TStaticOptClause list) : TExpr * TStaticOptClause list =
                match node with
                | Expr.LibraryOnlyStaticOptimization(expr = inner; optimizedExpr = optE) ->
                    let cs =
                        match ctx.StaticOpt.TryGetValue(CstKeys.ofExpr node) with
                        | ValueSome v -> v
                        | ValueNone -> []

                    peel
                        inner
                        ({
                            Constraints = cs
                            Body = translateExpr ctx optE
                         }
                         :: acc)
                | other -> translateExpr ctx other, acc

            let defaultExpr, clauses = peel e []
            TExpr.StaticOptimization(clauses, defaultExpr, ty)
        | _ ->
            // TODO: extend as the subset grows; surface the unhandled case
            // loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : TMatchArm list =
        [
            for r in rules do
                match r with
                | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                    let guardT =
                        match guard with
                        | ValueSome(PatternGuard(expr = g)) -> Some(translateExpr ctx g)
                        | ValueNone -> None

                    yield
                        {
                            Pat = translatePat ctx pat
                            Guard = guardT
                            Body = translateExpr ctx body
                        }
                | _ -> ()
        ]

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) : TExpr =
        match e with
        | Expr.String(parts = parts) ->
            match Unification.zonk ty with
            | TyClass(name, _) when name = PrintfSpec.printfFormatName ->
                // Format literal at a printf call site (typed by
                // `Unification.tryInferPrintfApp`). It denotes `new
                // PrintfFormat<…>(text)` — the single `value: string` ctor.
                TExpr.New(
                    name,
                    [
                        TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), BuiltinTypes.tyString)
                    ],
                    ty
                )
            | _ ->
                // A faithfully-renderable interpolation lowers to a `TExpr.Format`
                // (D9). Otherwise (plain string, or an unrenderable hole) stitch
                // the literal text, keeping any unrendered hole's `{<expr>}`
                // placeholder — additive over the pre-D9 behaviour.
                match tryTranslateInterpolation ctx parts ty with
                | Some node -> node
                | None -> TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), ty)
        | _ -> failwithf "Freeze.translateString: not a String expr: %A" e

    /// Interpolation holes have no rendering on this path, so they surface as
    /// `{<expr>}` placeholders. Only reached for plain strings, printf format
    /// literals, and interpolations a hole kept off the `TExpr.Format` path.
    and private stitchLiteralString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append("{<expr>}") |> ignore
            | StringPart.OrphanFormatSpecifier t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore

        sb.ToString()

    /// Classify one interpolation hole into the `(HoleKind, .NET format,
    /// alignment)` triple a `FormatSeg.Hole` carries, or `None` if it can't be
    /// rendered faithfully. A printf-style `%d{x}` reuses
    /// `PrintfSpec.tryHoleFormat` (so it covers exactly the specifiers the printf
    /// happy path does); a plain `{x}` / `{x:fmt}` is a `Formatted` hole.
    /// Interpolation alignment (`{x,n}`) isn't representable here — the parser
    /// folds `x,n` into a tuple expression — so alignment is always `None` for
    /// the plain forms.
    and private tryInterpHoleSpec
        (ctx: PassContext)
        (formatSpecifier: SyntaxToken voption)
        (formatClause: SyntaxToken voption)
        : (PrintfSpec.HoleKind * string option * int option) option =
        match formatSpecifier with
        | ValueSome ft ->
            match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
            | ValueSome p ->
                match PrintfSpec.tryHoleFormat p with
                | ValueSome(k, f, a) -> Some(k, f, a)
                | ValueNone -> None
            | ValueNone -> None
        | ValueNone ->
            let fmt =
                match formatClause with
                | ValueSome fc ->
                    let raw = ctx.NameOf fc
                    let f = if raw.StartsWith ":" then raw.Substring 1 else raw
                    if f.Length = 0 then None else Some f
                | ValueNone -> None

            Some(PrintfSpec.HoleKind.Formatted, fmt, None)

    /// Lower an interpolated string ($"…{x}…") to a `TExpr.Format` (D9). Returns
    /// `None` — keeping the literal-stitch fallback — when the string has no
    /// holes, or any hole isn't faithfully renderable: a free (unresolved) hole
    /// type, an orphan/standalone `%spec` or lexer-error part, or a printf-typed
    /// `%d{x}` whose specifier the happy path doesn't cover.
    and private tryTranslateInterpolation
        (ctx: PassContext)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        (ty: SemType)
        : TExpr option =
        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()
        let mutable hasHole = false
        let mutable lowerable = true

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        for part in parts do
            if lowerable then
                match part with
                // `%%` collapses to `%` (an interpolated string rides the same
                // PrintfFormat machinery as printf); escape sequences stay
                // verbatim — the unescaping gap `stitchLiteralString` /
                // `translatePrintfFormat` carry.
                | StringPart.Text t
                | StringPart.EscapeSequence t
                | StringPart.VerbatimEscapeQuote t -> litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
                | StringPart.EscapePercent _ -> litRun.Append('%') |> ignore
                | StringPart.Expr(formatSpecifier = fs; expr = holeExpr; formatClause = fc) ->
                    hasHole <- true
                    let holeTy = typeOfKey ctx (CstKeys.ofExpr holeExpr)

                    match Unification.zonk holeTy with
                    // A free hole type can't pick an `AppendFormatted<T>` — bail.
                    | TyVar _ -> lowerable <- false
                    | zHoleTy ->
                        match tryInterpHoleSpec ctx fs fc with
                        | Some(kind, netFormat, alignment) ->
                            flushLit ()

                            segments.Add(
                                FormatSeg.Hole(
                                    {
                                        Ty = zHoleTy
                                        Kind = kind
                                        Format = netFormat
                                        Alignment = alignment
                                    },
                                    translateExpr ctx holeExpr
                                )
                            )
                        | None -> lowerable <- false
                // A standalone `%spec`, orphan specifier, or lexer-error part has
                // interpolation-specific semantics we don't model — keep the whole
                // string on the literal-stitch fallback.
                | StringPart.FormatSpecifier _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> lowerable <- false

        if hasHole && lowerable then
            flushLit ()
            Some(TExpr.Format(FormatSink.ToString, EqArray.ofSeq segments, ty))
        else
            None

    and private translateIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) (ty: SemType) : TExpr =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty)
        | ValueNone ->
            // No Binding entry => NameResolution resolved through the provider.
            // Multi-segment names are joined with `.` so `External` carries the
            // same key the provider sees.
            let name =
                match e with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
                    // `(+)`-as-a-value: carry the operator's compiled name so the
                    // External matches what the provider (and codegen) key on.
                    match Desugar.symbolicOpCompiledName op.Token with
                    | ValueSome n -> n
                    | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
                | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

            TExpr.External(name, ValueNone, ty)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// head segment's TAST node is a `Var` pointing back at the local binding.
    and private translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        : TExpr =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent
        let headBinding = ctx.Bindings.Binding.TryGetValue headKey

        let headTy =
            // Unification didn't allocate a side-table entry for the synthetic
            // head key, so fall back to the binding site's TyVar.
            match headBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> finalTy

        let headExpr =
            match headBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
            | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy)

        let mutable currTy = headTy
        let mutable curr = headExpr

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Intermediate steps recover the segment's declared type from the
            // receiver — a record/union/class field, or a union/class *instance
            // member* return type (so a chain through a member returning a union,
            // `xs.Tail.Head`, keeps `xs.Tail : Lst<_>` instead of collapsing to the
            // chain's final type). The last step uses the whole chain's `finalTy`.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    let resolved =
                        match Unification.zonk currTy with
                        | TyRecord(recName, args) ->
                            match ctx.Types.Record.TryGetValue recName with
                            | true, info ->
                                info.Fields
                                |> Array.tryPick (fun f ->
                                    if f.Name = segName then
                                        Some(Unification.instantiateMember (info.TypeParams, args) f.Type)
                                    else
                                        None
                                )
                            | false, _ -> None
                        | TyUnion(unionName, args) ->
                            match ctx.Types.Union.TryGetValue unionName with
                            | true, info ->
                                info.Members
                                |> Array.tryPick (fun m ->
                                    if m.Name = segName && not m.IsStatic then
                                        Some(Unification.instantiateMember (info.TypeParams, args) m.Type)
                                    else
                                        None
                                )
                            | false, _ -> None
                        | TyClass(clsName, args) ->
                            match ctx.Types.Class.TryGetValue clsName with
                            | true, info ->
                                info.Members
                                |> Array.tryPick (fun m ->
                                    if m.Name = segName && not m.IsStatic then
                                        Some(Unification.instantiateMember (info.TypeParams, args) m.Type)
                                    else
                                        None
                                )
                            | false, _ -> None
                        | _ -> None

                    match resolved with
                    | Some t -> Unification.zonk t
                    | None -> finalTy

            // PropertyGet for a class/union member, FieldGet otherwise. Method
            // members accessed without an application keep the PropertyGet shape —
            // codegen can eta-expand if needed.
            let node =
                match Unification.zonk currTy with
                | TyClass(clsName, _) ->
                    match ctx.Types.Class.TryGetValue clsName with
                    | true, info when info.Members |> Array.exists (fun m -> m.Name = segName) ->
                        TExpr.PropertyGet(curr, segName, stepTy)
                    | _ -> TExpr.FieldGet(curr, segName, stepTy)
                // A union receiver's segment is an augmentation member (P3d.3,
                // `xs.IsEmpty`) → `PropertyGet` (codegen calls its `get_<name>`);
                // anything unknown stays a `FieldGet`.
                | TyUnion(unionName, _) ->
                    match ctx.Types.Union.TryGetValue unionName with
                    | true, info when info.Members |> Array.exists (fun m -> m.Name = segName) ->
                        TExpr.PropertyGet(curr, segName, stepTy)
                    | _ -> TExpr.FieldGet(curr, segName, stepTy)
                | _ -> TExpr.FieldGet(curr, segName, stepTy)

            curr <- node
            currTy <- stepTy

        curr

    and private translateApp
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : TExpr =
        let mutable result = translateExpr ctx fn
        let mutable currTy = typeOfKey ctx (CstKeys.ofExpr fn)

        for a in args do
            let argT = translateExpr ctx a

            let resTy =
                match currTy with
                | TyFun(_, r) -> r
                | _ ->
                    failwithf
                        "Freeze.translateApp: expected function type for application, got %A (Unification bug or free TypeVar)"
                        currTy

            result <- TExpr.App(result, argT, resTy)
            currTy <- resTy

        result

    /// Lower a marked printf call (`Unification.tryInferPrintfApp` recorded a
    /// `PrintfApp` sink for it) into a `TExpr.Format`, pairing each specifier
    /// with the next argument in spec order (the format is arg 0). The happy
    /// path therefore never produces a `New PrintfFormat` / `App printfn`.
    and private translatePrintfFormat
        (ctx: PassContext)
        (key: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        : TExpr =
        let sink =
            match ctx.PrintfApp.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Freeze.translatePrintfFormat: no PrintfApp marker at %O" key

        let parts =
            match args.[0] with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Freeze.translatePrintfFormat: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Holes consume the trailing args (the format is arg 0) in spec order.
        let mutable holeIdx = 1

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t ->
                // Verbatim source text (escape unescaping is a pre-existing gap
                // shared with `translateString`). `%%` is the printf escape for a
                // literal `%`; the lexer folds it into a raw `Text` part, and
                // there's no runtime format pass to collapse it, so collapse here.
                // A real specifier is its own `FormatSpecifier` part, so every `%`
                // in a raw run is half of a `%%` pair.
                litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
            | StringPart.EscapePercent _ ->
                // `%%` denotes a literal `%`; no runtime format pass here, so
                // collapse now (the FSharp.Core path does it at runtime).
                litRun.Append('%') |> ignore
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Freeze.translatePrintfFormat: unparsable specifier (marker invariant broken)"

                let kind, netFormat, alignment =
                    match PrintfSpec.tryHoleFormat placeholder with
                    | ValueSome(k, f, a) -> k, f, a
                    | ValueNone ->
                        failwith "Freeze.translatePrintfFormat: unsupported specifier (marker invariant broken)"

                let argExpr = args.[holeIdx]
                holeIdx <- holeIdx + 1
                let argT = translateExpr ctx argExpr
                let holeTy = typeOfKey ctx (CstKeys.ofExpr argExpr)

                segments.Add(
                    FormatSeg.Hole(
                        {
                            Ty = holeTy
                            Kind = kind
                            Format = netFormat
                            Alignment = alignment
                        },
                        argT
                    )
                )
            | StringPart.Expr _
            | StringPart.OrphanFormatSpecifier _
            | StringPart.InvalidText _ ->
                failwith "Freeze.translatePrintfFormat: non-literal format part (marker invariant broken)"

        flushLit ()

        let formatSink =
            match sink with
            | PrintfSpec.PrintfSink.StdOut nl -> FormatSink.ToStdOut nl
            | PrintfSpec.PrintfSink.StdErr nl -> FormatSink.ToStdErr nl
            | PrintfSpec.PrintfSink.StringResult -> FormatSink.ToString

        TExpr.Format(formatSink, EqArray.ofSeq segments, ty)

    and private translateInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // Reconstruct the operator's type from the resolved arms, not by
            // re-instantiating the scheme: re-instantiation would mint fresh
            // TypeVars the existing TyVar table doesn't link, so the External's
            // carried type wouldn't match the App chain's resolved arms.
            let leftTy = typeOfKey ctx (CstKeys.ofExpr left)
            let rightTy = typeOfKey ctx (CstKeys.ofExpr right)
            let partialTy = TyFun(rightTy, resultTy)
            let opTy = TyFun(leftTy, partialTy)
            let opExpr = TExpr.External(name, ValueNone, opTy)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy)
            TExpr.App(app1, translateExpr ctx right, resultTy)
        | ValueSome _
        | ValueNone ->
            // Desugar always attaches an OpName for an InfixApp key; reaching
            // here is a bug. Surface loudly.
            failwithf "Freeze: InfixApp at %O missing DesugaredForm entry" key

    and private translatePrefix
        (ctx: PassContext)
        (key: NodeKey)
        (operand: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // See translateInfix: reconstruct from the resolved operand + result
            // rather than re-instantiating the scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            let opExpr = TExpr.External(name, ValueNone, opTy)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy)
        | ValueSome _
        | ValueNone -> failwithf "Freeze: PrefixApp at %O missing DesugaredForm entry" key

    /// Project `[…]` / `[|…|]` literals into the shared `Cons` / `Nil` chain
    /// Unification typed them with. Arrays additionally route through `Array.ofList`
    /// so codegen sees a single lowering target — the list chain. Element type is
    /// recovered from the literal's frozen type; a degenerate type falls back to a
    /// free TyVar so downstream consumers see *some* element type, not a malformed
    /// node.
    and private translateListLikeLiteral
        (ctx: PassContext)
        (literalTy: SemType)
        (isArray: bool)
        (items: Expr<SyntaxToken> list)
        : TExpr =
        let zonked = Unification.zonk literalTy

        let elemTy =
            match zonked with
            | TyRecord(_, [ elem ])
            | TyUnion(_, [ elem ]) -> elem
            | _ -> TyVar(TypeVar())

        // A program-declared list union (resolved via the `'T list = List<'T>`
        // abbrev — see `Unification.listLiteralTy`) drives `[…]` construction
        // through that union's own case factories: nullary case = empty
        // terminator, single binary case = cons. Absent it (a normal program, or
        // any array literal), the FSharp.Core `Cons`/`Nil` nominal is the default.
        // Arrays never retarget — always the list chain + `Array.ofList` boundary.
        let listTy, consName, nilName =
            match zonked with
            | TyUnion(unionName, _) when not isArray && ctx.Types.Union.ContainsKey unionName ->
                let info = ctx.Types.Union.[unionName]
                let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
                let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

                match nilCase, consCase with
                | Some n, Some c -> zonked, c.Name, n.Name
                | _ -> TyRecord("Microsoft.FSharp.Collections.list", [ elemTy ]), "Cons", "Nil"
            // The external Vesper list (R3): a bare-program literal a consumer drove
            // onto the Vesper cons-list (`Unification.listLiteralTy` /
            // `resolveListLiterals`). Its `Cons` / `Nil` factories are minted by the
            // backend's `TryEmitUnionCons` Vesper case — BCL-only, no FSharp.Core.
            // Either the union name (self-host `'T list = List<'T>` expansion) or the
            // abbreviation name (`Vesper.List`'s contract `'T list` parameter — the
            // same convention FSharp.Core's `…Collections.list` uses) denotes it.
            | TyRecord(("Vesper.Collections.List" | "Vesper.Collections.list"), _) when not isArray ->
                zonked, "Cons", "Nil"
            | _ -> TyRecord("Microsoft.FSharp.Collections.list", [ elemTy ]), "Cons", "Nil"

        let listExpr =
            let nil = TExpr.UnionCons(nilName, [], listTy)

            items
            |> List.foldBack (fun item acc -> TExpr.UnionCons(consName, [ translateExpr ctx item; acc ], listTy))
            <| nil

        if isArray then
            let arrayTy = TyRecord("Microsoft.FSharp.Core.[]", [ elemTy ])
            // Codegen resolves `Array.ofList` against its target; alternate
            // targets are free to swap the wrapper.
            let opName = "Microsoft.FSharp.Collections.ArrayModule.OfList"
            let opTy = TyFun(listTy, arrayTy)
            TExpr.App(TExpr.External(opName, ValueNone, opTy), listExpr, arrayTy)
        else
            listExpr

    and private translateIfThenElse
        (ctx: PassContext)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        (resultTy: SemType)
        : TExpr =
        let elseExpr =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> e
            | ValueNone -> failwith "Freeze: if-then without else not yet supported"

        // Fold elifs right-to-left, each nested as the else-branch of the previous.
        let mutable nestedElse = translateExpr ctx elseExpr

        for i = elifs.Length - 1 downto 0 do
            let elifCond, elifThen =
                match elifs.[i] with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            nestedElse <- TExpr.IfThenElse(translateExpr ctx elifCond, translateExpr ctx elifThen, nestedElse, resultTy)

        TExpr.IfThenElse(translateExpr ctx cond, translateExpr ctx thenE, nestedElse, resultTy)

    and private translateFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : TExpr =
        let mutable result = translateExpr ctx body
        let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr body)

        for i = argPats.Length - 1 downto 0 do
            let p = argPats.[i]
            let tpat = translatePat ctx p
            let pTy = typeOfKey ctx (CstKeys.ofPat p)
            let lamTy = TyFun(pTy, resultTy)
            result <- TExpr.Lambda(tpat, result, lamTy)
            resultTy <- lamTy

        result

    and private translateLet
        (ctx: PassContext)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : TExpr =
        match body with
        | ValueSome bodyExpr ->
            let mutable result = translateExpr ctx bodyExpr
            let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr bodyExpr)

            for i = bindings.Length - 1 downto 0 do
                let b = bindings.[i]
                let tpat = translatePat ctx b.headPat
                let valT = translateBinding ctx b
                result <- TExpr.Let(tpat, valT, result, resultTy)

            result
        | ValueNone ->
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed`, not yet supported.
            failwith "Freeze: Expr.LetOrUse with no body (UseFixed) not supported"

    and private translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr

    // Interface-shaped type declarations (self-host rung 1).
    // Rung 1 surfaces exactly one emittable type shape: an interface (an
    // object-model body of all-abstract members, no base, no preamble). A
    // declaring-type typar becomes a `TyConst "'A"` marker the backend maps to a
    // generic-parameter index. Abbrevs (incl. Part-A primitive bindings), records,
    // unions, and concrete classes surface nothing. See docs/self-host-rung1-plan.md.

    let private typeNameSimple (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.IsEmpty then
            ""
        else
            ctx.NameOf li.Idents.[li.Idents.Length - 1]

    /// Rewrite declaring-type typars (free `TyVar`s, by zonked root) to the
    /// `TyConst "'A"` markers the backend's typar encoder consumes. Anything else
    /// passes through unchanged — a leftover inference var stays a `TyVar`, which
    /// the backend rejects loudly (out of scope for rung 1).
    let private remapDeclTypars (markers: (TypeVar * string) list) (t: SemType) : SemType =
        let rec go t =
            match t with
            | TyVar tv ->
                match
                    markers
                    |> List.tryPick (fun (r, n) -> if Object.ReferenceEquals(r, tv) then Some n else None)
                with
                | Some n -> TyConst n
                | None -> t
            | TyConst _ -> t
            | TyFun(a, b) -> TyFun(go a, go b)
            | TyTuple ts -> TyTuple(List.map go ts)
            | TyRecord(n, args) -> TyRecord(n, List.map go args)
            | TyUnion(n, args) -> TyUnion(n, List.map go args)
            | TyClass(n, args) -> TyClass(n, List.map go args)

        go (Unification.zonk t)

    /// Rewrite every `SemType` embedded in a member body via `f`. Used to push a
    /// generic union's declaring-typar remap (`remapDeclTypars`) through the whole
    /// member body, so a typar-typed local / scrutinee / bound variable carries the
    /// `TyConst "'T"` marker the backend's generic-member encoder consumes — just as
    /// the case-field types do (P3d.4 generalised to member bodies for R2).
    let private mapExprTypes (f: SemType -> SemType) (e: TExpr) : TExpr =
        TastWalk.mapExpr
            { TastWalk.identityMapper with
                MapType = f
            }
            e

    /// Classify an object-model body as an interface — every element an abstract
    /// method signature, no base type, no `let`/`do` preamble — and build its
    /// methods from the *resolved* member signatures in `ctx.Types.Class` (an
    /// `Anon`/`Interface` registers as a class). None for a concrete
    /// member/field/inherit (a class or later rung) or a never-registered type.
    let private tryInterfaceMethods
        (ctx: PassContext)
        (name: string)
        (body: ObjectModelBody<SyntaxToken>)
        : (string list * TAbstractMethod list) option =
        let allAbstractMethods =
            not body.elements.IsEmpty
            && body.elements
               |> Seq.forall (fun el ->
                   match el with
                   | TypeDefnElement.Member(MemberDefn.Member(
                       defn = MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig _))) -> true
                   | _ -> false
               )

        if body.inherits.IsSome || not body.classPreamble.IsEmpty || not allAbstractMethods then
            None
        else
            match ctx.Types.Class.TryGetValue name with
            | false, _ -> None
            | true, info ->
                // The member signatures share these prototype TyVars (Unification
                // typed them under the class's typar scope), so the remap reaches
                // every typar.
                let markers =
                    [
                        for (n, ptv) in info.TypeParams do
                            match Unification.zonk (TyVar ptv) with
                            | TyVar root -> yield (root, n)
                            | _ -> ()
                    ]

                let methods =
                    [
                        for m in info.Members do
                            if m.Kind = ClassMemberKind.Method then
                                // A generic method's own typars get markers too so
                                // the backend routes them to `GenericMethodParameter`
                                // (declaring typars stay `GenericTypeParameter`); the
                                // `TyConst "name"` picks the table.
                                let methodMarkers =
                                    markers
                                    @ [
                                        for (n, ptv) in m.MethodTypeParams do
                                            match Unification.zonk (TyVar ptv) with
                                            | TyVar root -> yield (root, n)
                                            | _ -> ()
                                    ]

                                yield
                                    {
                                        Name = m.Name
                                        MethodTypeParams = [ for (n, _) in m.MethodTypeParams -> n ]
                                        Signature = remapDeclTypars methodMarkers m.Type
                                    }
                    ]

                Some([ for (n, _) in info.TypeParams -> n ], methods)

    /// Member name from a member binding's `headPat` (`member this.M …` parses
    /// the member name as the head pattern's ident).
    let private memberNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id)
            // Operator-named binding head: surface the operator's compiled name
            // (`(=)` → `op_Equality`) so the member is addressable from a use
            // site's desugared `External(op_Equality)` head.
            | Pat.Op io -> Desugar.opPatCompiledName ctx.NameOf io
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// Member parameter list as `(bindingKey, ty)` pairs in declaration order
    /// (`this` is separate). The binding key is the same one `translatePat` mints,
    /// so a `Var` reference in the body resolves to it. Only simple parameters (a
    /// single ident per arg group) are surfaced (v1).
    let private memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : (NodeKey * SemType) list =
        [
            for p in b.argumentPats do
                match translatePat ctx p with
                | TPat.NamedSimple(k, ty) -> yield (k, ty)
                | _ -> ()
        ]

    /// Translate one union augmentation member element into a `TTypeMember`
    /// (P3d.3). Instance members reference `this` via `info.ThisKey`.
    let private translateUnionMember
        (ctx: PassContext)
        (info: UnionTypeInfo)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        match el with
        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
            let isStatic = s.IsSome

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Kind = kind
                            ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                            ThisTy = TyUnion(info.Name, [])
                            Params = memberParams ctx b
                            Body = translateExpr ctx b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                        }
                | ValueNone -> ValueNone

            match d with
            | MethodOrPropDefn.Method(defn = b) -> build TMemberKind.Method b
            | MethodOrPropDefn.Property(defn = b) -> build TMemberKind.Property b
            | MethodOrPropDefn.AutoProperty(ident = id; expr = e) ->
                ValueSome
                    {
                        Name = ctx.NameOf id
                        IsStatic = isStatic
                        Kind = TMemberKind.Property
                        ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                        ThisTy = TyUnion(info.Name, [])
                        Params = []
                        Body = translateExpr ctx e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Surface a `TypeDefn.Union` as a `TDecl.Type` from the resolved
    /// `UnionTypeInfo`. Any declaring-type typar is remapped to a `TyConst "'A"`
    /// marker (a no-op for a monomorphic union — `TypeParams` empty — but the
    /// right shape for the generic union rung). Augmentation members (`ext`) are
    /// surfaced as `TTypeMember`s (P3d.3).
    let private tryUnionType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : TDecl option =
        match ctx.Types.Union.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers =
                [
                    for (n, ptv) in info.TypeParams do
                        match Unification.zonk (TyVar ptv) with
                        | TyVar root -> yield (root, n)
                        | _ -> ()
                ]

            let cases =
                [
                    for c in info.Cases ->
                        let fields =
                            [
                                for i in 0 .. c.Fields.Length - 1 ->
                                    let nm =
                                        if i < c.FieldNames.Length then
                                            c.FieldNames.[i]
                                        else
                                            ValueNone

                                    nm, remapDeclTypars markers c.Fields.[i]
                            ]

                        { Name = c.Name; Fields = fields }
                ]

            // A generic union's members must carry the declaring-typar markers the
            // backend's generic-member encoder consumes (`!0`), exactly like the
            // case fields above: remap the member signature (`ThisTy` / `Params` /
            // `ReturnTy`) *and* the body's embedded types. Monomorphic unions
            // (`markers` empty) keep the bodies untouched — `translateUnionMember`'s
            // `TyUnion(name, [])` is already correct, so the path stays byte-identical.
            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let remapMember (m: TTypeMember) : TTypeMember =
                let f = remapDeclTypars markers

                { m with
                    ThisTy = TyUnion(info.Name, [ for n in declTypars -> TyConst n ])
                    Params = m.Params |> List.map (fun (k, ty) -> k, f ty)
                    Body = mapExprTypes f m.Body
                    ReturnTy = f m.ReturnTy
                }

            let members =
                match ext with
                | ValueNone -> []
                | ValueSome(TypeExtensionElements(elements = elems)) ->
                    [
                        for el in elems do
                            match translateUnionMember ctx info el with
                            | ValueSome m -> yield (if List.isEmpty declTypars then m else remapMember m)
                            | ValueNone -> ()
                    ]

            Some(
                TDecl.Type
                    {
                        Name = name
                        Namespace = ns
                        TypeParams = [ for (n, _) in info.TypeParams -> n ]
                        Kind = TTypeKind.Union(cases, members)
                        EqualitySupport = info.EqualitySupport
                        ComparisonSupport = info.ComparisonSupport
                    }
            )

    /// Surface a `TypeDefn.Record` as a `TDecl.Type` from the resolved
    /// `RecordTypeInfo`. Field types are remapped through the declaring-type
    /// typars (a no-op for a monomorphic record — `TypeParams` empty — but the
    /// right shape for the generic record path, exactly like `tryUnionType`).
    /// Augmentation members are out of scope for v1 (records-plan §B1) — the
    /// member list stays empty; the front end never registers them under a record
    /// today.
    let private tryRecordType (ctx: PassContext) (ns: string option) (name: string) : TDecl option =
        match ctx.Types.Record.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers =
                [
                    for (n, ptv) in info.TypeParams do
                        match Unification.zonk (TyVar ptv) with
                        | TyVar root -> yield (root, n)
                        | _ -> ()
                ]

            let fields =
                [
                    for f in info.Fields ->
                        {
                            Name = f.Name
                            Type = remapDeclTypars markers f.Type
                            IsMutable = f.IsMutable
                        }
                ]

            Some(
                TDecl.Type
                    {
                        Name = name
                        Namespace = ns
                        TypeParams = [ for (n, _) in info.TypeParams -> n ]
                        Kind = TTypeKind.Record(fields, [])
                        EqualitySupport = info.EqualitySupport
                        ComparisonSupport = info.ComparisonSupport
                    }
            )

    /// Surface an interface-shaped, union, or record `TypeDefn` as a
    /// `TDecl.Type`. Anything else (abbrevs, concrete classes) surfaces nothing.
    let private tryTypeDecl (ctx: PassContext) (ns: string option) (td: TypeDefn<SyntaxToken>) : TDecl option =
        let classify tn body =
            let name = typeNameSimple ctx tn

            match tryInterfaceMethods ctx name body with
            | Some(typars, methods) ->
                Some(
                    TDecl.Type
                        {
                            Name = name
                            Namespace = ns
                            TypeParams = typars
                            Kind = TTypeKind.Interface methods
                            // Interfaces never synthesise an equality triple;
                            // the field is filled to keep the record shape
                            // total and the value is unread for this kind.
                            EqualitySupport = EqualityVerdict.Structural
                            // Interfaces never synthesise a comparison pair
                            // either — same reasoning. Default to
                            // `NoComparison` so the field is present.
                            ComparisonSupport = ComparisonVerdict.NoComparison
                        }
                )
            | None -> None

        match td with
        | TypeDefn.Anon(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Interface(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Union(typeName = tn; extensions = ext) -> tryUnionType ctx ns (typeNameSimple ctx tn) ext
        | TypeDefn.Record(typeName = tn) -> tryRecordType ctx ns (typeNameSimple ctx tn)
        | _ -> None

    let private longIdentText (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string =
        li.Idents |> Seq.map ctx.NameOf |> String.concat "."

    /// `holder` is the enclosing named module's compiled holder-type name (R3
    /// deferred): `Some` for elements inside a `module Foo = …`, `None` at the
    /// namespace/file top level. A `let` binding under a holder records its
    /// `NodeKey` → `ModuleMemberInfo` so the backend emits it as a named public
    /// static method on that holder (e.g. `ListModule::fold`) rather than on the
    /// anonymous "Program" holder.
    let rec private translateModuleElem
        (ctx: PassContext)
        (ns: string option)
        (holder: string option)
        (m: ModuleElem<SyntaxToken>)
        : TDecl list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings ->
                    let tpat = translatePat ctx b.headPat

                    // Inside a named module: record where this binding's static
                    // method belongs (its source name on the holder type).
                    match holder with
                    | Some h ->
                        match memberNameOfBinding ctx b with
                        | ValueSome nm ->
                            ctx.Bindings.ModuleMembers.[(CstKeys.ofBinding b).Raw] <-
                                {
                                    Namespace = ns
                                    Holder = h
                                    Name = nm
                                }
                        | ValueNone -> ()
                    | None -> ()

                    let valT = translateBinding ctx b
                    TDecl.Let(tpat, valT, b.inlineToken.IsSome, typeOfKey ctx (CstKeys.ofBinding b))
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)) ]
        | ModuleElem.Type defs -> defs |> Seq.choose (tryTypeDecl ctx ns) |> List.ofSeq
        // A nested `module Foo = …` still surfaces its body flat at the enclosing
        // namespace (v1 has no module-scoped *types*), mirroring the analysis
        // passes' `CstWalk.implFileElems` flattening — but its *functions* now
        // carry the holder name `Foo`, suffixed `FooModule` when a type of the same
        // name shares the namespace (the exact F# rule that mandates
        // `[<CompilationRepresentation(ModuleSuffix)>]`), so they emit onto a real
        // holder type. Deeper nesting takes the innermost module's name (matching
        // the existing flatten; proper module qualification is a later rung —
        // docs/selfhost-handoff.md G10/R8).
        | ModuleElem.Module(ModuleDefn.ModuleDefn(ident = ident; body = ModuleDefnBody(elements = inner))) ->
            match inner with
            | ValueSome innerElems ->
                let moduleName = ctx.NameOf ident

                let holderName =
                    if
                        ctx.Types.Union.ContainsKey moduleName
                        || ctx.Types.Record.ContainsKey moduleName
                        || ctx.Types.Class.ContainsKey moduleName
                    then
                        moduleName + "Module"
                    else
                        moduleName

                innerElems
                |> Seq.collect (translateModuleElem ctx ns (Some holderName))
                |> List.ofSeq
            | ValueNone -> []
        | _ -> []

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        let decls =
            match file with
            | ImplementationFile.AnonymousModule elems ->
                elems |> Seq.collect (translateModuleElem ctx None None) |> List.ofSeq
            | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) ->
                elems |> Seq.collect (translateModuleElem ctx None None) |> List.ofSeq
            | ImplementationFile.Namespaces groups ->
                [
                    for g in groups do
                        let nsName, elems =
                            match g with
                            | NamespaceDeclGroup.Named(longIdent = li; elements = elems) ->
                                Some(longIdentText ctx li), elems
                            | NamespaceDeclGroup.Global(elements = elems) -> None, elems

                        yield! elems |> Seq.collect (translateModuleElem ctx nsName None)
                ]

        {
            Decls = decls
            Diagnostics = List.ofSeq ctx.Diagnostics
            // Snapshot so the backend can key the emitted IL type off the
            // representation string (G7) without the PassContext.
            IntrinsicReprTypes =
                ctx.Types.IntrinsicReprTypes
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
            // Snapshot the named-module placements (R3 deferred): the backend keys
            // off a binding's `NodeKey.Raw` to emit it on its holder type.
            ModuleMembers = ctx.Bindings.ModuleMembers |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
        }
