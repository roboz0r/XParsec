namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Expression / pattern translation for the Freeze pass: the recursive CST->TExpr
// projection and its helpers. The companion ``Freeze`` module (type-declaration
// surfacing + ``run``) opens this one for the entry points it projects from.

module FreezeExpr =

    // Public surface for the companion `Freeze` (type-declaration) module: the
    // entry points it projects member bodies / ctor args / field types from.
    let typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
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
            | Token.NumIEEE32
            | Token.NumIEEE32Hex
            | Token.NumIEEE32Octal
            | Token.NumIEEE32Binary ->
                // `f` suffix (case-insensitive via `stripSuffix`) stripped before
                // the invariant-culture single parse — the float32 twin of the
                // `float` arm above.
                TConstValue.Float32(
                    System.Single.Parse(stripSuffix "f" text, System.Globalization.CultureInfo.InvariantCulture)
                )
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

    /// Does `caseName` (optionally written with `qualifier`) name a case of an
    /// *external* (referenced-package) union the provider knows? Mirrors the
    /// Unification recogniser (`tryExternalCasePattern`) for the Freeze pattern
    /// path, so a cross-package `match o with Some x -> …` lowers to `TPat.Union`
    /// exactly as the local-union arm does (vesper-lib-test-plan Gap 2 Layer C).
    /// The lowering is identical to the local case — only the recognition differs.
    let private isExternalUnionCase (ctx: PassContext) (qualifier: string voption) (caseName: string) : bool =
        match ctx.Provider.TryLookupUnionCase caseName with
        | ValueSome uc ->
            match qualifier with
            | ValueNone -> true
            | ValueSome q -> SymbolKeyOps.shortName uc.UnionName = q
        | ValueNone -> false

    /// The `(consName, nilName)` case names of the list union a `[…]` literal,
    /// `[]`/`h :: t` pattern, or `::` construction targets. Mirrors the
    /// case-by-arity resolution in `translateListLikeLiteral`: a program-declared
    /// list union (its nullary case = the empty terminator, its single binary case
    /// = cons) drives its own factories. For the self-host `list.fs` and the
    /// external Vesper list (whose `[]`/`::` cases register / compile as
    /// `Empty`/`Cons`) this returns `("Cons", "Empty")`; the FSharp.Core fallback
    /// keeps `("Cons", "Nil")`.
    let private listCaseNames (ctx: PassContext) (ty: SemType) : string * string =
        match Unification.zonk ty with
        | TyUnion(unionKey, _) when (TypeRegistry.tryUnionByKey ctx.Types unionKey).IsSome ->
            let info = (TypeRegistry.tryUnionByKey ctx.Types unionKey).Value
            let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
            let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

            match nilCase, consCase with
            | Some n, Some c -> c.Name, n.Name
            | _ -> "Cons", "Empty"
        | TyUnion(listKey, _) when RuntimeNames.isVesperListKey listKey -> "Cons", "Empty"
        | _ -> "Cons", "Nil"

    /// Patterns Unification doesn't understand yet fall through loudly so the
    /// gap surfaces at translation time.
    let rec translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t

            n.Length > 0
            && System.Char.IsUpper n.[0]
            && (ctx.Types.CtorIndex.ContainsKey n || isExternalUnionCase ctx ValueNone n)
            ->
            // Nullary ctor in pattern position — a local union or an external
            // referenced-package one (`None`). Must precede the plain NamedSimple
            // arm. Both lower to the same `TPat.Union`; the node's type
            // (`typeOfKey`) already carries the right `TyUnion`, so the backend
            // routes local vs external off that.
            TPat.Union(ctx.NameOf t, EqArray.empty, ty)
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty)
        | Pat.Wildcard _ -> TPat.Wildcard ty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) -> TPat.Tuple(EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub }), ty)
        | Pat.EmptyBlock(lParen = ParenKind.List _) ->
            // `[]` pattern → the list union's nullary (empty) case, by arity.
            let _, nilName = listCaseNames ctx ty
            TPat.Union(nilName, EqArray.empty, ty)
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t` → the list union's binary (cons) case. The node's type
            // (`typeOfKey`) is the list `TyUnion` Unification resolved; the backend
            // routes local vs external off it, exactly like a named-ctor pattern.
            let consName, _ = listCaseNames ctx ty
            TPat.Union(consName, EqArray.ofList [ translatePat ctx headPat; translatePat ctx tailPat ], ty)
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
                EqArray.ofSeq (
                    seq {
                        for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translatePat ctx sub
                    }
                )

            TPat.Record(fields, ty)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                last.Length > 0
                && System.Char.IsUpper last.[0]
                && (li.Idents.Length = 1
                    && (ctx.Types.CtorIndex.ContainsKey last || isExternalUnionCase ctx ValueNone last)
                    || li.Idents.Length = 2
                       && (TypeRegistry.localQualifiedCase ctx.Types (ctx.NameOf li.Idents.[0]) last
                           || isExternalUnionCase ctx (ValueSome(ctx.NameOf li.Idents.[0])) last)))
            ->
            let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            let subPats =
                if args.Length = 1 then
                    match args.[0] with
                    | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) ->
                        EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub })
                    | Pat.EnclosedBlock(pat = inner) -> EqArray.singleton (translatePat ctx inner)
                    | Pat.Tuple(patterns = pats) -> EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub })
                    | sub -> EqArray.singleton (translatePat ctx sub)
                else
                    EqArray.ofSeq (seq { for sub in args -> translatePat ctx sub })

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
    /// really is a class name, not a shadowing local. An explicit type application
    /// (`Set<'T>(args)`) wraps the name in `Expr.TypeApp`; peel it so the
    /// construction lowers to `TExpr.New` exactly like the inference-pinned
    /// `Set(args)` form (the node's inferred type already carries the instantiation).
    ///
    /// `arity` is the type-argument count from an enclosing `Expr.TypeApp`
    /// (`ResizeArray<'T>()` → 1; a bare head → 0). Generic external types are keyed
    /// arity-suffixed in the provider (`ResizeArray\`1`), so the external lookup must
    /// try the suffixed name before the bare one — exactly the candidate order
    /// NameResolution uses.
    let rec private tryClassRef (ctx: PassContext) (arity: int) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            // Look an external type up by its arity-suffixed name first, then bare.
            let lookupShape (c: string) : ExternalTypeShape voption =
                let rec go names =
                    match names with
                    | [] -> ValueNone
                    | k :: rest ->
                        match ctx.Provider.TryLookupType k with
                        | ValueSome _ as found -> found
                        | ValueNone -> go rest

                go (
                    if arity > 0 then
                        [ SymbolKeyOps.arityName c arity; c ]
                    else
                        [ c ]
                )

            // `new`-less ctor sugar on an *external* class (`InvalidOperationException
            // "x"`): resolve the head through the active `open`s to its metadata name
            // so the `App(ClassRef …)` arm emits the same `TExpr.New` as `new T(…)`.
            // An abbreviation that expands to a class (`ResizeArray<'T>` →
            // `System.Collections.Generic.List<'T>`) resolves to the *underlying*
            // class's qualified name: the abbreviation itself is not a constructible
            // metadata type, so construction must lower to `new List<'T>()`. The
            // expansion args are irrelevant to the head name, so we apply the body
            // with `unit` placeholders. Mirrors `Infer.tryInferExternalCtorApp`;
            // without it Freeze's generic application path trips on the head's
            // external `TyClass`/abbrev type.
            // The returned name must match what the `Expr.New` arm derives from the
            // node's `TyClass` key (`qualifiedName key`) so codegen's member lookup
            // hits: that key is arity-suffixed for a generic type (`List\`1`). For a
            // class we re-suffix the bare qualified name (`arityName` is a no-op at
            // arity 0, so non-generic exceptions stay bare); an abbreviation's
            // expanded key already carries the suffix.
            let underlyingClassName (shape: ExternalTypeShape) (qualified: string) : string voption =
                match shape with
                | ExternalTypeShape.Class _ -> ValueSome(SymbolKeyOps.arityName qualified arity)
                | ExternalTypeShape.Abbrev(a, frozen) ->
                    match FrozenTypeBridge.instantiateDeclaring frozen (Array.create a BuiltinTypes.tyUnit) with
                    | TyClass(key, _) -> ValueSome(SymbolKeyOps.qualifiedName key)
                    | _ -> ValueNone
                | _ -> ValueNone

            let tryExternal (n: string) : string voption =
                match
                    OpenScope.tryQualify
                        ctx.Resolution.OpenScope
                        (fun c ->
                            match lookupShape c with
                            | ValueSome shape -> (underlyingClassName shape c).IsSome
                            | ValueNone -> false
                        )
                        n
                with
                | ValueSome c ->
                    match lookupShape c with
                    | ValueSome shape -> underlyingClassName shape c
                    | ValueNone -> ValueNone
                | ValueNone -> ValueNone

            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if ctx.Types.Class.ContainsKey n then
                    ValueSome n
                else
                    tryExternal n
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if ctx.Types.Class.ContainsKey n then
                    ValueSome n
                else
                    tryExternal n
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                tryExternal (li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
            | Expr.TypeApp(expr = inner; types = types) -> tryClassRef ctx types.Length inner
            | _ -> ValueNone

    /// Peel an `Expr.App` argument that may be a single `EnclosedBlock`
    /// wrapping a `Tuple` (the F# parser shape for `Point(3, 4)`) so
    /// downstream consumers see the constructor's declared arity directly.
    let peelCtorArgs
        (translate: Expr<SyntaxToken> -> TExpr)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : EqArray<TExpr> =
        if args.Length = 1 then
            match args.[0] with
            | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) ->
                EqArray.ofSeq (seq { for a in items -> translate a })
            | Expr.Tuple(exprs = items) -> EqArray.ofSeq (seq { for a in items -> translate a })
            | Expr.EnclosedBlock(expr = inner) -> EqArray.singleton (translate inner)
            | Expr.EmptyBlock _ -> EqArray.empty
            | a -> EqArray.singleton (translate a)
        else
            EqArray.ofSeq (seq { for a in args -> translate a })

    /// Same as `peelCtorArgs` but for a single argument expression
    /// (HighPrecedenceApp form / Expr.New).
    let peelOneArg (translate: Expr<SyntaxToken> -> TExpr) (arg: Expr<SyntaxToken>) : EqArray<TExpr> =
        match arg with
        | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> EqArray.ofSeq (seq { for a in items -> translate a })
        | Expr.Tuple(exprs = items) -> EqArray.ofSeq (seq { for a in items -> translate a })
        | Expr.EnclosedBlock(expr = Expr.EmptyBlock _) -> EqArray.empty
        | Expr.EnclosedBlock(expr = inner) -> EqArray.singleton (translate inner)
        | Expr.EmptyBlock _ -> EqArray.empty
        | a -> EqArray.singleton (translate a)

    /// The declaring nominal `SymbolKey` of a class/union receiver type — the
    /// `decl` slot of the `MemberKey` minted for an instance member access. Only
    /// called where the receiver is already known to be nominal (the active
    /// patterns / `InstanceMethodCall` guard on `TyClass`/`TyUnion`), so a
    /// non-nominal type is a Freeze invariant break.
    let private nominalDeclKey (ty: SemType) : SymbolKey =
        match Unification.zonk ty with
        | TyClass(key, _)
        | TyUnion(key, _) -> key
        | other -> failwithf "Freeze: expected a class/union receiver for a member access, got %A" other

    /// Look up `memberName` on `typeName` — a class or (P3d.3) a union
    /// augmentation. Returns the declaring type's `SymbolKey` (`info.Key`)
    /// alongside the member so the static-member path can mint a local
    /// `SymbolKey.MemberKey` off the resolved type (Phase 4).
    let private tryClassMember
        (ctx: PassContext)
        (typeName: string)
        (memberName: string)
        : (SymbolKey * TypeMemberInfo) voption =
        let pick (key: SymbolKey) (members: TypeMemberInfo[]) =
            match members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m -> ValueSome(key, m)
            | None -> ValueNone

        match ctx.Types.Class.TryGetValue typeName with
        | true, info -> pick info.Key info.Members
        | false, _ ->
            match ctx.Types.Union.TryGetValue typeName with
            | true, info -> pick info.Key info.Members
            | false, _ -> ValueNone

    /// `(+)`-as-a-value whose operands are a *project-local* nominal that declares
    /// the operator as a `static member` (the `Set.(+)` shape — `set.fs:821`, used
    /// by value in `Set.Union`'s `Seq.fold (+) …`). F# resolves such an operator
    /// value to the type's **own** static member, not the built-in arithmetic
    /// operator; so eta-expand it here into a closure whose body `call`s that static
    /// member — `fun a b -> T.op_Addition(a, b)` as a `StaticMethodCall` — rather
    /// than leaving a bare `External("op_Addition", …)` value. The latter is wrong
    /// two ways: codegen's eta path is only reached for module-level decls (not
    /// member bodies, so the value would survive to `buildExpr`'s catch-all as
    /// `Emit: unsupported expression: External`), and even when it *is* reached,
    /// `expandBuiltinOps` collapses the saturated `op_Addition` to an inline `add`
    /// opcode — emitting integer arithmetic over object references. Returns
    /// `ValueNone` for a built-in operator over primitives (`int (+)` etc.) or any
    /// operand whose type isn't a project-local nominal with that static member, so
    /// the existing `External` value path is untouched.
    let private tryOwnOperatorValue (ctx: PassContext) (key: NodeKey) (name: string) (ty: SemType) : TExpr voption =
        // A user-defined operator member always compiles to an `op_*` name; bail
        // early on anything else (a plain ident never reaches this).
        if not (name.StartsWith "op_") then
            ValueNone
        else
            // Peel the curried arrows to (paramTys, retTy). A non-function value
            // is not an operator passed by value.
            let rec arrows (t: SemType) =
                match Unification.zonk t with
                | TyFun(a, b) ->
                    let ps, r = arrows b
                    a :: ps, r
                | other -> [], other

            match arrows ty with
            | [], _ -> ValueNone
            | (operand :: _) as paramTys, retTy ->
                match Unification.zonk operand with
                | TyClass(operandKey, _)
                | TyUnion(operandKey, _) ->
                    match tryClassMember ctx (SymbolKeyOps.simpleName operandKey) name with
                    | ValueSome(declKey, m) when m.IsStatic ->
                        // One synthetic lambda parameter per arrow, keyed under the
                        // value-site offset (distinct per index, mirroring the
                        // `Expr.Function` synthetic-param mint). The body never
                        // re-enters the side tables, so the inline `SemType` carried
                        // on each `Var` is authoritative (no TyVar lookup).
                        let psKeyed =
                            paramTys
                            |> List.mapi (fun i pty ->
                                NodeKey.ofSynthetic (key.Offset + i) NodeKind.SynthLambdaBody, pty
                            )

                        let memberKey = LocalSymbolKey.ofMember declKey name MemberKind.Method

                        let body =
                            TExpr.StaticMethodCall(
                                memberKey,
                                EqArray.ofList [ for (k, pty) in psKeyed -> TExpr.Var(k, pty) ],
                                retTy
                            )

                        let lam, _ =
                            List.foldBack
                                (fun (k, pty) (inner, innerTy) ->
                                    let lamTy = TyFun(pty, innerTy)
                                    TExpr.Lambda(TPat.NamedSimple(k, pty), inner, lamTy), lamTy
                                )
                                psKeyed
                                (body, retTy)

                        ValueSome lam
                    | _ -> ValueNone
                | _ -> ValueNone

    /// Resolve `head.M` when the head is a local binding of a `TyClass`/`TyUnion`
    /// with a known member `M`. The parser folds the dot into the long ident
    /// rather than emitting `DotLookup` when the head is a regular identifier.
    let private tryLongIdentClassTail
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * TypeMemberInfo) voption =
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
                    | TyClass(typeKey, _)
                    | TyUnion(typeKey, _) ->
                        let typeName = SymbolKeyOps.simpleName typeKey
                        let memberName = ctx.NameOf li.Idents.[1]

                        match tryClassMember ctx typeName memberName with
                        | ValueSome(_, m) -> ValueSome(rb.BindingSite, Unification.zonk (TyVar tv), m)
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

    /// Resolve `ClassName.MemberName` to its static member info. `ValueNone` if
    /// either is unknown or the member is an instance member (use
    /// `tryLongIdentClassTail` for instance dispatch on a local binding).
    let private tryLongIdentStaticMember
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (SymbolKey * TypeMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let className = ctx.NameOf li.Idents.[0]
            let memberName = ctx.NameOf li.Idents.[1]

            tryClassMember ctx className memberName
            |> ValueOption.filter (fun (_, m) -> m.IsStatic)

    /// DU ctor reference (`Circle`, `Result2.Ok`, or an external `Some` / `None`),
    /// returning the case name. Excludes local bindings whose names happen to
    /// match a ctor — they have a `Binding` entry. An external case is recognised
    /// through the provider's reverse index; the case name alone is returned (the
    /// CtorRef arms read the declaring union off the node's resolved `TyUnion`
    /// type), so the local and external paths emit `TExpr.UnionCons` identically
    /// (vesper-lib-test-plan Gap 2 Layer B).
    let private tryCtorRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            // A local *or* external union declares `n` as a case.
            let isCase (n: string) =
                ctx.Types.CtorIndex.ContainsKey n || (ctx.Provider.TryLookupUnionCase n).IsSome

            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if isCase n then ValueSome n else ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if isCase n then ValueSome n else ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 2
                && TypeRegistry.localQualifiedCase ctx.Types (ctx.NameOf li.Idents.[0]) (ctx.NameOf li.Idents.[1])
                ->
                // `localQualifiedCase` already confirmed the case belongs to the
                // qualifier's union (arity-safe over `Choice\`2`…`Choice\`7`).
                ValueSome(ctx.NameOf li.Idents.[1])
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 2 ->
                // Qualified external union case (`Option.Some`): the head is an
                // external union, not a local one. Accept only when the resolved
                // union's short name matches the written qualifier.
                let typeName = ctx.NameOf li.Idents.[0]
                let caseName = ctx.NameOf li.Idents.[1]

                match ctx.Provider.TryLookupUnionCase caseName with
                | ValueSome uc when SymbolKeyOps.shortName uc.UnionName = typeName -> ValueSome caseName
                | _ -> ValueNone
            | _ -> ValueNone

    // Active patterns wrap the four `try*` helpers so each `translateExpr` arm
    // computes its guard once and binds the destructured result directly,
    // rather than re-evaluating in the body with a `ValueNone -> failwith
    // "unreachable"` fall-through.

    [<return: Struct>]
    let private (|ClassRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryClassRef ctx 0 e

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
    let private (|StaticMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (SymbolKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(declKey, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(declKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let private (|StaticMember|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (SymbolKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(declKey, _) -> ValueSome(declKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | ValueNone -> ValueNone

    /// `ClassName<'args>.Member` — a static member access on an *explicitly*
    /// instantiated generic class. It parses as `DotLookup(TypeApp(ClassName,
    /// <'args>), .Member)` rather than the folded `LongIdent[ClassName; Member]`
    /// the bare `ClassName.Member` form takes (`StaticMember` / `StaticMethod`).
    /// The type args only pin the generic instantiation (already carried on the
    /// node's `ty`); the receiver is a type, so it lowers to the same
    /// receiver-less static get / call. Returns the member's `Kind` so the caller
    /// routes a property read vs a method call (the method form is `App`-wrapped).
    [<return: Struct>]
    let private (|TypeAppStaticMember|_|)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        : (SymbolKey * string * ClassMemberKind) voption =
        match e with
        | Expr.DotLookup(expr = Expr.TypeApp(expr = classExpr); longIdentOrOp = LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 1
            ->
            let classNameOpt =
                match classExpr with
                | Expr.Ident t -> ValueSome(ctx.NameOf t)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent cli) when cli.Idents.Length = 1 ->
                    ValueSome(ctx.NameOf cli.Idents.[0])
                | _ -> ValueNone

            match classNameOpt with
            | ValueSome className ->
                let memberName = ctx.NameOf li.Idents.[0]

                match tryClassMember ctx className memberName with
                | ValueSome(declKey, m) when m.IsStatic -> ValueSome(declKey, memberName, m.Kind)
                | _ -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// `[1; 2; 3]` parses as `EnclosedBlock(ParenKind.List, Sequential [...])`;
    /// a one-item literal `[1]` skips the Sequential wrapper.
    let private listLiteralItems (body: Expr<SyntaxToken>) : Expr<SyntaxToken> list =
        match body with
        | Expr.Sequential(exprs = items) -> [ for x in items -> x ]
        | single -> [ single ]

    /// Concatenate the literal text of every string part via `ctx.NameOf`,
    /// rendering an interpolation hole (`StringPart.Expr`) through `onHole`.
    /// Shared by the IL-intrinsic and literal-string stitchers, which differ
    /// only in how a hole renders.
    let private foldStringParts
        (ctx: PassContext)
        (onHole: unit -> string)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
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
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()

    /// Stitch a value-level `Expr.ILIntrinsic` instruction string (e.g.
    /// `(# "ceq" … #)` → `"ceq"`), trimming surrounding whitespace. Mirrors
    /// `NameResolution.ilIntrinsicString` for the type-level intrinsic.
    let private stitchIlInstruction (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        (foldStringParts ctx (fun () -> "") parts).Trim()

    /// Dispatch discriminator for an instance member access (inheritance-plan
    /// §Subtle migrations). A `base.M(...)` / `base.X` receiver translates to a
    /// `TExpr.Var` whose binding site is some class's `BaseKey`; that must
    /// dispatch non-virtually so an `override` calling `base.M()` doesn't recurse
    /// into itself. The check is O(classes) per access — the gap doc accepts this
    /// for v1 (most files declare a handful of classes); a reverse index is a
    /// later optimisation.
    let private viaOfReceiver (ctx: PassContext) (receiver: TExpr) : CallVia =
        match receiver with
        | TExpr.Var(bindingSite, _) ->
            let mutable isBase = false

            for kv in ctx.Types.Class do
                if not isBase && kv.Value.BaseType.IsSome && kv.Value.BaseKey = bindingSite then
                    isBase <- true

            if isBase then CallVia.Base else CallVia.Self
        | _ -> CallVia.Self

    /// Recover segment `segName`'s declared type from receiver type `recvTy` — a
    /// record field, or a union / class instance-member return type — instantiated
    /// at the receiver's type arguments. `ValueNone` when the receiver isn't a
    /// known nominal or has no such member (the caller picks a fallback type).
    let private recoverFieldStepTy (ctx: PassContext) (recvTy: SemType) (segName: string) : SemType voption =
        let memberTy (typeParams, args) (members: TypeMemberInfo[]) =
            members
            |> Array.tryPick (fun m ->
                if m.Name = segName && not m.IsStatic then
                    Some(Unification.instantiateMember (typeParams, args) m.Type)
                else
                    None
            )

        let resolved =
            match Unification.zonk recvTy with
            | TyRecord(recKey, args) ->
                match TypeRegistry.tryRecordByKey ctx.Types recKey with
                | ValueSome info ->
                    info.Fields
                    |> Array.tryPick (fun f ->
                        if f.Name = segName then
                            Some(Unification.instantiateMember (info.TypeParams, args) f.Type)
                        else
                            None
                    )
                | ValueNone -> None
            | TyUnion(unionKey, args) ->
                match TypeRegistry.tryUnionByKey ctx.Types unionKey with
                | ValueSome info -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | TyClass(clsKey, args) ->
                match TypeRegistry.tryClassByKey ctx.Types clsKey with
                | ValueSome info ->
                    // A `this.x` chain segment may be an explicit `val` instance
                    // field or a primary-ctor parameter (both emitted as fields),
                    // not an instance member — `memberTy` alone misses it, and the
                    // caller would then fall back to the chain's *final* type,
                    // mis-typing the receiver (e.g. `this.stack.IsEmpty` typing
                    // `this.stack` as `bool`). Check fields first, then members.
                    let fieldTy =
                        Seq.append
                            (info.InstanceFields |> Seq.map (fun f -> f.Name, f.Type))
                            (info.CtorParams |> Seq.map (fun p -> p.Name, p.Type))
                        |> Seq.tryPick (fun (n, t) ->
                            if n = segName then
                                Some(Unification.instantiateMember (info.TypeParams, args) t)
                            else
                                None
                        )

                    match fieldTy with
                    | Some _ -> fieldTy
                    | None -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | _ -> None

        match resolved with
        | Some t -> ValueSome(Unification.zonk t)
        | None -> ValueNone

    /// Walk a class receiver's `inherit` chain for a non-static member `segName`,
    /// returning the *declaring* ancestor's instantiated nominal type. The caller
    /// reaches here after failing the receiver's own-class lookup, so the first
    /// strict ancestor that declares `segName` is returned. `ValueNone` when no
    /// ancestor declares it as an instance member, or a parent isn't a project-local
    /// class. Mirrors `Unification.tryClassChainMember` (the inference-side walk that
    /// already types `node.Key` where `Key` is on a base class) but yields the
    /// declaring type rather than the member's type — Freeze upcasts the receiver to
    /// it so codegen's receiver-keyed `resolveInstanceMember` lands on the class that
    /// actually emits `get_<seg>`. `seen` guards a cyclic `inherit` chain.
    let private tryInheritedMemberDecl (ctx: PassContext) (recvTy: SemType) (segName: string) : SemType voption =
        let rec walk (seen: string list) (ty: SemType) : SemType voption =
            match Unification.zonk ty with
            | TyClass(clsKey, args) ->
                let clsName = SymbolKeyOps.simpleName clsKey

                if List.contains clsName seen then
                    ValueNone
                else
                    match TypeRegistry.tryClassByKey ctx.Types clsKey with
                    | ValueSome info ->
                        let declaresHere =
                            info.Members |> Array.exists (fun m -> m.Name = segName && not m.IsStatic)

                        if declaresHere then
                            ValueSome(TyClass(clsKey, args))
                        else
                            match info.BaseType with
                            | ValueSome parentTy ->
                                walk (clsName :: seen) (Unification.instantiateMember (info.TypeParams, args) parentTy)
                            | ValueNone -> ValueNone
                    | ValueNone -> ValueNone
            | _ -> ValueNone

        walk [] recvTy

    /// One `receiver.seg` access node: `PropertyGet` for a class / union member,
    /// `FieldGet` otherwise. `recvTy` is the receiver's (un-zonked) type; `stepTy`
    /// is the segment's already-resolved result type.
    let private fieldStep
        (ctx: PassContext)
        (receiver: TExpr)
        (recvTy: SemType)
        (segName: string)
        (stepTy: SemType)
        : TExpr =
        let isMember (members: TypeMemberInfo[]) =
            members |> Array.exists (fun m -> m.Name = segName)

        match Unification.zonk recvTy with
        | TyClass(clsKey, _) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info when isMember info.Members ->
                let key = LocalSymbolKey.ofMember clsKey segName MemberKind.Property
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy)
            | _ ->
                // An *inherited* member (declared on a base class, e.g. `node.Key`
                // where `Key` is on the parent `SetTree`): upcast the receiver to the
                // declaring ancestor so codegen's receiver-keyed
                // `resolveInstanceMember` resolves `get_<seg>` on the class that
                // emits it (a reference-type upcast is a codegen no-op). Falls
                // through to `FieldGet` only when no ancestor declares it — a genuine
                // ctor-param / `val` field access.
                match tryInheritedMemberDecl ctx (Unification.zonk recvTy) segName with
                | ValueSome baseTy ->
                    let key =
                        LocalSymbolKey.ofMember (nominalDeclKey baseTy) segName MemberKind.Property

                    TExpr.PropertyGet(TExpr.Upcast(receiver, baseTy), key, viaOfReceiver ctx receiver, stepTy)
                | ValueNone -> TExpr.FieldGet(receiver, segName, stepTy)
        | TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info when isMember info.Members ->
                let key = LocalSymbolKey.ofMember unionKey segName MemberKind.Property
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy)
            | _ -> TExpr.FieldGet(receiver, segName, stepTy)
        // `arr.Length` on an intrinsic rank-1 array desugars to the core
        // `GetArrayLength` inline function (the `ldlen` mnemonic lives in
        // `ops-platform.fs`, spliced here by `InlineExpansion`). `array.Length`
        // parses as a local-headed LongIdent field chain (not `DotLookup`), so this
        // `fieldStep` arm is the one that fires; mirrors the `DotLookup` array guard.
        | TyConst(name, _) when name = RuntimeNames.arrayName 1 && segName = "Length" ->
            TExpr.App(TExpr.External("GetArrayLength", ValueNone, TyFun(recvTy, stepTy)), receiver, stepTy)
        | _ -> TExpr.FieldGet(receiver, segName, stepTy)

    /// `r.M(...)` where `r` has a class / union type and `M` is one of its
    /// instance methods. Returns the receiver expr + resolved member name so the
    /// `App` and `HighPrecedenceApp` invocation arms share one guard (the same
    /// convention as `ClassTailMethod` above) instead of repeating the
    /// receiver-type lookup verbatim.
    [<return: Struct>]
    let private (|InstanceMethodCall|_|)
        (ctx: PassContext)
        (funcExpr: Expr<SyntaxToken>)
        : (Expr<SyntaxToken> * SymbolKey * string) voption =
        match funcExpr with
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let memberName = ctx.NameOf li.Idents.[0]

            match Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r)) with
            | TyClass(typeKey, _)
            | TyUnion(typeKey, _) ->
                match tryClassMember ctx (SymbolKeyOps.simpleName typeKey) memberName with
                | ValueSome(_, m) when m.Kind = ClassMemberKind.Method -> ValueSome(r, typeKey, memberName)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// The `ResolvedExternalMember` Unification recorded for this node, if any.
    /// Used with a `&` conjunction so the external-member arms drop both the
    /// `ContainsKey` guard and the body's `failwith "unreachable"` re-lookup.
    [<return: Struct>]
    let private (|ExternalAccess|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : ResolvedExternalMember voption =
        ctx.Resolution.ExternalAccess.TryGetValue(CstKeys.ofExpr e)

    let rec translateExpr (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
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
            // If the final segment is an *external* instance member (e.g.
            // `e.Current` on a BCL `IEnumerator<'T>`), Unification recorded it in
            // `ExternalAccess` on this chain's key — pass it so the last step emits
            // a keyed `TExpr.ExternalMember` rather than a project-local `FieldGet`.
            translateLongIdentFieldChain ctx li ty (ctx.Resolution.ExternalAccess.TryGetValue key)
        // Static member on an *external* type reached through a folded LongIdent
        // (`System.Console.Out`, `Console.Out`) — Unification resolved the prefix
        // as a type and recorded the member in `ExternalAccess`. Emit the same
        // keyed `TExpr.ExternalMember` as the generic `DotLookup` form; always
        // static, so the type-name receiver is dropped.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when li.Idents.Length >= 2 ->
            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            TExpr.ExternalMember(ValueNone, info.Key, memberName, info.IsProperty, ty)
        // `new T(args)` — Unification stamps `ty` with the `TyClass`. The CST-side
        // fallback is purely defensive for error paths where Unification couldn't
        // pin the receiver.
        | Expr.New(typ = t; expr = argExpr) ->
            let className =
                match Unification.zonk ty with
                // Qualified so the backend's external-ctor recipe (`new
                // System.Exception(...)`) resolves; the backend strips to the bare
                // simple name for the project-local class lookup.
                | TyClass(n, _) -> SymbolKeyOps.qualifiedName n
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
            let argsList = peelCtorArgs (translateExpr ctx) args
            TExpr.New(className, argsList, ty)
        | Expr.HighPrecedenceApp(funcExpr = ClassRef ctx className; argExpr = arg) ->
            let argsList = peelOneArg (translateExpr ctx) arg
            TExpr.New(className, argsList, ty)
        // Class instance method invocation: `r.M(args)` →
        // `App(DotLookup(r, ., M), args)`.
        | Expr.App(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExprs = args) ->
            let receiver = translateExpr ctx r
            let argsList = peelCtorArgs (translateExpr ctx) args
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty)
        | Expr.HighPrecedenceApp(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExpr = arg) ->
            let receiver = translateExpr ctx r
            let argsList = peelOneArg (translateExpr ctx) arg
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty)
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
            let argsList = peelCtorArgs (translateExpr ctx) args

            let key =
                LocalSymbolKey.ofMember (nominalDeclKey receiverTy) memberName MemberKind.Method

            TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExpr = arg) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            let argsList = peelOneArg (translateExpr ctx) arg

            let key =
                LocalSymbolKey.ofMember (nominalDeclKey receiverTy) memberName MemberKind.Method

            TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty)
        // `p.X` (property) parses as `Expr.LongIdentOrOp(LongIdent[p; X])` when
        // the head is a regular identifier. Anything not a class property falls
        // to the chained FieldGet path below.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailProperty ctx (bindingSite, receiverTy, memberName))) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)

            let key =
                LocalSymbolKey.ofMember (nominalDeclKey receiverTy) memberName MemberKind.Property

            TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty)
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExprs = args) ->
            let argsList = peelCtorArgs (translateExpr ctx) args
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.StaticMethodCall(key, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExpr = arg) ->
            let argsList = peelOneArg (translateExpr ctx) arg
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.StaticMethodCall(key, argsList, ty)
        // `ClassName<'args>.Method args` — static-method call on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Singleton value`). The
        // `<'args>`-bearing receiver makes the funcExpr a `DotLookup` over a
        // `TypeApp` rather than a folded `LongIdent`; same `StaticMethodCall`
        // lowering as the folded `StaticMethod` arms above.
        | Expr.App(funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExprs = args) ->
            let argsList = peelCtorArgs (translateExpr ctx) args
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.StaticMethodCall(key, argsList, ty)
        | Expr.HighPrecedenceApp(
            funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExpr = arg) ->
            let argsList = peelOneArg (translateExpr ctx) arg
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Method
            TExpr.StaticMethodCall(key, argsList, ty)
        // `ClassName.X` — static property read (or method-as-value).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMember ctx (declKey, memberName))) ->
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Property
            TExpr.StaticPropertyGet(key, ty)
        | CtorRef ctx caseName ->
            // Bare or qualified ctor reference outside an App. v1 distinguishes
            // nullary ctor (→ `UnionCons`) from ctor-as-value (`let f = Circle`,
            // typed `TyFun(_, TyUnion _)` → External) by the result type.
            match Unification.zonk ty with
            | TyUnion(_, _) -> TExpr.UnionCons(caseName, EqArray.empty, ty)
            // Function-typed ctor-as-value; codegen can eta-expand to a
            // UnionCons lambda.
            | _ -> TExpr.External(caseName, ValueNone, ty)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty
        | Expr.App(CtorRef ctx caseName, args) ->
            // Ctor application: `Circle 1.0` or `Rectangle(2.0, 3.0)`. F# treats
            // DU arguments as a single tuple; the TAST flattens it back to a
            // per-field list (the same peel the class-ctor arms use) so consumers
            // see the ctor's declared arity directly.
            TExpr.UnionCons(caseName, peelCtorArgs (translateExpr ctx) args, ty)
        | Expr.HighPrecedenceApp(funcExpr = CtorRef ctx caseName; argExpr = arg) ->
            TExpr.UnionCons(caseName, peelOneArg (translateExpr ctx) arg, ty)
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
        | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> translateLet ctx kw bindings body
        | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner) ->
            translateListLikeLiteral ctx ty false (listLiteralItems inner)
        | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner) ->
            translateListLikeLiteral ctx ty true (listLiteralItems inner)
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty
        | Expr.Tuple(exprs = items) -> TExpr.Tuple(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty)
        | Expr.Sequential(exprs = items) ->
            TExpr.Sequential(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty)
        // The annotation has no runtime representation — it only constrained
        // types in Unification; the TAST carries the inferred type inline.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        // Casts carry the resolved node type (`ty`): the target type for
        // `:>` / `:?>`, and `bool` for `:?` — Unification validated the
        // coercion via `subsumes`, codegen emits the box / castclass / isinst.
        | Expr.StaticUpcast(expr = inner) -> TExpr.Upcast(translateExpr ctx inner, ty)
        | Expr.DynamicDowncast(expr = inner) -> TExpr.Downcast(translateExpr ctx inner, ty)
        | Expr.DynamicTypeTest(expr = inner) ->
            // `ty` is the `bool` result; the tested-against type was stashed by
            // Unification (`inferDynamicTypeTest`) keyed by this node.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Freeze: no recorded type-test target for %O" key

            TExpr.TypeTest(translateExpr ctx inner, testTy, ty)
        | Expr.EmptyBlock(lParen = ParenKind.List _) -> translateListLikeLiteral ctx ty false []
        | Expr.EmptyBlock(lParen = ParenKind.Array _) -> translateListLikeLiteral ctx ty true []
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) -> TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty)
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            // How the source yields its enumerator was resolved by Unification and
            // stashed by this node's key; absent ⇒ the §4.2 interface path (range
            // sources and IEnumerable<'T> sources alike).
            let enumerator =
                match ctx.Resolution.ForInShape.TryGetValue key with
                | ValueSome shape -> shape
                | ValueNone -> ForInEnumeratorG.Interface

            TExpr.ForIn(translatePat ctx pat, translateExpr ctx src, translateExpr ctx body, enumerator, ty)
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
            // `arr.[i] <- v` desugars to the core `SetArray` inline function (the
            // write mirror of the `IndexedLookup` → `GetArray` read path below):
            // the `stelem` mnemonic lives in Vesper.Core's `ops-platform.fs`,
            // spliced at this use site by `InlineExpansion` — never invented in this
            // target-agnostic pass. Emit a curried `External` call whose type is
            // rebuilt from the resolved operand types (`ty` is the assignment's
            // `unit` result).
            | Expr.IndexedLookup(expr = arrE; indexExpr = idxE) ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
                let valTy = typeOfKey ctx (CstKeys.ofExpr right)
                let valuePartial = TyFun(valTy, ty)
                let idxPartial = TyFun(idxTy, valuePartial)
                let setExpr = TExpr.External("SetArray", ValueNone, TyFun(arrTy, idxPartial))
                let app1 = TExpr.App(setExpr, translateExpr ctx arrE, idxPartial)
                let app2 = TExpr.App(app1, translateExpr ctx idxE, valuePartial)
                TExpr.App(app2, translateExpr ctx right, ty)
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

                    // Field reads for the intermediate segments — the assigned slot
                    // is the final one, handled by the `FieldSet` below. Same chain
                    // walk as `translateLongIdentFieldChain`, stopping one short.
                    for i = 1 to lastIdx - 1 do
                        let segName = ctx.NameOf receiverIdents.[i]

                        let stepTy =
                            match recoverFieldStepTy ctx currTy segName with
                            | ValueSome t -> t
                            | ValueNone -> currTy

                        curr <- fieldStep ctx curr currTy segName stepTy
                        currTy <- stepTy

                    curr

                let lastName = ctx.NameOf receiverIdents.[lastIdx]
                TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty)
            | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty)
        | Expr.Record(fieldInitializers = inits) ->
            let fields =
                EqArray.ofSeq (
                    seq {
                        for FieldInitializer(longIdent = li; expr = e) in inits ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                    }
                )

            TExpr.RecordCons(fields, ty)
        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            let overrides =
                EqArray.ofSeq (
                    seq {
                        for FieldInitializer(longIdent = li; expr = e) in inits ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                    }
                )

            TExpr.RecordClone(translateExpr ctx src, overrides, ty)
        // Member access on an *external* type (static `Type.Member` or instance
        // `value.Member`) that Unification resolved through the provider — emit a
        // keyed `TExpr.ExternalMember`. A static
        // access drops the type-name receiver (`info.IsStatic`).
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when
            li.Idents.Length = 1
            ->
            let memberName = ctx.NameOf li.Idents.[0]

            let receiver =
                if info.IsStatic then
                    ValueNone
                else
                    ValueSome(translateExpr ctx r)

            TExpr.ExternalMember(receiver, info.Key, memberName, info.IsProperty, ty)
        // `ClassName<'args>.Prop` — local static property read on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Empty`). Same lowering as the
        // folded `ClassName.Member` form; the `<'args>` only pinned the generic
        // instantiation in inference and is carried on `ty`. The method form
        // (`Set<'T>.Singleton value`) is `App`-wrapped and handled with the other
        // static-method arms.
        | TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Property) ->
            let key = LocalSymbolKey.ofMember declKey memberName MemberKind.Property
            TExpr.StaticPropertyGet(key, ty)
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let memberName = ctx.NameOf li.Idents.[0]
            let rTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r))
            let receiver = translateExpr ctx r

            // A class/union receiver's member access is a `PropertyGet` (a
            // method-as-value keeps the same shape — codegen eta-expands);
            // anything else reads a record/tuple field.
            match rTy with
            | TyClass _
            | TyUnion _ ->
                let key =
                    LocalSymbolKey.ofMember (nominalDeclKey rTy) memberName MemberKind.Property

                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty)
            // `(expr).Length` on an intrinsic rank-1 array desugars to the core
            // `GetArrayLength` inline function — the `ldlen` mnemonic lives in
            // `ops-platform.fs`, spliced by `InlineExpansion`. Mirrors the
            // `fieldStep` array guard (the LongIdent-chain form).
            | TyConst(name, _) when name = RuntimeNames.arrayName 1 && memberName = "Length" ->
                TExpr.App(TExpr.External("GetArrayLength", ValueNone, TyFun(rTy, ty)), receiver, ty)
            | _ -> TExpr.FieldGet(receiver, memberName, ty)
        | Expr.Null _ -> TExpr.Null ty
        | Expr.Range(fromExpr = a; toExpr = b) -> TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty)
        // `arr.[i]` desugars to the core `GetArray` inline function (mirroring F#'s
        // `IntrinsicFunctions.GetArray`): the `ldelem` mnemonic lives in
        // Vesper.Core's `ops-platform.fs`, spliced at this use site by
        // `InlineExpansion` — never invented in this target-agnostic pass. Mirrors
        // the operator path (`translateInfix`): emit a curried `External` call whose
        // type is rebuilt from the resolved operand types. `ty` is the element type.
        | Expr.IndexedLookup(expr = r; indexExpr = idx) ->
            let arrTy = typeOfKey ctx (CstKeys.ofExpr r)
            let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
            let partialTy = TyFun(idxTy, ty)
            let getTy = TyFun(arrTy, partialTy)
            let getExpr = TExpr.External("GetArray", ValueNone, getTy)
            let app1 = TExpr.App(getExpr, translateExpr ctx r, partialTy)
            TExpr.App(app1, translateExpr ctx idx, ty)
        | Expr.ILIntrinsic(instrParts = parts; args = args) ->
            let opCode = stitchIlInstruction ctx parts
            let tArgs = EqArray.ofSeq (seq { for a in args -> translateExpr ctx a })

            // The tokenful array opcodes (`newarr`/`ldelem.any`) carry a single
            // element-type operand. The source `!0` placeholder is unparsed tokens,
            // so the element is recovered from the node's declared types — `newarr`'s
            // result is the array (`elem` = its argument), `ldelem`'s result IS the
            // element. The mnemonics ORIGINATE in per-target library source
            // (`array.fs`'s `zeroCreate`, `ops-platform.fs`'s `GetArray`), so this is
            // interpreting source IL, not inventing it. The mnemonic is normalised
            // (`ldelem.any` → `ldelem`) to the form codegen's emit arm reads.
            if opCode.StartsWith "newarr" then
                let elem =
                    match Unification.zonk ty with
                    | TyConst(name, eargs) when name = RuntimeNames.arrayName 1 && eargs.Length = 1 -> eargs.[0]
                    | other -> failwithf "Freeze: 'newarr' result is not a rank-1 array: %A" other

                TExpr.ILIntrinsic("newarr", ValueSome elem, tArgs, ty)
            elif opCode.StartsWith "ldelem" then
                TExpr.ILIntrinsic("ldelem", ValueSome(Unification.zonk ty), tArgs, ty)
            elif opCode.StartsWith "stelem" then
                // `arr.[i] <- v` / `SetArray`. The store's result is `unit`, so the
                // element type is recovered from the value operand (the 3rd arg:
                // array, index, value), not the node's result type as `ldelem` does.
                let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[2]))
                TExpr.ILIntrinsic("stelem", ValueSome elem, tArgs, ty)
            elif opCode.StartsWith "box" then
                // `box value` — the boxed element type is the *argument's* static
                // type (the result is always `obj`), so recover it from the single
                // value operand. A value type emits `box <T>`; a reference type's
                // box is the JIT-erased identity (codegen leaves it as `box`, which
                // the runtime treats as a no-op on a ref type).
                let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[0]))
                TExpr.ILIntrinsic("box", ValueSome elem, tArgs, ty)
            else
                TExpr.ILIntrinsic(opCode, ValueNone, tArgs, ty)
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
                        | ValueNone -> EqArray.empty

                    peel
                        inner
                        ({
                            Constraints = cs
                            Body = translateExpr ctx optE
                         }
                         :: acc)
                | other -> translateExpr ctx other, acc

            let defaultExpr, clauses = peel e []
            TExpr.StaticOptimization(EqArray.ofList clauses, defaultExpr, ty)
        | _ ->
            // TODO: extend as the subset grows; surface the unhandled case
            // loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : EqArray<TMatchArm> =
        EqArray.ofSeq (
            seq {
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
            }
        )

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) : TExpr =
        match e with
        | Expr.String(parts = parts) ->
            match Unification.zonk ty with
            | TyClass(key, _) when RuntimeNames.isPrintfFormatKey key ->
                // Format literal at a printf call site (typed by
                // `Unification.tryInferPrintfApp`). It denotes `new
                // PrintfFormat<…>(text)` — the single `value: string` ctor.
                TExpr.New(
                    PrintfSpec.printfFormatName,
                    EqArray.singleton (
                        TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), BuiltinTypes.tyString)
                    ),
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
        foldStringParts ctx (fun () -> "{<expr>}") parts

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

            // An own-class static-operator member used by value (`Set.(+)`) resolves
            // to that member, not the built-in operator: eta-expand to a closure
            // calling it, ahead of the generic `External` value path.
            match tryOwnOperatorValue ctx key name ty with
            | ValueSome lam -> lam
            | ValueNone ->
                // Stamp the resolved `SymbolKey.ValueKey` when NameResolution recorded
                // one (provider hit). Lets codegen distinguish a canonical
                // `Vesper.Printf.printfn` from a user shadow `MyMod.printfn` by
                // identity rather than name suffix (vesper-set-sprint-plan §0.1 / M1).
                let symKey = ctx.Resolution.ExternalValue.TryGetValue key
                TExpr.External(name, symKey, ty)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// head segment's TAST node is a `Var` pointing back at the local binding.
    and private translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        (lastExternal: ResolvedExternalMember voption)
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
            let segName = ctx.NameOf li.Idents.[i]
            // Intermediate steps recover the segment's declared type from the
            // receiver — a record/union/class field, or a union/class *instance
            // member* return type (so a chain through a member returning a union,
            // `xs.Tail.Head`, keeps `xs.Tail : Lst<_>` instead of collapsing to the
            // chain's final type). The last step uses the whole chain's `finalTy`.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    match recoverFieldStepTy ctx currTy segName with
                    | ValueSome t -> t
                    | ValueNone -> finalTy

            // PropertyGet for a class/union member (codegen calls its `get_<name>`,
            // eta-expanding a method-as-value if needed), FieldGet otherwise. The
            // last segment of an external instance access (`e.Current`) emits a
            // keyed `TExpr.ExternalMember` against the receiver built so far — the
            // BCL interface/class member-ref path, not a project-local field.
            curr <-
                match lastExternal with
                | ValueSome info when i = li.Idents.Length - 1 && not info.IsStatic ->
                    TExpr.ExternalMember(ValueSome curr, info.Key, segName, info.IsProperty, stepTy)
                | _ -> fieldStep ctx curr currTy segName stepTy

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
        | ValueSome DesugaredForm.ConsExpr ->
            // `h :: t` → `UnionCons("Cons", [h; t])` against the resolved list
            // union — the same shape `[…]` literals lower to (one cons cell).
            let consName, _ = listCaseNames ctx resultTy
            TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx left; translateExpr ctx right ], resultTy)
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
            // An array literal's zonked type is the generic intrinsic
            // `TyConst("[]", [elem])`; a list
            // literal's is `TyRecord`/`TyUnion`. Pull the element out of whichever.
            | TyConst(_, args) when args.Length = 1 -> args.[0]
            | TyRecord(_, args) when args.Length = 1 -> args.[0]
            | TyUnion(_, args) when args.Length = 1 -> args.[0]
            | _ -> TyVar(TypeVar())

        // A program-declared list union (resolved via the `'T list = List<'T>`
        // abbrev — see `Unification.listLiteralTy`) drives `[…]` construction
        // through that union's own case factories: nullary case = empty
        // terminator, single binary case = cons. Absent it (a normal program, or
        // any array literal), the FSharp.Core `Cons`/`Nil` nominal is the default.
        // Arrays never retarget — always the list chain + `Array.ofList` boundary.
        let listTy, consName, nilName =
            match zonked with
            | TyUnion(unionKey, _) when not isArray && (TypeRegistry.tryUnionByKey ctx.Types unionKey).IsSome ->
                let info = (TypeRegistry.tryUnionByKey ctx.Types unionKey).Value
                let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
                let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

                match nilCase, consCase with
                | Some n, Some c -> zonked, c.Name, n.Name
                | _ -> TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy), "Cons", "Nil"
            // The external Vesper list: a bare-program literal a consumer drove
            // onto the Vesper cons-list (`Unification.listLiteralTy` /
            // `resolveListLiterals`). Its `Cons` / `Nil` factories are minted by the
            // backend's `TryEmitUnionCons` Vesper case — BCL-only, no FSharp.Core.
            // It is an *external* union, so it is absent from `ctx.Types.Union` and
            // is not caught by the user-union arm above. Recognition (bare /
            // arity-suffixed union name, or the lowercase abbreviation) is shared
            // with codegen via `RuntimeNames.isVesperListKey`, so the `` `N ``-strip isn't re-derived here.
            | TyUnion(listKey, _) when not isArray && RuntimeNames.isVesperListKey listKey -> zonked, "Cons", "Empty"
            | _ -> TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy), "Cons", "Nil"

        let listExpr =
            let nil = TExpr.UnionCons(nilName, EqArray.empty, listTy)

            items
            |> List.foldBack (fun item acc ->
                TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx item; acc ], listTy)
            )
            <| nil

        if isArray then
            let arrayTy = TyConst(RuntimeNames.arrayName 1, EqArray.singleton elemTy)
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
        // Fold elifs right-to-left, each nested as the else-branch of the previous.
        // A missing else is `else ()` (F# spec): inference has already constrained
        // the then/elif branches and the whole expression to `unit`, so synthesize a
        // `unit` constant as the innermost else.
        let mutable nestedElse =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> translateExpr ctx e
            | ValueNone -> TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit)

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
        (keyword: LetOrUseKeyword<SyntaxToken>)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : TExpr =
        let bodyExpr = CstWalk.requireLetBody body
        let mutable result = translateExpr ctx bodyExpr
        let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr bodyExpr)

        // `use` / `use!` bind a disposable: each binding folds to a `TExpr.Use`
        // (codegen wraps the body in a `try … finally Dispose()` region, B-5)
        // rather than a plain `TExpr.Let`.
        let isUse =
            match keyword with
            | LetOrUseKeyword.Use _
            | LetOrUseKeyword.UseBang _ -> true
            | LetOrUseKeyword.Let _
            | LetOrUseKeyword.LetBang _ -> false

        for i = bindings.Length - 1 downto 0 do
            let b = bindings.[i]
            let tpat = translatePat ctx b.headPat
            let valT = translateBinding ctx b

            result <-
                if isUse then
                    // An external (BCL) binder's keyed `Dispose` is recorded by
                    // Unification under the head-pattern's key; a project-local binder
                    // has none and codegen takes the duck-typed direct call (§4.3).
                    let dispose = ctx.Resolution.UseDispose.TryGetValue(CstKeys.ofPat b.headPat)
                    TExpr.Use(tpat, valT, result, dispose, resultTy)
                else
                    TExpr.Let(tpat, valT, result, resultTy)

        result

    and translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr
