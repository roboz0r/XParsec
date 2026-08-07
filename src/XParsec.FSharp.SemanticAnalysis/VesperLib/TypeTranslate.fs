namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open VesperLibTyparCapture

/// CST → `FrozenType` translation, with the small token/identifier/attribute helpers the
/// rest of the extractor reuses. Calls thread an `ExtractCtx` (type-name resolution), a
/// `TyparCollector` (typar interning) and a `ConstraintCollector` (inline `when …`).
module VesperLibTypeTranslate =

    let nameOfTok (lexed: Lexed) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT)
        | TokenIndex.Virtual -> ""

    let longIdentName (lexed: Lexed) (li: LongIdent<SyntaxToken>) : string =
        let parts = [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed li.Idents.[i] ]

        String.concat "." parts

    let longIdentShortName (lexed: Lexed) (li: LongIdent<SyntaxToken>) : string =
        if li.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed li.Idents.[li.Idents.Length - 1]

    let identOrOpName (lexed: Lexed) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed tok)
        // `(::)` is a binding head only the contract surface needs to name (cons has no `op_`
        // member in expression position), so it is mapped before the shared resolver.
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) when opTok.Token = Token.KWColonColon ->
            ValueSome OperatorData.OpColonColon
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) ->
            OperatorNames.ofParenSymbolic (nameOfTok lexed opTok) opTok
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome OperatorData.OpRange
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome OperatorData.OpRangeStep
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome OperatorData.OpNil
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) ->
            // Active-pattern compiled names are non-trivial — defer.
            ValueNone

    /// First attribute whose short name (last segment) matches a candidate,
    /// ignoring the optional `Attribute` suffix.
    let findAttribute
        (lexed: Lexed)
        (attrs: Attributes<SyntaxToken> voption)
        (candidates: string list)
        : ObjectConstruction<SyntaxToken> voption =
        let candidateSet = Set.ofList candidates

        let matchName (name: string) =
            let trimmed =
                if name.EndsWith "Attribute" then
                    name.Substring(0, name.Length - "Attribute".Length)
                else
                    name

            Set.contains trimmed candidateSet

        let mutable found = ValueNone

        match attrs with
        | ValueNone -> ()
        | ValueSome sets ->
            for i in 0 .. sets.Length - 1 do
                if found.IsNone then
                    let (AttributeSet(_, items, _)) = sets.[i]

                    for j in 0 .. items.Length - 1 do
                        if found.IsNone then
                            let (Attribute(_, construction), _) = items.[j]

                            let typ =
                                match construction with
                                | ObjectConstruction(typ = t)
                                | InterfaceConstruction(typ = t) -> t

                            let attrName =
                                match typ with
                                | Type.NamedType li -> longIdentShortName lexed li
                                | Type.GenericType(li, _, _, _, _) -> longIdentShortName lexed li
                                | _ -> ""

                            if matchName attrName then
                                found <- ValueSome construction

        found

    let constructionExpr (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> voption =
        match oc with
        | ObjectConstruction(_, e) -> ValueSome e
        | InterfaceConstruction _ -> ValueNone

    /// Text of a parsed string-literal expression. Ignores expression holes and other
    /// interpolation artefacts: a compiled-name argument is never interpolated.
    let stringExprText
        (lexed: Lexed)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for i in 0 .. parts.Length - 1 do
            match parts.[i] with
            | StringPart.Text tok -> sb.Append(nameOfTok lexed tok) |> ignore
            | StringPart.EscapeSequence tok ->
                let raw = nameOfTok lexed tok
                // Preserve source-level text; full escape decoding is the
                // lexer's job and unneeded for attribute args.
                sb.Append raw |> ignore
            | _ -> ()

        sb.ToString()

    let tryCompiledName (lexed: Lexed) (attrs: Attributes<SyntaxToken> voption) : string voption =
        match findAttribute lexed attrs [ "CompiledName" ] with
        | ValueNone -> ValueNone
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> ValueNone
            | ValueSome argExpr ->
                // The argument is `("Foo")`.
                let rec stripParens (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> stripParens inner
                    | _ -> e

                match stripParens argExpr with
                | Expr.String(_, parts, _) ->
                    let s = stringExprText lexed parts
                    if s.Length > 0 then ValueSome s else ValueNone
                | Expr.Const(Constant.Literal tok) ->
                    let raw = nameOfTok lexed tok
                    let trimmed = raw.Trim([| '"' |])
                    if trimmed.Length > 0 then ValueSome trimmed else ValueNone
                | _ -> ValueNone

    /// True iff the module-level attributes carry `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`.
    let hasModuleSuffix (lexed: Lexed) (attrs: Attributes<SyntaxToken> voption) : bool =
        match findAttribute lexed attrs [ "CompilationRepresentation" ] with
        | ValueNone -> false
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> false
            | ValueSome argExpr ->
                // Heuristic: the token text "ModuleSuffix" anywhere in the expression.
                let rec containsModuleSuffix (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> containsModuleSuffix inner
                    | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed li
                        name = "ModuleSuffix"
                    | Expr.DotLookup(_, _, LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed li
                        name = "ModuleSuffix"
                    | _ -> false

                containsModuleSuffix argExpr

    /// True iff the module-level attributes carry `[<AutoOpen>]` — its members are in
    /// scope unqualified for a consumer of the package.
    let isAutoOpen (lexed: Lexed) (attrs: Attributes<SyntaxToken> voption) : bool =
        findAttribute lexed attrs [ "AutoOpen" ] |> ValueOption.isSome

    /// True iff the type-level attributes carry `[<RequireQualifiedAccess>]` — a bare `Red`
    /// for `[<RequireQualifiedAccess>] type Color = Red | …` must NOT resolve.
    let isRequireQualifiedAccess (lexed: Lexed) (attrs: Attributes<SyntaxToken> voption) : bool =
        findAttribute lexed attrs [ "RequireQualifiedAccess" ] |> ValueOption.isSome

    /// Resolve a long-identifier type name to its canonical compiled name, using the
    /// per-file open prefixes (newest first) as candidate qualifiers when the short-name
    /// lookup misses. Own-package names win: a local type shadows a dependency's.
    let resolveTypeName
        (ctx: ExtractCtx)
        (openPrefixes: string list)
        (name: string)
        (arity: int)
        : Result<string, string> =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        let isQualified = name.IndexOf '.' >= 0

        // A *generic* type is keyed by its arity-suffixed compiled name (`List`1`) while
        // source writes the bare head (`List`), so probe the suffixed form first.
        let forms (n: string) : string list =
            if arity > 0 then
                [ SymbolKeyOps.arityName n arity; n ]
            else
                [ n ]

        // What comes back out is the matched key's own canonical name, never the spelling
        // that matched: the shape tables downstream are addressed by that rendering.
        let inQualified (n: string) : string option =
            forms n
            |> List.tryPick (fun f ->
                match ExtractCtx.tryTypeKey ctx f with
                | ValueSome key -> Some(SymbolKeyOps.typeMetaName key)
                | ValueNone -> None
            )

        let inAmbient (n: string) : string option =
            forms n |> List.tryFind (fun k -> (ctx.AmbientShapes k).IsSome)

        match inQualified name with
        | Some k -> Ok k
        | None ->
            // A fully-qualified reference to a dependency's or the BCL's type
            // (`System.Collections.Generic.List`). Restricted to dotted names: a bare name
            // is a *short* name that the own-package index below must get first.
            match (if isQualified then inAmbient name else None) with
            | Some k -> Ok k
            | None ->
                // Short-name index hit — only for a *bare* name. A *qualified*
                // `System.Collections.Generic.List` must NOT collapse onto a local `List`
                // (the cons-list union): the written qualifier names a different type.
                let shortHit =
                    match ctx.Types.TryGetValue short with
                    | true, v when not isQualified -> ValueSome v
                    | _ -> ValueNone

                match shortHit with
                // An EXACT (short-name, arity) hit wins immediately. A disagreeing arity
                // (`Fun`2` vs `Fun`3`) falls through to the arity-aware open-prefix
                // resolution, and is accepted only as a last resort below.
                | ValueSome(recArity, compiled) when recArity = arity -> Ok compiled
                | _ ->
                    let mutable hit = ValueNone

                    for prefix in openPrefixes do
                        if hit.IsNone then
                            let candidate = prefix + "." + name

                            match inQualified candidate with
                            | Some k -> hit <- ValueSome k
                            | None ->
                                match inAmbient candidate with
                                | Some k -> hit <- ValueSome k
                                | None -> ()

                    match hit with
                    | ValueSome c -> Ok c
                    | ValueNone ->
                        match shortHit with
                        | ValueSome(_, compiled) -> Ok compiled
                        | ValueNone -> Error(sprintf "Unresolved type name '%s'" name)

    let private primitiveNames: Set<string> =
        RuntimeNames.numericTypeNames + RuntimeNames.referencePrimitiveNames

    /// For a SOURCE-WRITTEN name, before any identity exists for it — the one position where
    /// a bare spelling is all there is (it may still be an ALIAS, which no key spells).
    let isPrimitiveName (s: string) = primitiveNames.Contains s

    /// `isPrimitiveName` for a caller holding the resolved identity: exact, because the key
    /// carries the `Vesper` namespace and the arity a bare spelling cannot.
    let private isPrimitiveKey =
        RuntimeNames.isKeyIn (RuntimeNames.numericKeys @ RuntimeNames.referencePrimitiveKeys)

    /// Source-text name of a typar (the part after `'` or `^`), or `ValueNone` for an
    /// anonymous one, whose constraint participation cannot be addressed by name later.
    let typarName (lexed: Lexed) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(_, identTok)
        | Typar.Static(_, identTok) -> ValueSome(nameOfTok lexed identTok)
        | Typar.Anon _ -> ValueNone

    /// Walk a `when …` clause and emit `RawConstraint` entries into the collector.
    let captureConstraints (lexed: Lexed) (acc: ConstraintCollector) (clauses: TyparConstraints<SyntaxToken>) : unit =
        for c in clauses.Constraints do
            match c with
            | Constraint.Equality(t, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Equality))
                | ValueNone -> ()
            | Constraint.Comparison(t, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Comparison))
                | ValueNone -> ()
            | Constraint.Struct(t, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Struct))
                | ValueNone -> ()
            | Constraint.ReferenceType(t, _, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.ReferenceType))
                | ValueNone -> ()
            | Constraint.Nullness(t, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Nullness))
                | ValueNone -> ()
            | Constraint.NotNull(t, _, _, _) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.NotNull))
                | ValueNone -> ()
            | Constraint.MemberTrait(staticTypars, _, _, _, _, memberSig, _) ->
                let names =
                    match staticTypars with
                    | StaticTypars.Single t ->
                        match typarName lexed t with
                        | ValueSome n -> [ n ]
                        | ValueNone -> []
                    | StaticTypars.OrList(_, items, _, _) ->
                        [
                            for k in 0 .. items.Length - 1 do
                                match typarName lexed items.[k] with
                                | ValueSome n -> yield n
                                | ValueNone -> ()
                        ]

                if not (List.isEmpty names) then
                    // `static member (+) : ^T1 * ^T2 -> ^T3` parses as a
                    // `CurriedSig` (args as one asterisk-joined `ArgsSpec`).
                    // Property sigs (`Zero : ^T`) are a zero-arg curried sig.
                    let sign =
                        match memberSig with
                        | MemberSig.MethodOrPropSig(ident = ident; sign = s) -> ValueSome(ident, s)
                        | MemberSig.PropSig(ident = ident; sign = s) -> ValueSome(ident, s)

                    match sign with
                    | ValueNone -> ()
                    | ValueSome(ident, CurriedSig(args, retTy)) ->
                        match identOrOpName lexed ident with
                        | ValueNone -> ()
                        | ValueSome mName ->
                            // F# trait sigs are tupled by convention (`^T * ^T -> ^T`), parsing
                            // as one `ArgsSpec` with N args; flatten it into the arg list.
                            let argTys =
                                [
                                    for k in 0 .. args.Length - 1 do
                                        let struct (ArgsSpec(specs, _), _) = args.[k]

                                        for j in 0 .. specs.Length - 1 do
                                            let (ArgSpec(_, _, t)) = specs.[j]
                                            yield t
                                ]

                            acc.Add(RawConstraint.MemberTrait(names, mName, argTys, retTy))
            | Constraint.Default(_, t, _, target) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Default(n, target))
                | ValueNone -> ()
            | Constraint.Coercion(typar = t; typ = target) ->
                match typarName lexed t with
                | ValueSome n -> acc.Add(RawConstraint.Coercion(n, target))
                | ValueNone -> ()
            | Constraint.DefaultConstructor _
            | Constraint.Enum _
            | Constraint.Unmanaged _
            | Constraint.Delegate _ ->
                // No `RawConstraint` shape for these; dropped.
                ()

    /// Bake a nominal reference (`compiled` head + already-translated `args`) to its
    /// kind-correct `FrozenType` template, minting no `SemType`. A body-less (`Opaque`) head
    /// has no kind to bake and raises `BodylessExternalShape` instead of a placeholder.
    let mkNominal (ctx: ExtractCtx) (compiled: string) (args: EqArray<FrozenType>) : FrozenType =
        // This package's own declarations are looked up in the identity index, because a
        // module-held type's `InModule` chain is NOT recoverable by re-cutting its
        // rendering: `N.MModule+T` re-parses as a CLR-nested `InType`, a different identity.
        let key (arity: int) : TypeKey =
            match ExtractCtx.tryTypeKey ctx compiled with
            | ValueSome k -> k
            | ValueNone -> SymbolKeyOps.qualifiedTypeKeyOf compiled arity

        match ExtractCtx.shapeOf ctx compiled with
        | ValueSome(ExternalTypeShape.Union _) -> FTUnion(key args.Length, args)
        | ValueSome(ExternalTypeShape.Class _) -> FTClass(key args.Length, args)
        // A capability interface resolves to a `TyClass` constraint, so its frozen mirror
        // is an `FTClass`.
        | ValueSome(ExternalTypeShape.IntrinsicInterface _) -> FTClass(key args.Length, args)
        | ValueSome(ExternalTypeShape.Record _) -> FTRecord(key args.Length, args)
        | ValueSome(ExternalTypeShape.Enum _) ->
            // Enums are never generic, so no args.
            FTEnum(key 0)
        | ValueSome(ExternalTypeShape.Abbrev(_, frozen)) ->
            // Expand the abbreviation by substituting `args` for its declaring placeholders.
            // One not yet finalized in this pass degrades to `FTUnknown "<deferred>"`.
            FrozenTypeBridge.substituteDeclaring (args.AsSpan().ToArray()) frozen
        // An intrinsic's nominal identity is the canon `FTConst`. Read the canon stored on
        // the matched shape rather than re-deriving it by name — re-minting would hardcode
        // the `Vesper` namespace and diverge for any non-Vesper-homed intrinsic.
        | ValueSome(ExternalTypeShape.Intrinsic ishape) -> FTConst(SymbolKey.Type ishape.Id.Canon, EqArray.empty)
        | ValueSome(ExternalTypeShape.Opaque _) -> raise (BodylessExternalShape compiled)
        | ValueNone ->
            failwithf
                "mkNominal: '%s' resolved as a type name but carries no in-scope shape — every registered type declaration must register a shape"
                compiled

    /// A primitive *alias* dealiases to the underlying primitive its own `.fsi` declares:
    /// `int32`'s `Abbrev` RHS is the frozen `int`, so a parameter written `int32` accepts an
    /// `int` literal.
    let private dealiasPrimitiveAbbrev (ctx: ExtractCtx) (opens: string list) (name: string) : FrozenType voption =
        match resolveTypeName ctx opens name 0 with
        | Ok compiled ->
            match ExtractCtx.shapeOf ctx compiled with
            | ValueSome(ExternalTypeShape.Abbrev _) ->
                // Collapse ONLY when the abbreviation bottoms out at *another* primitive
                // (`int32 = int`). One abbreviating a NON-primitive (`bool = Boolean`, no
                // in-scope shape) keeps the key its own name mints.
                match mkNominal ctx compiled EqArray.empty with
                | FTConst(key, args) when args.IsEmpty && isPrimitiveKey key -> ValueSome(FTConst(key, EqArray.empty))
                | _ -> ValueNone
            | _ -> ValueNone
        | Error _ -> ValueNone

    /// `CST → FrozenType`: every val signature, type-shape body (record field, union-case
    /// field, abbreviation RHS), constraint target and augmentation-member signature runs
    /// through this in the finalize pass. An `Opaque` head propagates out of `mkNominal`.
    let rec translateType
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (typ: Type<SyntaxToken>)
        : Result<FrozenType, string> =
        match typ with
        | Type.ParenType(_, inner, _) -> translateType ctx lexed opens typars constraints inner

        | Type.FunctionType(a, _, b) ->
            match translateType ctx lexed opens typars constraints a with
            | Error e -> Error e
            | Ok fa ->
                match translateType ctx lexed opens typars constraints b with
                | Error e -> Error e
                | Ok fb -> Ok(FTFun(fa, fb))

        | Type.TupleType(parts, _)
        | Type.StructTupleType(_, _, parts, _, _) ->
            let mutable err = None
            let items = ResizeArray<FrozenType>(parts.Length)

            for i in 0 .. parts.Length - 1 do
                if err.IsNone then
                    match translateType ctx lexed opens typars constraints parts.[i] with
                    | Error e -> err <- Some e
                    | Ok b -> items.Add b

            match err with
            | Some e -> Error e
            | None -> Ok(FTTuple(EqArray.ofResizeArray items))

        | Type.VarType(Typar.Named(_, identTok))
        | Type.VarType(Typar.Static(_, identTok)) ->
            let idx = typars.IndexOf(nameOfTok lexed identTok)
            Ok(FTTypar(TyparAxis.Declaring, idx))

        | Type.VarType(Typar.Anon _) ->
            // Synthetic name so distinct anonymous typars don't collide.
            let synthetic = sprintf "_anon%d" typars.Count
            let idx = typars.IndexOf synthetic
            Ok(FTTypar(TyparAxis.Declaring, idx))

        | Type.NamedType li ->
            let name = longIdentName lexed li

            if isPrimitiveName name then
                match dealiasPrimitiveAbbrev ctx opens name with
                | ValueSome ft -> Ok ft
                | ValueNone -> Ok(FTConst(RuntimeNames.primitiveKey name, EqArray.empty))
            else
                match resolveTypeName ctx opens name 0 with
                // A name that resolves to nothing in scope bakes an `FTUnknown` leaf.
                | Error _ -> Ok(FTUnknown name)
                | Ok compiled -> Ok(mkNominal ctx compiled EqArray.empty)

        | Type.GenericType(li, _, args, _, _) ->
            let name = longIdentName lexed li
            let mutable err = None
            let items = ResizeArray<FrozenType>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    match args.[i] with
                    | TypeArg.Type t ->
                        match translateType ctx lexed opens typars constraints t with
                        | Error e -> err <- Some e
                        | Ok b -> items.Add b
                    | TypeArg.Measure _ -> err <- Some "Measure arg not supported"

            match err with
            | Some e -> Error e
            | None ->
                match resolveTypeName ctx opens name items.Count with
                | Error _ -> Ok(FTUnknown name)
                | Ok compiled -> Ok(mkNominal ctx compiled (EqArray.ofResizeArray items))

        | Type.SuffixedType(baseTy, li) ->
            // `'T list` ≡ `List<'T>`.
            let name = longIdentName lexed li

            match translateType ctx lexed opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb ->
                // `'T array` is the rank-1 array intrinsic — the postfix-keyword spelling
                // of `'T[]`. It resolves to no registered type shape, so route it to the
                // same intrinsic the bracket form bakes rather than `FTUnknown "array"`.
                if name = "array" then
                    Ok(FTConst(RuntimeNames.arrayKey 1, EqArray.singleton fb))
                else
                    match resolveTypeName ctx opens name 1 with
                    | Error _ -> Ok(FTUnknown name)
                    | Ok compiled -> Ok(mkNominal ctx compiled (EqArray.singleton fb))

        | Type.ArrayType(baseTy, _, commas, _) ->
            // rank = commas + 1; key by `array<rank>` so unification stays simple.
            let rank = commas.Length + 1

            match translateType ctx lexed opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb -> Ok(FTConst(RuntimeNames.arrayKey rank, EqArray.singleton fb))

        | Type.WhenConstrainedType(inner, clauses) ->
            captureConstraints lexed constraints clauses
            translateType ctx lexed opens typars constraints inner

        | Type.SubtypeConstraint(_, _, inner)
        | Type.AnonymousSubtype(_, inner) -> translateType ctx lexed opens typars constraints inner

        | Type.DottedType(baseTy, _, _) -> translateType ctx lexed opens typars constraints baseTy

        | Type.UnionType(left, _, right) ->
            // A nullable reference type (`type objnull = obj | null`) freezes to the
            // anonymous union `FTOr [T; null]`, `null` being the cross-backend `nullKey`
            // intrinsic. Any other union shape is unrepresentable.
            match right with
            | Type.Null _ ->
                match translateType ctx lexed opens typars constraints left with
                | Error e -> Error e
                | Ok fl -> Ok(FrozenType.MkUnion [ fl; FTConst(RuntimeNames.nullKey, EqArray.empty) ])
            | _ -> Error "Union types (e.g. `obj | null`) not supported"
        | Type.Null _ -> Ok(FTConst(RuntimeNames.nullKey, EqArray.empty))
        | Type.ILIntrinsic _ -> Error "Inline IL not supported"
        | Type.MeasureType _ -> Error "Measure types not supported"
        | Type.AnonRecordType _ -> Error "Anonymous record types not supported"
        | Type.Missing -> Error "Missing type"
        | Type.SkipsTokens _ -> Error "Recovery-skipped type"

    /// An `ArgsSpec` folds to its `.NET`-tupled `FrozenType` (0 args → `unit`,
    /// 1 → the arg, N → `FTTuple`).
    let translateArgsSpec
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (argsSpec: ArgsSpec<SyntaxToken>)
        : Result<FrozenType, string> =
        let (ArgsSpec(args, _)) = argsSpec

        if args.Length = 0 then
            Ok(FTConst(RuntimeNames.unitKey, EqArray.empty))
        elif args.Length = 1 then
            let (ArgSpec(_, _, t)) = args.[0]
            translateType ctx lexed opens typars constraints t
        else
            let mutable err = None
            let items = ResizeArray<FrozenType>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (ArgSpec(_, _, t)) = args.[i]

                    match translateType ctx lexed opens typars constraints t with
                    | Error e -> err <- Some e
                    | Ok b -> items.Add b

            match err with
            | Some e -> Error e
            | None -> Ok(FTTuple(EqArray.ofResizeArray items))

    /// The curried signature folds right-associatively into nested `FTFun` nodes. The
    /// finalize pass splits the head `FTFun(params, ret)` into a member's two-axis
    /// `ExternalSignature`, or takes the whole chain as a val's template.
    let translateCurriedSig
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (sigCurried: CurriedSig<SyntaxToken>)
        : Result<FrozenType, string> =
        let (CurriedSig(args, retTy)) = sigCurried

        // Intern typars ARGS-first (left-to-right) with the RETURN type LAST, so Declaring
        // indices land in the canonical ABI order: a return-only typar (`Set.map`'s `'U` in
        // `-> Set<'U>`) must fall behind every argument typar.
        let mutable err = None
        let argFs = ResizeArray<FrozenType>(args.Length)

        for i in 0 .. args.Length - 1 do
            if err.IsNone then
                let (struct (argsSpec, _)) = args.[i]

                match translateArgsSpec ctx lexed opens typars constraints argsSpec with
                | Error e -> err <- Some e
                | Ok b -> argFs.Add b

        match err with
        | Some e -> Error e
        | None ->
            match translateType ctx lexed opens typars constraints retTy with
            | Error e -> Error e
            | Ok retF ->
                let mutable acc = retF

                for k in argFs.Count - 1 .. -1 .. 0 do
                    acc <- FTFun(argFs.[k], acc)

                Ok acc

    let registerExplicitTypars (lexed: Lexed) (typars: TyparCollector) (defns: TyparDefns<SyntaxToken> voption) : unit =
        match defns with
        | ValueNone -> ()
        | ValueSome(TyparDefns(_, items, _, _)) ->
            for i in 0 .. items.Length - 1 do
                let (TyparDefn(_, typar)) = items.[i]

                match typar with
                | Typar.Named(_, identTok)
                | Typar.Static(_, identTok) -> typars.IndexOf(nameOfTok lexed identTok) |> ignore
                | Typar.Anon _ -> ()
