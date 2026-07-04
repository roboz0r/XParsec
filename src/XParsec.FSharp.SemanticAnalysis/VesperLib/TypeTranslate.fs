namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open VesperLibTyparCapture

/// CST → `FrozenType` translation, with the small token/identifier/attribute
/// helpers that the rest of the extractor reuses. `translateType` (and its
/// `translateArgsSpec` / `translateCurriedSig` wrappers) is the single
/// `CST → FrozenType` translation the contract-extraction finalize pass runs once
/// the registry is complete; the `FrozenTypeBridge` realisers turn the resulting
/// templates back into `SemType`s at use time (val instantiation, codegen). Calls
/// thread an `ExtractCtx` (for type-name resolution against `ctx.QualifiedTypes` /
/// `ctx.Types`) plus a `TyparCollector` (for typar interning) and a
/// `ConstraintCollector` (for inline `when …` clauses).
module VesperLibTypeTranslate =

    let nameOfTok (lexed: Lexed) (input: string) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
        | TokenIndex.Virtual -> ""

    let longIdentName (lexed: Lexed) (input: string) (li: LongIdent<SyntaxToken>) : string =
        let parts =
            [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]

        String.concat "." parts

    let longIdentShortName (lexed: Lexed) (input: string) (li: LongIdent<SyntaxToken>) : string =
        if li.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed input li.Idents.[li.Idents.Length - 1]

    let identOrOpName (lexed: Lexed) (input: string) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
        // `(::)` is a binding head only the contract surface needs to name (cons
        // has no `op_` member in expression position — see `OperatorNames.ofToken`),
        // so it is mapped here before delegating to the shared resolver.
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) when opTok.Token = Token.KWColonColon ->
            ValueSome "op_ColonColon"
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) ->
            OperatorNames.ofParenSymbolic (nameOfTok lexed input opTok) opTok
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome "op_Range"
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome "op_RangeStep"
        | IdentOrOp.StarOp _ -> ValueSome "op_Multiply"
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "op_Nil"
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) ->
            // Active-pattern compiled names are non-trivial — defer.
            ValueNone

    /// First attribute whose short name (last segment) matches a candidate,
    /// ignoring the optional `Attribute` suffix.
    let findAttribute
        (lexed: Lexed)
        (input: string)
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
                                | Type.NamedType li -> longIdentShortName lexed input li
                                | Type.GenericType(li, _, _, _, _) -> longIdentShortName lexed input li
                                | _ -> ""

                            if matchName attrName then
                                found <- ValueSome construction

        found

    let constructionExpr (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> voption =
        match oc with
        | ObjectConstruction(_, e) -> ValueSome e
        | InterfaceConstruction _ -> ValueNone

    /// Text of a parsed string-literal expression. Ignores expression holes
    /// and other interpolation artefacts (compiled-name args are non-
    /// interpolated strings in practice).
    let stringExprText
        (lexed: Lexed)
        (input: string)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for i in 0 .. parts.Length - 1 do
            match parts.[i] with
            | StringPart.Text tok -> sb.Append(nameOfTok lexed input tok) |> ignore
            | StringPart.EscapeSequence tok ->
                let raw = nameOfTok lexed input tok
                // Preserve source-level text; full escape decoding is the
                // lexer's job and unneeded for attribute args.
                sb.Append raw |> ignore
            | _ -> ()

        sb.ToString()

    let tryCompiledName (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : string voption =
        match findAttribute lexed input attrs [ "CompiledName" ] with
        | ValueNone -> ValueNone
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> ValueNone
            | ValueSome argExpr ->
                // Argument is `("Foo")`; accept either `Expr.String` (typical)
                // or an older `Expr.Const(Constant.Literal _)` fallback.
                let rec stripParens (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> stripParens inner
                    | _ -> e

                match stripParens argExpr with
                | Expr.String(_, parts, _) ->
                    let s = stringExprText lexed input parts
                    if s.Length > 0 then ValueSome s else ValueNone
                | Expr.Const(Constant.Literal tok) ->
                    let raw = nameOfTok lexed input tok
                    let trimmed = raw.Trim([| '"' |])
                    if trimmed.Length > 0 then ValueSome trimmed else ValueNone
                | _ -> ValueNone

    /// True iff the module-level attributes carry `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`.
    let hasModuleSuffix (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : bool =
        match findAttribute lexed input attrs [ "CompilationRepresentation" ] with
        | ValueNone -> false
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> false
            | ValueSome argExpr ->
                // Heuristic: look for the token text "ModuleSuffix" anywhere
                // in the expression. Good enough for the v1 walker.
                let rec containsModuleSuffix (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> containsModuleSuffix inner
                    | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed input li
                        name = "ModuleSuffix"
                    | Expr.DotLookup(_, _, LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed input li
                        name = "ModuleSuffix"
                    | _ -> false

                containsModuleSuffix argExpr

    /// True iff the module-level attributes carry `[<AutoOpen>]` — the module's
    /// members are in scope unqualified for a consumer of the package. Recorded
    /// as an ambient open prefix.
    let isAutoOpen (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : bool =
        findAttribute lexed input attrs [ "AutoOpen" ] |> ValueOption.isSome

    /// True iff the type-level attributes carry `[<RequireQualifiedAccess>]` — the
    /// union's cases (and a module's members) are NOT in scope unqualified, so a
    /// bare `Red` for `[<RequireQualifiedAccess>] type Color = Red | …` must NOT
    /// resolve (F# forbids the short form). Drives the resolution-side suppression
    /// of bare RQA case names.
    let isRequireQualifiedAccess (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : bool =
        findAttribute lexed input attrs [ "RequireQualifiedAccess" ]
        |> ValueOption.isSome

    /// Resolve a long-identifier type name against `ctx.Types`, using the
    /// per-file open prefixes (newest first) as candidate qualifiers when
    /// the short-name lookup misses. Returns the canonical compiled name
    /// on success; `Error` with a brief reason on failure so the caller
    /// can attach a per-file diagnostic and skip the val.
    ///
    /// Resolution order:
    ///   1. Direct hit on the qualified name as written (own package, then a
    ///      dependency's fully-qualified name via `ctx.AmbientShapes`).
    ///   2. Short-name lookup in `ctx.Types`. Arity mismatch still resolves
    ///      (cross-file disagreements shouldn't block extraction) but uses
    ///      the recorded compiled name.
    ///   3. For each open prefix in newest-first order, try
    ///      `prefix + "." + name` against the qualified-name set, then the
    ///      dependency shapes (`ctx.AmbientShapes`).
    ///
    /// Dependency packages contribute their type shapes through `ctx.AmbientShapes`,
    /// keyed by qualified compiled name —
    /// not the per-package `ctx.Types` / `ctx.QualifiedTypes` index, which holds
    /// only this package's own declarations. A cross-package reference is written
    /// either fully-qualified or as a short name resolved through an open prefix
    /// (the package's own namespace is one such prefix), so each candidate is tried
    /// against the local qualified set first, then the ambient shapes. Own-package
    /// names always win: a same-short-name local type shadows a dependency's.
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

        // The own-package qualified set and the metadata / dependency provider both
        // key a *generic* type by its arity-suffixed compiled name (`List`1`,
        // `Vesper.Choice`2`); source writes the bare head (`List`). Probe the
        // suffixed form first (so an arity-overloaded type is unambiguous), then the
        // bare name (arity-0 types). The matched form is the canonical compiled name
        // `mkNominal`'s `shapeOf` re-resolves through.
        let forms (n: string) : string list =
            if arity > 0 then
                [ SymbolKeyOps.arityName n arity; n ]
            else
                [ n ]

        let inQualified (n: string) : string option =
            forms n |> List.tryFind ctx.QualifiedTypes.Contains

        let inAmbient (n: string) : string option =
            forms n |> List.tryFind (fun k -> (ctx.AmbientShapes k).IsSome)

        match inQualified name with
        | Some k -> Ok k
        | None ->
            // A fully-qualified reference to a dependency's or the BCL's type
            // (`Vesper.Option.Option`, `System.Collections.Generic.List`) — resolved
            // through the ambient shapes (dependency providers + the layer-2
            // metadata provider). Restricted to dotted names: a bare name is a
            // *short* name that the own-package index (below) must get first, so a
            // local type isn't hijacked by a same-named dependency type at the root.
            match (if isQualified then inAmbient name else None) with
            | Some k -> Ok k
            | None ->
                // Short-name index hit — only for a *bare* name. A *qualified* name
                // (`System.Collections.Generic.List`) must NOT collapse onto a local
                // type sharing the last segment (`List`, the cons-list union): the
                // written qualifier names a different type the index can't speak for.
                // It falls through to the open-prefix / ambient resolution below, and
                // failing that to `Error` (a `TyUnknown` leaf) — never the local short.
                let shortHit =
                    match ctx.Types.TryGetValue short with
                    | true, v when not isQualified -> ValueSome v
                    | _ -> ValueNone

                match shortHit with
                // An EXACT (short-name, arity) hit wins immediately. The recorded arity
                // matters once a short name is arity-overloaded (`Fun`2`/`Fun`3`): a
                // disagreeing hit must NOT be taken blindly — fall through to the
                // arity-aware open-prefix resolution (`forms` probes ``name`arity`` first),
                // and accept the disagreeing short hit only as a last resort (still
                // beating a placeholder for a genuine cross-file arity mismatch).
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

    let isPrimitiveName (s: string) =
        // Numeric core shared via `RuntimeNames.numericTypeNames`; the non-numeric
        // primitives (scalars + the built-in reference types) are unioned in here.
        RuntimeNames.numericTypeNames.Contains s
        || match s with
           | "char"
           | "string"
           | "bool"
           | "unit"
           | "obj"
           | "objnull"
           | "voidptr"
           | "exn" -> true
           | _ -> false

    /// Source-text name of a typar (the part after `'` or `^`), or
    /// `ValueNone` for anonymous typars (whose constraint participation
    /// can't be addressed by name later).
    let typarName (lexed: Lexed) (input: string) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(_, identTok)
        | Typar.Static(_, identTok) -> ValueSome(nameOfTok lexed input identTok)
        | Typar.Anon _ -> ValueNone

    /// Walk a `when …` clause and emit `RawConstraint` entries into the
    /// collector. Trait-style constraints (Equality/Comparison/etc.) flow
    /// through directly; SRTP member traits and defaults are captured as
    /// opaque markers for Phase 5b. Unsupported constraint shapes
    /// (`Coercion`, `Enum`, …) silently drop in v1.
    let captureConstraints
        (lexed: Lexed)
        (input: string)
        (acc: ConstraintCollector)
        (clauses: TyparConstraints<SyntaxToken>)
        : unit =
        let (TyparConstraints(_, items, _)) = clauses

        for i in 0 .. items.Length - 1 do
            match items.[i] with
            | Constraint.Equality(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Equality))
                | ValueNone -> ()
            | Constraint.Comparison(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Comparison))
                | ValueNone -> ()
            | Constraint.Struct(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Struct))
                | ValueNone -> ()
            | Constraint.ReferenceType(t, _, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.ReferenceType))
                | ValueNone -> ()
            | Constraint.Nullness(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Nullness))
                | ValueNone -> ()
            | Constraint.NotNull(t, _, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.NotNull))
                | ValueNone -> ()
            | Constraint.MemberTrait(staticTypars, _, _, _, _, memberSig, _) ->
                let names =
                    match staticTypars with
                    | StaticTypars.Single t ->
                        match typarName lexed input t with
                        | ValueSome n -> [ n ]
                        | ValueNone -> []
                    | StaticTypars.OrList(_, items, _, _) ->
                        [
                            for k in 0 .. items.Length - 1 do
                                match typarName lexed input items.[k] with
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
                        match identOrOpName lexed input ident with
                        | ValueNone -> ()
                        | ValueSome mName ->
                            // F# trait sigs are tupled by convention
                            // (`^T * ^T -> ^T`), parsing as one ArgsSpec with
                            // N args; flatten it into the trait's arg list.
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
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Default(n, target))
                | ValueNone -> ()
            | Constraint.Coercion(typar = t; typ = target) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Coercion(n, target))
                | ValueNone -> ()
            | Constraint.DefaultConstructor _
            | Constraint.Enum _
            | Constraint.Unmanaged _
            | Constraint.Delegate _ ->
                // v1 silently drops; Phase 5b extends as needed.
                ()

    /// Bake a nominal reference (`compiled` head + already-translated `args`) to
    /// its kind-correct `FrozenType` template, consulting the in-scope type shapes
    /// (`ExtractCtx.shapeOf`: this package's own shapes, then its dependencies' via
    /// `ctx.AmbientShapes`), minting no `SemType`. A transparent abbreviation
    /// expands to its body (`substituteDeclaring` — a total `FrozenType →
    /// FrozenType` walk); a referenced-package intrinsic collapses to its short
    /// `FTConst` (so an external `int` / `exn` matches the literal-typed form).
    ///
    /// Called by the contract-extraction finalize pass, so `ctx.TypeShapes` is
    /// fully populated and an intra-package forward reference — legal only inside a
    /// `type … and …` group or a `rec` scope, whose shapes register together
    /// before any body is kinded — resolves.
    ///
    /// Two arms are loud invariant assertions, not fall-throughs:
    /// - `Opaque` is a body-less residue (`enum` / `delegate` / type-extension, or
    ///   a body the extractor couldn't model). Referencing one in a signature has
    ///   no kind to bake; rather than mint a placeholder that flows to codegen,
    ///   raise `BodylessExternalShape` — the finalize pass catches it and degrades
    ///   the naming template to `FTUnknown`.
    /// - `ValueNone` means *name resolved but no shape registered* — impossible,
    ///   since every `registerTypeDecl` registers a shape (incl. the `Opaque`
    ///   deferrals) and a dependency name resolves *through* its ambient shape. A
    ///   genuine *unresolved name* never reaches here — `resolveTypeName` fails
    ///   first and that arm bakes the `FTUnknown` leaf.
    let mkNominal (ctx: ExtractCtx) (compiled: string) (args: EqArray<FrozenType>) : FrozenType =
        let homeOf (originAsm: string option) =
            match originAsm with
            | Some _ -> originAsm
            | None -> ctx.HomeAssembly

        match ExtractCtx.shapeOf ctx compiled with
        | ValueSome(ExternalTypeShape.Union(_, _, _, origin)) ->
            FTUnion(SymbolKeyOps.qualifiedTypeKeyOf (homeOf origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Class info) ->
            FTClass(SymbolKeyOps.qualifiedTypeKeyOf (homeOf info.Origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Record(_, _, origin)) ->
            FTRecord(SymbolKeyOps.qualifiedTypeKeyOf (homeOf origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Enum(_, origin)) ->
            // The enum nominal — no args (enums are never generic). The `.fsi`
            // contract extractor never produces an `Enum` shape (it is a TS-manifest
            // arm), so this is unreached today, but the mirror keeps the match total
            // and faithful should a contract enum ever flow through here.
            FTEnum(SymbolKeyOps.qualifiedTypeKeyOf (homeOf origin.Assembly) compiled 0)
        | ValueSome(ExternalTypeShape.Abbrev(_, frozen)) ->
            // Expand the abbreviation by substituting `args` for its declaring
            // placeholders. A still-`deferredTemplate` abbrev (one not yet
            // finalized in this pass) degrades to `FTUnknown "<deferred>"`.
            FrozenTypeBridge.substituteDeclaring (args.AsSpan().ToArray()) frozen
        | ValueSome(ExternalTypeShape.Intrinsic _) -> FTConst(SymbolKeyOps.shortName compiled, EqArray.empty)
        | ValueSome(ExternalTypeShape.Opaque _) -> raise (BodylessExternalShape compiled)
        | ValueNone ->
            failwithf
                "mkNominal: '%s' resolved as a type name but carries no in-scope shape — every registered type declaration must register a shape"
                compiled

    /// A primitive *alias* (`type int32 = int`, `type uint = uint32`, `type int8 =
    /// sbyte`) dealiases to the underlying primitive its own `.fsi` declares — the
    /// canonical intrinsic the front end (`Translate.fs`'s abbreviation expansion)
    /// and the codegen IL encoder key on. Resolved from the abbreviation
    /// *definition* registered in `ctx.TypeShapes` (via `mkNominal`'s existing
    /// `Abbrev` arm), never a hardcoded direction: `int32`'s `Abbrev` RHS is the
    /// frozen `int`, so this returns `FTConst "int"`. A true intrinsic (`int`,
    /// `sbyte` — an `Intrinsic` shape) or a primitive whose package registers no
    /// shape returns `ValueNone`, leaving the bare `FTConst name` the caller bakes.
    /// Without this an alias param (`shift: int32` on `(<<<)`) froze as a nominal
    /// `FTConst "int32"` that never unified with an `int` literal at the use site.
    let private dealiasPrimitiveAbbrev (ctx: ExtractCtx) (opens: string list) (name: string) : FrozenType voption =
        match resolveTypeName ctx opens name 0 with
        | Ok compiled ->
            match ExtractCtx.shapeOf ctx compiled with
            | ValueSome(ExternalTypeShape.Abbrev _) ->
                // Collapse to the dealiased primitive ONLY when the abbreviation
                // bottoms out at *another* primitive (`int32 = int`, `uint =
                // uint32`). A primitive that is itself declared as an abbreviation
                // of a *non-primitive* (`bool = Boolean`, the BCL `System.Boolean`,
                // which resolves to no in-scope shape during extraction) keeps its
                // canonical `FTConst name` — the form the front end and codegen key
                // on — rather than dealiasing to an unresolved `Boolean`.
                match mkNominal ctx compiled EqArray.empty with
                | FTConst(p, args) when args.IsEmpty && isPrimitiveName p -> ValueSome(FTConst(p, EqArray.empty))
                | _ -> ValueNone
            | _ -> ValueNone
        | Error _ -> ValueNone

    /// `CST → FrozenType` translation: every val
    /// signature, type-shape body (record field, union-case field, abbreviation
    /// RHS), constraint target, and augmentation-member signature is translated
    /// through this in the `ExtractCtx.toProvider` finalize pass, once the registry
    /// is complete. A typar leaf bakes a self-describing `FTTypar(Declaring,i)`
    /// placeholder; a nominal head kinds through `mkNominal`. An `Opaque` head
    /// raises `BodylessExternalShape`; the finalize pass catches it and degrades to
    /// `FTUnknown`.
    let rec translateType
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (typ: Type<SyntaxToken>)
        : Result<FrozenType, string> =
        match typ with
        | Type.ParenType(_, inner, _) -> translateType ctx lexed input opens typars constraints inner

        | Type.FunctionType(a, _, b) ->
            match translateType ctx lexed input opens typars constraints a with
            | Error e -> Error e
            | Ok fa ->
                match translateType ctx lexed input opens typars constraints b with
                | Error e -> Error e
                | Ok fb -> Ok(FTFun(fa, fb))

        | Type.TupleType(parts, _)
        | Type.StructTupleType(_, _, parts, _, _) ->
            let mutable err = None
            let items = ResizeArray<FrozenType>(parts.Length)

            for i in 0 .. parts.Length - 1 do
                if err.IsNone then
                    match translateType ctx lexed input opens typars constraints parts.[i] with
                    | Error e -> err <- Some e
                    | Ok b -> items.Add b

            match err with
            | Some e -> Error e
            | None -> Ok(FTTuple(EqArray.ofResizeArray items))

        | Type.VarType(Typar.Named(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Regular)
            Ok(FTTypar(TyparAxis.Declaring, idx))

        | Type.VarType(Typar.Static(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Static)
            Ok(FTTypar(TyparAxis.Declaring, idx))

        | Type.VarType(Typar.Anon _) ->
            // Synthetic name so distinct anonymous typars don't collide.
            let synthetic = sprintf "_anon%d" typars.Count
            let idx = typars.IndexOf(synthetic, TyparKind.Regular)
            Ok(FTTypar(TyparAxis.Declaring, idx))

        | Type.NamedType li ->
            let name = longIdentName lexed input li

            if isPrimitiveName name then
                match dealiasPrimitiveAbbrev ctx opens name with
                | ValueSome ft -> Ok ft
                | ValueNone -> Ok(FTConst(name, EqArray.empty))
            else
                match resolveTypeName ctx opens name 0 with
                // A name that resolves to nothing in scope bakes a `FTUnknown`
                // leaf — the frozen counterpart of `translateType`'s `TyUnknown`.
                | Error _ -> Ok(FTUnknown name)
                | Ok compiled -> Ok(mkNominal ctx compiled EqArray.empty)

        | Type.GenericType(li, _, args, _, _) ->
            let name = longIdentName lexed input li
            let mutable err = None
            let items = ResizeArray<FrozenType>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    match args.[i] with
                    | TypeArg.Type t ->
                        match translateType ctx lexed input opens typars constraints t with
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
            let name = longIdentName lexed input li

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb ->
                // `'T array` is the rank-1 array intrinsic — the postfix-keyword
                // spelling of `'T[]` (`Type.ArrayType`). It resolves to no
                // registered type shape, so route it to the same `arrayName`
                // intrinsic the bracket form bakes rather than `FTUnknown "array"`.
                if name = "array" then
                    Ok(FTConst(RuntimeNames.arrayName 1, EqArray.singleton fb))
                else
                    match resolveTypeName ctx opens name 1 with
                    | Error _ -> Ok(FTUnknown name)
                    | Ok compiled -> Ok(mkNominal ctx compiled (EqArray.singleton fb))

        | Type.ArrayType(baseTy, _, commas, _) ->
            // rank = commas + 1; key by `array<rank>` so unification stays simple.
            let rank = commas.Length + 1

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb -> Ok(FTConst(RuntimeNames.arrayName rank, EqArray.singleton fb))

        | Type.WhenConstrainedType(inner, clauses) ->
            captureConstraints lexed input constraints clauses
            translateType ctx lexed input opens typars constraints inner

        | Type.SubtypeConstraint(_, _, inner)
        | Type.AnonymousSubtype(_, inner) -> translateType ctx lexed input opens typars constraints inner

        | Type.DottedType(baseTy, _, _) -> translateType ctx lexed input opens typars constraints baseTy

        | Type.UnionType(left, _, right) ->
            // A nullable reference type `T | null` (F# nullable refs, e.g.
            // `type objnull = obj | null`). Vesper SemTypes carry no nullability
            // axis, so the nullable form collapses to its non-null part `T`.
            // Any other union shape (not `… | null`) is genuinely unrepresentable.
            match right with
            | Type.Null _ -> translateType ctx lexed input opens typars constraints left
            | _ -> Error "Union types (e.g. `obj | null`) not supported"
        | Type.Null _ -> Error "Null types not supported"
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
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (argsSpec: ArgsSpec<SyntaxToken>)
        : Result<FrozenType, string> =
        let (ArgsSpec(args, _)) = argsSpec

        if args.Length = 0 then
            Ok(FTConst("unit", EqArray.empty))
        elif args.Length = 1 then
            let (ArgSpec(_, _, t)) = args.[0]
            translateType ctx lexed input opens typars constraints t
        else
            let mutable err = None
            let items = ResizeArray<FrozenType>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (ArgSpec(_, _, t)) = args.[i]

                    match translateType ctx lexed input opens typars constraints t with
                    | Error e -> err <- Some e
                    | Ok b -> items.Add b

            match err with
            | Some e -> Error e
            | None -> Ok(FTTuple(EqArray.ofResizeArray items))

    /// The curried signature folds right-associatively into nested `FTFun` nodes.
    /// The finalize pass splits the head `FTFun(params, ret)` into the member's
    /// two-axis `ExternalSignature` (or treats the whole result as the value for a
    /// property), and uses the whole `FTFun` chain as a val's template.
    let translateCurriedSig
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (sigCurried: CurriedSig<SyntaxToken>)
        : Result<FrozenType, string> =
        let (CurriedSig(args, retTy)) = sigCurried

        // Intern typars ARGS-first (left-to-right) and the RETURN type LAST, so the
        // collector assigns Declaring indices in the SAME first-left-to-right-
        // appearance order the producer's `Elaborate.mkMethodQuantEnv` ▸
        // `GeneralizedTypars.canonical` uses (`TyFun` domain before range). A
        // free-function's `Scheme` index is then the canonical ABI typar order, which
        // `Inline.openMethodSignature` maps positionally onto the method axis. The
        // earlier return-first walk interned a return-only typar (`Set.map`'s `'U` in
        // `-> Set<'U>`) ahead of an argument typar, permuting the order and breaking
        // that match. Explicit `<'T>` typars are seeded ahead of this walk regardless.
        let mutable err = None
        let argFs = ResizeArray<FrozenType>(args.Length)

        for i in 0 .. args.Length - 1 do
            if err.IsNone then
                let (struct (argsSpec, _)) = args.[i]

                match translateArgsSpec ctx lexed input opens typars constraints argsSpec with
                | Error e -> err <- Some e
                | Ok b -> argFs.Add b

        match err with
        | Some e -> Error e
        | None ->
            match translateType ctx lexed input opens typars constraints retTy with
            | Error e -> Error e
            | Ok retF ->
                let mutable acc = retF

                for k in argFs.Count - 1 .. -1 .. 0 do
                    acc <- FTFun(argFs.[k], acc)

                Ok acc

    let registerExplicitTypars
        (lexed: Lexed)
        (input: string)
        (typars: TyparCollector)
        (defns: TyparDefns<SyntaxToken> voption)
        : unit =
        match defns with
        | ValueNone -> ()
        | ValueSome(TyparDefns(_, items, _, _)) ->
            for i in 0 .. items.Length - 1 do
                let (TyparDefn(_, typar)) = items.[i]

                match typar with
                | Typar.Named(_, identTok) ->
                    let name = nameOfTok lexed input identTok
                    typars.IndexOf(name, TyparKind.Regular) |> ignore
                | Typar.Static(_, identTok) ->
                    let name = nameOfTok lexed input identTok
                    typars.IndexOf(name, TyparKind.Static) |> ignore
                | Typar.Anon _ -> ()
