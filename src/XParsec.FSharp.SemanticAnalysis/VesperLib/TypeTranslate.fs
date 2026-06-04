namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open VesperLibTyparCapture

/// CST → `SemBuilder` translation, with the small token/identifier/attribute
/// helpers that the rest of the extractor reuses. Calls thread an
/// `ExtractCtx` (for type-name resolution against `ctx.QualifiedTypes` /
/// `ctx.Types`) plus a `TyparCollector` (for typar interning) and a
/// `ConstraintCollector` (for inline `when …` clauses on the type).
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

    /// Map an operator token (`+`, `<|`, etc.) to its compiled name
    /// (`op_Addition`, `op_PipeLeft`, etc.). The Token-enum match covers the
    /// well-known operators with dedicated enum values; the text fallback
    /// covers generic operators the lexer collapses to `OpGeneric`.
    let opTokenToCompiled (tok: SyntaxToken) (text: string) : string voption =
        match tok.Token with
        | Token.OpAddition -> ValueSome "op_Addition"
        | Token.OpSubtraction -> ValueSome "op_Subtraction"
        | Token.OpMultiply -> ValueSome "op_Multiply"
        | Token.OpDivision -> ValueSome "op_Division"
        | Token.OpModulus -> ValueSome "op_Modulus"
        | Token.OpLessThan -> ValueSome "op_LessThan"
        | Token.OpGreaterThan -> ValueSome "op_GreaterThan"
        | Token.OpLessThanOrEqual -> ValueSome "op_LessThanOrEqual"
        | Token.OpGreaterThanOrEqual -> ValueSome "op_GreaterThanOrEqual"
        | Token.OpEquality -> ValueSome "op_Equality"
        | Token.OpInequality -> ValueSome "op_Inequality"
        | Token.OpAmpAmp -> ValueSome "op_BooleanAnd"
        | Token.OpBarBar -> ValueSome "op_BooleanOr"
        | Token.OpPipeRight -> ValueSome "op_PipeRight"
        | Token.OpPipeLeft -> ValueSome "op_PipeLeft"
        | Token.OpComposeRight -> ValueSome "op_ComposeRight"
        | Token.OpComposeLeft -> ValueSome "op_ComposeLeft"
        | Token.KWColonColon -> ValueSome "op_ColonColon"
        | _ ->
            // Only operators that appear inside `.fsi` val sigs need to land
            // here; the type checker picks them up by compiled name.
            match text with
            | "|>" -> ValueSome "op_PipeRight"
            | "<|" -> ValueSome "op_PipeLeft"
            | ">>" -> ValueSome "op_ComposeRight"
            | "<<" -> ValueSome "op_ComposeLeft"
            | "||>" -> ValueSome "op_PipeRight2"
            | "<||" -> ValueSome "op_PipeLeft2"
            | "|||>" -> ValueSome "op_PipeRight3"
            | "<|||" -> ValueSome "op_PipeLeft3"
            | "@" -> ValueSome "op_Append"
            | "^" -> ValueSome "op_Concatenate"
            | "?" -> ValueSome "op_Dynamic"
            | "?<-" -> ValueSome "op_DynamicAssignment"
            | ".." -> ValueSome "op_Range"
            | ".. .." -> ValueSome "op_RangeStep"
            | _ -> ValueNone

    let identOrOpName (lexed: Lexed) (input: string) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) -> opTokenToCompiled opTok (nameOfTok lexed input opTok)
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
                match ctx.Types.TryGetValue short with
                // Short-name index hit — only for a *bare* name. A *qualified* name
                // (`System.Collections.Generic.List`) must NOT collapse onto a local
                // type sharing the last segment (`List`, the cons-list union): the
                // written qualifier names a different type the index can't speak for.
                // It falls through to the open-prefix / ambient resolution below, and
                // failing that to `Error` (a `TyUnknown` leaf) — never the local short.
                | true, (_, compiled) when not isQualified ->
                    // Arity disagreement is tolerated; the recorded compiled name still
                    // beats a placeholder.
                    Ok compiled
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
                    | ValueNone -> Error(sprintf "Unresolved type name '%s'" name)

    let isPrimitiveName (s: string) =
        match s with
        | "int"
        | "int8"
        | "int16"
        | "int32"
        | "int64"
        | "uint"
        | "uint8"
        | "uint16"
        | "uint32"
        | "uint64"
        | "byte"
        | "sbyte"
        | "nativeint"
        | "unativeint"
        | "float"
        | "float32"
        | "double"
        | "single"
        | "decimal"
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
    /// its kind-correct `SemType`, consulting the in-scope type shapes
    /// (`ExtractCtx.shapeOf`: this package's own shapes, then its dependencies'
    /// via `ctx.AmbientShapes`)
    /// A transparent abbreviation expands to its body, and a referenced-package
    /// intrinsic collapses to its short `TyConst` (so an external `int` / `exn`
    /// matches the literal-typed form).
    ///
    /// Called *inside the deferred builder* (at `Instantiate` time), so
    /// `ctx.TypeShapes` is fully populated and an intra-package forward reference
    /// — legal only inside a `type … and …` group or a `rec` scope, whose shapes
    /// register together before any body is kinded — resolves.
    ///
    /// Two arms are loud invariant assertions, not fall-throughs:
    /// - `Opaque` is a body-less residue (`enum` / `delegate` / type-extension,
    ///   or a body the extractor couldn't model). Referencing one in a signature
    ///   has no kind to bake; rather than mint a `TyRecord` placeholder that flows
    ///   to codegen, refuse it here. Modelling such a kind turns
    ///   it into a real shape and an arm above; until then no shipping contract
    ///   names one.
    /// - `ValueNone` means *name resolved but no shape registered* — impossible,
    ///   since every `registerTypeDecl` registers a shape (incl. the `Opaque`
    ///   deferrals) and a dependency name resolves *through* its ambient shape. A
    ///   genuine *unresolved name* never reaches here — `resolveTypeName` fails
    ///   first and that arm bakes the `TyUnknown` leaf.
    let mkNominal (ctx: ExtractCtx) (compiled: string) (args: EqArray<SemType>) : SemType =
        // Mint the nominal head's key with the type's **home assembly** so a type
        // baked here carries the SAME home a consumer's `Translate` mints for it —
        // the unification-equality invariant. The
        // `ns`/`name` come from splitting the fully-qualified `compiled` name (the
        // shape's `origin.Namespace` is Empty for this package's own types during
        // extraction); the home assembly is the shape's `origin.Assembly` when
        // filled (a dependency, via `AmbientShapes`) else this package's
        // `HomeAssembly` (its own types, whose extraction-time origin is Empty).
        let homeOf (originAsm: string option) =
            match originAsm with
            | Some _ -> originAsm
            | None -> ctx.HomeAssembly

        match ExtractCtx.shapeOf ctx compiled with
        | ValueSome(ExternalTypeShape.Union(_, _, origin)) ->
            TyUnion(SymbolKeyOps.qualifiedTypeKeyOf (homeOf origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Class info) ->
            TyClass(SymbolKeyOps.qualifiedTypeKeyOf (homeOf info.Origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Record(_, _, origin)) ->
            TyRecord(SymbolKeyOps.qualifiedTypeKeyOf (homeOf origin.Assembly) compiled args.Length, args)
        | ValueSome(ExternalTypeShape.Abbrev(_, _, frozen)) ->
            // Expand the abbreviation to its body. `frozen` is the *defining*
            // package's `translateType` builder frozen to a template, whose nominal
            // heads already kind through `mkNominal` against that package's scope
            // (its own shapes plus its dependencies) — so the expanded body is
            // already kind-correct and needs no further reconciliation.
            FrozenTypeBridge.instantiateDeclaring frozen (args.AsSpan().ToArray())
        | ValueSome(ExternalTypeShape.Intrinsic _) -> TyConst(SymbolKeyOps.shortName compiled, EqArray.empty)
        | ValueSome(ExternalTypeShape.Opaque _) ->
            failwithf
                "mkNominal: '%s' is an Opaque (body-less) shape — an enum / delegate / type-extension or an unmodelled body. Model its kind before a contract names it"
                compiled
        | ValueNone ->
            failwithf
                "mkNominal: '%s' resolved as a type name but carries no in-scope shape — every registered type declaration must register a shape"
                compiled

    let rec translateType
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (typ: Type<SyntaxToken>)
        : Result<SemBuilder, string> =
        match typ with
        | Type.ParenType(_, inner, _) -> translateType ctx lexed input opens typars constraints inner

        | Type.FunctionType(a, _, b) ->
            match translateType ctx lexed input opens typars constraints a with
            | Error e -> Error e
            | Ok fa ->
                match translateType ctx lexed input opens typars constraints b with
                | Error e -> Error e
                | Ok fb -> Ok(fun ts -> TyFun(fa ts, fb ts))

        | Type.TupleType(parts, _)
        | Type.StructTupleType(_, _, parts, _, _) ->
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(parts.Length)

            for i in 0 .. parts.Length - 1 do
                if err.IsNone then
                    match translateType ctx lexed input opens typars constraints parts.[i] with
                    | Error e -> err <- Some e
                    | Ok b -> builders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()
                Ok(fun ts -> TyTuple(EqArray.ofSeq (seq { for b in bs -> b ts })))

        | Type.VarType(Typar.Named(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Regular)
            Ok(fun ts -> ts.[idx])

        | Type.VarType(Typar.Static(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Static)
            Ok(fun ts -> ts.[idx])

        | Type.VarType(Typar.Anon _) ->
            // Synthetic name so distinct anonymous typars don't collide.
            let synthetic = sprintf "_anon%d" typars.Count
            let idx = typars.IndexOf(synthetic, TyparKind.Regular)
            Ok(fun ts -> ts.[idx])

        | Type.NamedType li ->
            let name = longIdentName lexed input li

            if isPrimitiveName name then
                let ty = TyConst(name, EqArray.empty)
                Ok(fun _ -> ty)
            else
                match resolveTypeName ctx opens name 0 with
                // A name that resolves to nothing in scope bakes a `TyUnknown`
                // leaf rather than skipping the val: it surfaces as a
                // use-site diagnostic instead of a silent drop.
                | Error _ -> Ok(fun _ -> TyUnknown name)
                // Kind the bare nominal against the in-scope shapes (a zero-arity
                // union bakes `TyUnion(compiled, [])`, etc.); `mkNominal` runs in
                // the deferred builder so forward references resolve.
                | Ok compiled -> Ok(fun _ -> mkNominal ctx compiled EqArray.empty)

        | Type.GenericType(li, _, args, _, _) ->
            let name = longIdentName lexed input li
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    match args.[i] with
                    | TypeArg.Type t ->
                        match translateType ctx lexed input opens typars constraints t with
                        | Error e -> err <- Some e
                        | Ok b -> builders.Add b
                    | TypeArg.Measure _ -> err <- Some "Measure arg not supported"

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()

                match resolveTypeName ctx opens name bs.Length with
                // Unresolved head ⇒ `TyUnknown` leaf. The arg builders
                // are dropped: an unknown head has no kind to carry them into, and
                // the use-site diagnostic only needs the name.
                | Error _ -> Ok(fun _ -> TyUnknown name)
                // Kind the head against the in-scope shapes (own + dependency
                // packages) at `Instantiate` time — see `mkNominal`.
                | Ok compiled -> Ok(fun ts -> mkNominal ctx compiled (EqArray.ofSeq (seq { for b in bs -> b ts })))

        | Type.SuffixedType(baseTy, li) ->
            // `'T list` ≡ `List<'T>`.
            let name = longIdentName lexed input li

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb ->
                match resolveTypeName ctx opens name 1 with
                // Unresolved suffix head ⇒ `TyUnknown` leaf.
                | Error _ -> Ok(fun _ -> TyUnknown name)
                // `'T list` ≡ `List<'T>`; kind the head against the in-scope
                // shapes at `Instantiate` time — see `mkNominal`.
                | Ok compiled -> Ok(fun ts -> mkNominal ctx compiled (EqArray.singleton (fb ts)))

        | Type.ArrayType(baseTy, _, commas, _) ->
            // rank = commas + 1; key by `array<rank>` so unification stays simple.
            let rank = commas.Length + 1

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb -> Ok(fun ts -> TyConst(RuntimeNames.arrayName rank, EqArray.singleton (fb ts)))

        | Type.WhenConstrainedType(inner, clauses) ->
            // Capture clauses now; the collector is resolved (name -> index)
            // only once the body is fully walked, so a clause referencing a
            // typar declared anywhere in the val still resolves.
            captureConstraints lexed input constraints clauses
            translateType ctx lexed input opens typars constraints inner

        | Type.SubtypeConstraint(_, _, inner)
        | Type.AnonymousSubtype(_, inner) -> translateType ctx lexed input opens typars constraints inner

        | Type.DottedType(baseTy, _, _) ->
            // Treat `T.NestedName` as opaque (pass the base); proper nested-type
            // modelling lands later.
            translateType ctx lexed input opens typars constraints baseTy

        | Type.UnionType _ -> Error "Union types (e.g. `obj | null`) not supported"
        | Type.Null _ -> Error "Null types not supported"
        | Type.ILIntrinsic _ -> Error "Inline IL not supported"
        | Type.MeasureType _ -> Error "Measure types not supported"
        | Type.AnonRecordType _ -> Error "Anonymous record types not supported"
        | Type.Missing -> Error "Missing type"
        | Type.SkipsTokens _ -> Error "Recovery-skipped type"

    let translateArgsSpec
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (argsSpec: ArgsSpec<SyntaxToken>)
        : Result<SemBuilder, string> =
        let (ArgsSpec(args, _)) = argsSpec

        if args.Length = 0 then
            Ok(fun _ -> TyConst("unit", EqArray.empty))
        elif args.Length = 1 then
            let (ArgSpec(_, _, t)) = args.[0]
            translateType ctx lexed input opens typars constraints t
        else
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (ArgSpec(_, _, t)) = args.[i]

                    match translateType ctx lexed input opens typars constraints t with
                    | Error e -> err <- Some e
                    | Ok b -> builders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()
                Ok(fun ts -> TyTuple(EqArray.ofSeq (seq { for b in bs -> b ts })))

    let translateCurriedSig
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (sigCurried: CurriedSig<SyntaxToken>)
        : Result<SemBuilder, string> =
        let (CurriedSig(args, retTy)) = sigCurried

        match translateType ctx lexed input opens typars constraints retTy with
        | Error e -> Error e
        | Ok retBuilder ->
            // Args nest right-assoc:
            //   `int -> string -> bool` ≡ `TyFun(int, TyFun(string, bool))`.
            let mutable err = None
            let argBuilders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (struct (argsSpec, _)) = args.[i]

                    match translateArgsSpec ctx lexed input opens typars constraints argsSpec with
                    | Error e -> err <- Some e
                    | Ok b -> argBuilders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let argArr = argBuilders.ToArray()

                let final =
                    fun (ts: SemType[]) ->
                        let mutable acc = retBuilder ts

                        for k in argArr.Length - 1 .. -1 .. 0 do
                            acc <- TyFun(argArr.[k] ts, acc)

                        acc

                Ok final

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
