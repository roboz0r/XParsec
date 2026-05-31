namespace XParsec.FSharp.SemanticAnalysis

open System
open System.IO
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open VesperLibTyparCapture
open VesperLibTypeTranslate

/// Loads `XParsec.FSharp.Lib` (the signature-only port of FSharp.Core) and
/// answers `IExternalSymbolProvider` lookups from the parsed signatures.
///
/// The module is split across `VesperLib\Manifest.fs` (the bucket / file
/// loader), `VesperLib\TyparCapture.fs` (typar + constraint collectors plus
/// `ExtractCtx`), `VesperLib\TypeTranslate.fs` (CST → `SemBuilder` and the
/// `when`-clause capture), and this file (val-sig / type-sig / module-walker
/// driver plus the cached provider). External callers consume the public
/// surface through `VesperLib.*` — type aliases / re-exports below.
///
/// See `src/XParsec.FSharp.Lib/compiler-clr-project.md` for the manifest
/// schema and bucket layout, and `docs/extract-symbols-plan.md` for the
/// extraction design.
module VesperLib =

    type BucketEntry = VesperLibManifest.BucketEntry
    type RootManifest = VesperLibManifest.RootManifest
    type LibFile = VesperLibManifest.LibFile
    type LoadedLib = VesperLibManifest.LoadedLib
    type ParsedFile = VesperLibManifest.ParsedFile

    let loadAll = VesperLibManifest.loadAll
    let parseFileFull = VesperLibManifest.parseFileFull
    let parseFile = VesperLibManifest.parseFile

    type ExtractCtx = VesperLibTyparCapture.ExtractCtx

    module ExtractCtx =
        let empty = VesperLibTyparCapture.ExtractCtx.empty
        let toProvider = VesperLibTyparCapture.ExtractCtx.toProvider

    let private isAccessible (access: Access<SyntaxToken> voption) : bool =
        match access with
        | ValueNone
        | ValueSome(Access.Public _) -> true
        | ValueSome(Access.Internal _)
        | ValueSome(Access.Private _) -> false

    /// Build the compiled name for a val. Respects `[<CompiledName(_)>]`
    /// (overrides the source ident) and the `ModuleSuffix` flag (already
    /// baked into `path`'s last segment by the walker).
    let private compiledNameForVal
        (lexed: Lexed)
        (input: string)
        (path: string list)
        (attrs: Attributes<SyntaxToken> voption)
        (ident: IdentOrOp<SyntaxToken>)
        : string voption =
        let identName =
            match tryCompiledName lexed input attrs with
            | ValueSome n -> ValueSome n
            | ValueNone -> identOrOpName lexed input ident

        match identName with
        | ValueNone -> ValueNone
        | ValueSome n ->
            let qualifier = String.concat "." (List.rev path)

            if qualifier.Length = 0 then
                ValueSome n
            else
                ValueSome(qualifier + "." + n)

    /// Resolve a `RawConstraint`'s typar names against the val's typar
    /// collector, dropping entries that reference an undeclared typar.
    /// `Trait` and `MemberTrait` entries fold to opaque markers; `Default`
    /// entries translate their target `Type<SyntaxToken>` through the same
    /// `translateType` path the val signature used, so the default's RHS
    /// resolves typar references through the same indexing scheme.
    /// Entries whose target fails translation (e.g. references an unknown
    /// type name) silently drop — the default would be unusable in
    /// Instantiate anyway.
    let private resolveConstraints
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (raw: RawConstraint list)
        : ExternalConstraint list =
        // Sink for any constraints a target type carries (exotic, not
        // surfaced on the symbol).
        let throwaway = ConstraintCollector()

        raw
        |> List.choose (fun rc ->
            match rc with
            | RawConstraint.Trait(n, kind) ->
                match typars.TryIndexOf n with
                | ValueSome i -> Some(ExternalConstraint.Trait(i, kind))
                | ValueNone -> None
            | RawConstraint.MemberTrait(names, memberName, argTys, retTy) ->
                let indexBuf = ResizeArray<int>(List.length names)

                for n in names do
                    match typars.TryIndexOf n with
                    | ValueSome i -> indexBuf.Add i
                    | ValueNone -> ()

                if indexBuf.Count = 0 then
                    None
                else
                    // Any translation failure drops the whole entry — better
                    // to under-stamp the trait than to mis-stamp it.
                    let mutable failed = false
                    let argBuilders = ResizeArray<SemBuilder>(argTys.Length)

                    for t in argTys do
                        if not failed then
                            match translateType ctx lexed input opens typars throwaway t with
                            | Error _ -> failed <- true
                            | Ok b -> argBuilders.Add b

                    if failed then
                        None
                    else
                        match translateType ctx lexed input opens typars throwaway retTy with
                        | Error _ -> None
                        | Ok retBuilder ->
                            Some(
                                ExternalConstraint.MemberTrait(
                                    EqArray.ofResizeArray indexBuf,
                                    memberName,
                                    argBuilders.ToArray(),
                                    retBuilder
                                )
                            )
            | RawConstraint.Default(n, target) ->
                match typars.TryIndexOf n with
                | ValueNone -> None
                | ValueSome i ->
                    match translateType ctx lexed input opens typars throwaway target with
                    | Error _ -> None
                    | Ok builder -> Some(ExternalConstraint.Default(i, builder))
            | RawConstraint.Coercion(n, target) ->
                match typars.TryIndexOf n with
                | ValueNone -> None
                | ValueSome i ->
                    match translateType ctx lexed input opens typars throwaway target with
                    | Error _ -> None
                    | Ok builder -> Some(ExternalConstraint.Coercion(i, builder))
        )

    let private extractValSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        // The *source* module path (no `ModuleSuffix` rewrite). Differs from
        // `path` only inside a `[<CompilationRepresentation(ModuleSuffix)>]`
        // module (`List` ⇒ compiled `ListModule`). The provider is probed with the
        // *source*-qualified name the front end writes (`List.fold`, not
        // `ListModule.fold`), so the val is additionally registered under that
        // name; the compiled-name entry stays for the desugar/Freeze paths that key
        // on it (and the VesperLib `…Module.Map` tests).
        (sourcePath: string list)
        (valSig: ValSig<SyntaxToken>)
        : unit =
        let (ValSig(attrs, _, _, access, _, ident, typars, _, signature, _)) = valSig

        if not (isAccessible access) then
            ()
        else
            match compiledNameForVal lexed input path attrs ident with
            | ValueNone -> ()
            | ValueSome compiled ->
                let collector = TyparCollector()
                registerExplicitTypars lexed input collector typars
                let constraints = ConstraintCollector()

                match translateCurriedSig ctx lexed input opens collector constraints signature with
                | Error e ->
                    // Skip so unresolved names don't masquerade as opaque
                    // TyConsts. Per-val skips go to `Skipped`, not
                    // `Diagnostics` (reserved for file-level parse failures).
                    ctx.Skipped.Add(file, sprintf "%s: %s" compiled e)
                | Ok build ->
                    let typarCount = collector.Count

                    let resolved =
                        resolveConstraints ctx lexed input opens collector (constraints.Snapshot())

                    // Trait-style entries are applied at instantiation time;
                    // everything else stays on the symbol for diagnostics.
                    let traitConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.Trait(i, k) -> Some(i, k)
                            | _ -> None
                        )

                    // Applied at instantiation time so the source TyVar's
                    // `Defaults` list carries the resolved target;
                    // generalisation walks the list when a TyVar is still free
                    // and links it to the first concrete shape.
                    let defaultConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.Default(i, builder) -> Some(i, builder)
                            | _ -> None
                        )

                    // Stamped on every participating fresh TyVar's `SrtpBounds`;
                    // `Unification.drainSrtpBounds` fires the first time any of
                    // them gets a `Link`, sharing a `Resolved` ref so the others
                    // no-op.
                    let memberTraitConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.MemberTrait(idxs, name, argBs, retB) -> Some(idxs, name, argBs, retB)
                            | _ -> None
                        )

                    // `when 'e :> <ty>` entries — stamped as a
                    // `SemanticConstraintKind.Coercion` on the fresh TyVar so the
                    // first `Link` fires `checkConstraint`/`subsumes`.
                    let coercionConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.Coercion(i, builder) -> Some(i, builder)
                            | _ -> None
                        )

                    let instantiate =
                        if typarCount = 0 then
                            let semType = build [||]
                            fun _ -> semType
                        else
                            fun level ->
                                let freshTvs =
                                    Array.init
                                        typarCount
                                        (fun _ ->
                                            let tv = TypeVar()
                                            tv.Level <- level
                                            tv
                                        )

                                let fresh = freshTvs |> Array.map TyVar

                                for (i, kind) in traitConstraints do
                                    if i >= 0 && i < freshTvs.Length then
                                        // External symbols carry no source-side
                                        // NodeKey; stamp `Unknown` so diagnostics
                                        // attribute the constraint to the use site.
                                        let cstr: SemanticConstraint =
                                            {
                                                Kind = kind
                                                DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                                            }

                                        freshTvs.[i].Constraints <- cstr :: freshTvs.[i].Constraints

                                // Defaults accumulate newest-last so source
                                // order is preserved when generalisation later
                                // walks the list for the first concrete shape.
                                for (i, builder) in defaultConstraints do
                                    if i >= 0 && i < freshTvs.Length then
                                        let target = builder fresh
                                        let tv = freshTvs.[i]
                                        tv.Defaults <- tv.Defaults @ [ target ]

                                // Shared `Resolved` ref dedupes dispatch:
                                // whichever participating typar resolves first
                                // runs the drain; the others see it flipped and
                                // skip.
                                for (idxs, mName, argBs, retB) in memberTraitConstraints do
                                    let argTyBuf = ResizeArray<SemType>(argBs.Length)

                                    for b in argBs do
                                        argTyBuf.Add(b fresh)

                                    let retTy = retB fresh

                                    let sig_: MemberSignature =
                                        {
                                            MemberName = mName
                                            ArgTypes = EqArray.ofResizeArray argTyBuf
                                            ReturnType = retTy
                                            Resolved = false
                                        }

                                    for i in idxs do
                                        if i >= 0 && i < freshTvs.Length then
                                            let tv = freshTvs.[i]
                                            tv.SrtpBounds <- sig_ :: tv.SrtpBounds

                                // The target is built against the SAME fresh array,
                                // so a self-referential `'e :> 'f` resolves too.
                                for (i, builder) in coercionConstraints do
                                    if i >= 0 && i < freshTvs.Length then
                                        let cstr: SemanticConstraint =
                                            {
                                                Kind = SemanticConstraintKind.Coercion(builder fresh)
                                                DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                                            }

                                        freshTvs.[i].Constraints <- cstr :: freshTvs.[i].Constraints

                                build fresh

                    let sym: ExternalSymbol =
                        {
                            Name = compiled
                            Instantiate = instantiate
                            Constraints = resolved
                            Origin = SymbolOrigin.Empty
                            Key = ExternalSymbols.valueKeyOf None compiled
                        }

                    ctx.Symbols.[compiled] <- sym

                    // Source-name alias for a `ModuleSuffix` module's members
                    // (`List.fold` alongside the compiled `ListModule.fold`), so a
                    // front-end probe of the source-qualified name resolves. Only
                    // when it differs from the compiled name and nothing already
                    // claims it (first registration wins, like the type index).
                    match compiledNameForVal lexed input sourcePath attrs ident with
                    | ValueSome source when source <> compiled && not (ctx.Symbols.ContainsKey source) ->
                        // The alias keeps pointing at the SAME compiled-name key:
                        // both source and compiled forms (e.g. `List.fold` and
                        // `ListModule.fold`) denote one symbol identity. `stack`'s
                        // stampSymbol re-mints the key with the wrapping package's
                        // assembly, so this default never leaks past `wrap`.
                        ctx.Symbols.[source] <-
                            { sym with
                                Name = source
                                Key = ExternalSymbols.valueKeyOf None compiled
                            }
                    | _ -> ()

    let private registerPrefixTypars
        (lexed: Lexed)
        (input: string)
        (typars: TyparCollector)
        (prefix: PrefixTypars<SyntaxToken> voption)
        : unit =
        let register (t: Typar<SyntaxToken>) =
            match t with
            | Typar.Named(_, identTok) ->
                let name = nameOfTok lexed input identTok
                typars.IndexOf(name, TyparKind.Regular) |> ignore
            | Typar.Static(_, identTok) ->
                let name = nameOfTok lexed input identTok
                typars.IndexOf(name, TyparKind.Static) |> ignore
            | Typar.Anon _ -> ()

        match prefix with
        | ValueNone -> ()
        | ValueSome(PrefixTypars.Single t) -> register t
        | ValueSome(PrefixTypars.Multiple(_, items, _, _)) ->
            for i in 0 .. items.Length - 1 do
                register items.[i]

    let private typeNameTypars
        (defns: TyparDefns<SyntaxToken> voption)
        (prefix: PrefixTypars<SyntaxToken> voption)
        : int =
        let prefixCount =
            match prefix with
            | ValueSome(PrefixTypars.Single _) -> 1
            | ValueSome(PrefixTypars.Multiple(_, items, _, _)) -> items.Length
            | ValueNone -> 0

        let defnCount =
            match defns with
            | ValueSome(TyparDefns(_, items, _, _)) -> items.Length
            | ValueNone -> 0

        max prefixCount defnCount

    /// Register a `type` declaration's short name + qualified compiled name.
    /// Returns the (compiled name, declared arity) tuple so the body
    /// extractor below can populate `ctx.TypeShapes` against the same key.
    /// `ValueNone` indicates the declaration was malformed (no ident).
    /// The undotted short name of a `TypeName` (its last ident segment), or `""`
    /// when it carries none. The key `ctx.IntrinsicReprs` is harvested under.
    let private shortNameOfTypeName (lexed: Lexed) (input: string) (typeName: TypeName<SyntaxToken>) : string =
        let (TypeName(_, _, _, ident, _, _)) = typeName

        if ident.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed input ident.Idents.[ident.Idents.Length - 1]

    let private registerTypeDecl
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (path: string list)
        (typeName: TypeName<SyntaxToken>)
        : struct (string * int) voption =
        let (TypeName(_, _, prefix, ident, defns, _)) = typeName

        if ident.Idents.Length = 0 then
            ValueNone
        else
            let short = nameOfTok lexed input ident.Idents.[ident.Idents.Length - 1]

            if short.Length = 0 then
                ValueNone
            else
                let arity = typeNameTypars defns prefix

                let qualifier = String.concat "." (List.rev path)

                let compiled =
                    if qualifier.Length = 0 then
                        short
                    else
                        qualifier + "." + short

                // First declaration wins on collision.
                if not (ctx.Types.ContainsKey short) then
                    ctx.Types.[short] <- (arity, compiled)

                ctx.QualifiedTypes.Add compiled |> ignore
                ValueSome(struct (compiled, arity))

    let private collectorForTypeName (lexed: Lexed) (input: string) (typeName: TypeName<SyntaxToken>) : TyparCollector =
        let (TypeName(_, _, prefix, _, defns, _)) = typeName
        let collector = TyparCollector()
        registerPrefixTypars lexed input collector prefix
        registerExplicitTypars lexed input collector defns
        collector

    /// `Ok` only if every typar the body touched was declared in the type's
    /// prefix / typar-defns — otherwise the shape would be instantiable with
    /// a wrong-length array. v1 skips the body in that case but keeps the
    /// short-name registration so other types can still reference it nominally.
    let private bodyTyparsOk (collector: TyparCollector) (declaredArity: int) : Result<unit, string> =
        if collector.Count <= declaredArity then
            Ok()
        else
            Error(sprintf "body references %d typars but only %d declared" collector.Count declaredArity)

    /// A body extractor bailed (an unsupported field/case/RHS form, a typar-arity
    /// overflow): record *why* in `ctx.Skipped` AND register the `Opaque` residue
    /// shape, so the type — whose name+arity are known — keeps a shape (no
    /// name-without-shape gap) and `TryLookupType` stays total. A signature that
    /// actually *names* such a body-less type is refused at bake time by
    /// `mkNominal`'s `Opaque` arm.
    let private skipBodyOpaque
        (ctx: ExtractCtx)
        (file: LibFile)
        (compiled: string)
        (arity: int)
        (reason: string)
        : unit =
        ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled reason)
        ctx.TypeShapes.[compiled] <- ExternalTypeShape.Opaque arity

    let private extractAbbrevBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()

        match translateType ctx lexed input opens collector throwawayConstraints rhs with
        | Error e -> skipBodyOpaque ctx file compiled arity e
        | Ok build ->
            match bodyTyparsOk collector arity with
            | Error e -> skipBodyOpaque ctx file compiled arity e
            | Ok() -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Abbrev(arity, build)

    let private extractRecordBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (fields: RecordFields<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()
        let shapes = ResizeArray<ExternalFieldShape>(fields.Length)
        let mutable err = None

        for i in 0 .. fields.Length - 1 do
            if err.IsNone then
                let (RecordField(_, mutableTok, _, identTok, _, fieldTy)) = fields.[i]

                match translateType ctx lexed input opens collector throwawayConstraints fieldTy with
                | Error e -> err <- Some e
                | Ok b ->
                    shapes.Add
                        {
                            Name = nameOfTok lexed input identTok
                            IsMutable = mutableTok.IsSome
                            BuildType = b
                        }

        match err with
        | Some e -> skipBodyOpaque ctx file compiled arity e
        | None ->
            match bodyTyparsOk collector arity with
            | Error e -> skipBodyOpaque ctx file compiled arity e
            | Ok() ->
                // `Origin` is filled later by `ReferencedProject.wrap` (which
                // knows the package's assembly + namespace from the manifest);
                // the extractor itself records `Empty`.
                ctx.TypeShapes.[compiled] <- ExternalTypeShape.Record(arity, shapes.ToArray(), SymbolOrigin.Empty)

    let private extractUnionBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (cases: UnionTypeCases<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()
        let caseShapes = ResizeArray<ExternalCaseShape>(cases.Length)
        let mutable err = None

        let caseName (ioo: IdentOrOp<SyntaxToken>) : string voption =
            match ioo with
            | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
            | _ ->
                // Operator-named cases (the cons-list's `([])` → `op_Nil`,
                // `(::)` → `op_ColonColon`) keep their *compiled-op* form, NOT the
                // source ctor name (`Empty`/`Cons`). The cons-list is special-cased
                // throughout construction/literals (`FreezeExpr`, `TryEmitUnionCons`),
                // so its case names are never resolved through the generic
                // `TryLookupUnionCase` path; the op-form keeps them from colliding
                // with a user union's `Cons` / `Nil` in ctor-name resolution.
                identOrOpName lexed input ioo

        for i in 0 .. cases.Length - 1 do
            if err.IsNone then
                let (UnionTypeCase(_, data)) = cases.[i]

                match data with
                | UnionTypeCaseData.Nullary ident ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add
                            {
                                Name = n
                                FieldNames = [||]
                                BuildFieldTypes = [||]
                            }

                | UnionTypeCaseData.Nary(ident, _, fields, _) ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(fields.Length)
                        let builds = ResizeArray<SemBuilder>(fields.Length)

                        for j in 0 .. fields.Length - 1 do
                            if err.IsNone then
                                let nameOpt, fieldTy =
                                    match fields.[j] with
                                    | UnionTypeField.Unnamed t -> ValueNone, t
                                    | UnionTypeField.Named(identTok, _, t) ->
                                        ValueSome(nameOfTok lexed input identTok), t

                                match translateType ctx lexed input opens collector throwawayConstraints fieldTy with
                                | Error e -> err <- Some(sprintf "case %s field: %s" n e)
                                | Ok b ->
                                    names.Add nameOpt
                                    builds.Add b

                        if err.IsNone then
                            caseShapes.Add
                                {
                                    Name = n
                                    FieldNames = names.ToArray()
                                    BuildFieldTypes = builds.ToArray()
                                }

                | UnionTypeCaseData.GadtNullary(name = ident) ->
                    // GADT-syntax nullary (`([]): 'T list`): the explicit return
                    // type is the declaring union and carries no field, so it
                    // models exactly as an ordinary nullary case. A true
                    // type-refining return type is out of scope — the same stance
                    // the front-end's `TypeRegistration.inspectCaseData` takes.
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add
                            {
                                Name = n
                                FieldNames = [||]
                                BuildFieldTypes = [||]
                            }

                | UnionTypeCaseData.GadtNary(name = ident; sign = UncurriedSig(args = ArgsSpec(specs, _))) ->
                    // GADT-syntax n-ary (`(::): Head: 'T * Tail: 'T list -> 'T list`):
                    // the fields are the signature's args; the return type names the
                    // declaring union and is ignored (true type-refining GADTs are
                    // out of scope, exactly as in the front-end).
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(specs.Length)
                        let builds = ResizeArray<SemBuilder>(specs.Length)

                        for j in 0 .. specs.Length - 1 do
                            if err.IsNone then
                                let (ArgSpec(_, nameSpec, fieldTy)) = specs.[j]

                                let nameOpt =
                                    match nameSpec with
                                    | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(nameOfTok lexed input id)
                                    | ValueNone -> ValueNone

                                match translateType ctx lexed input opens collector throwawayConstraints fieldTy with
                                | Error e -> err <- Some(sprintf "case %s field: %s" n e)
                                | Ok b ->
                                    names.Add nameOpt
                                    builds.Add b

                        if err.IsNone then
                            caseShapes.Add
                                {
                                    Name = n
                                    FieldNames = names.ToArray()
                                    BuildFieldTypes = builds.ToArray()
                                }

        match err with
        | Some e -> skipBodyOpaque ctx file compiled arity e
        | None ->
            match bodyTyparsOk collector arity with
            | Error e -> skipBodyOpaque ctx file compiled arity e
            // `Origin` is filled later by `ReferencedProject.wrap` (which knows
            // the package's assembly + namespace from the manifest); the
            // extractor itself records `Empty`.
            | Ok() ->
                ctx.TypeShapes.[compiled] <- ExternalTypeShape.Union(arity, caseShapes.ToArray(), SymbolOrigin.Empty)

    /// Extract the augmentation `member`s declared inside a type body's
    /// `with`-block (`member Value: 'T` / `member IsSome: bool` on `Option`)
    /// into `ctx.TypeMembers`, keyed by the type's qualified compiled name.
    /// Each member's signature is translated over the *type's* typar collector
    /// (so `'T` substitutes through the enclosing type's args at a use site),
    /// yielding the `ExternalMember.BuildSignature` the consumer's
    /// `resolveFieldStep` instantiates. Scope (vesper-lib-test-plan Gap 2 Layer
    /// A): instance/static `member` property/method sigs with no own generic
    /// parameters — a member that introduces its own typars, or whose signature
    /// fails to translate, is skipped (not faked), exactly like the val path.
    let private extractTypeMembers
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (extensions: TypeExtensionElementsSignature<SyntaxToken> voption)
        : unit =
        match extensions with
        | ValueNone -> ()
        | ValueSome(TypeExtensionElementsSignature(_, elems, _)) ->
            // Split the qualified compiled name into the declaring `TypeKey`
            // (ns, simple name) so each member carries a best-effort identity;
            // the asm slot is stamped later by the wrapping source.
            let declKey =
                let i = compiled.LastIndexOf '.'

                if i < 0 then
                    SymbolKey.TypeKey(None, "", compiled)
                else
                    SymbolKey.TypeKey(None, compiled.Substring(0, i), compiled.Substring(i + 1))

            let members = ResizeArray<ExternalMember>()

            for i in 0 .. elems.Length - 1 do
                let memberSig =
                    match elems.[i] with
                    | TypeSignatureElement.Member(signature = s) -> ValueSome(false, s)
                    | TypeSignatureElement.StaticMember(signature = s) -> ValueSome(true, s)
                    | _ -> ValueNone

                match memberSig with
                | ValueNone -> ()
                | ValueSome(isStatic, sign) ->
                    // Only property/method sigs with no own generic parameters
                    // (`typarDefns = ValueNone`): an own-typar member would need
                    // fresh args the consumer can't mint from the receiver's arg
                    // list alone, so it is skipped.
                    let identAndSig =
                        match sign with
                        | MemberSig.MethodOrPropSig(ident = ioo; typarDefns = ValueNone; sign = csig) ->
                            ValueSome(ioo, csig, false)
                        | MemberSig.PropSig(ident = ioo; typarDefns = ValueNone; sign = csig) ->
                            ValueSome(ioo, csig, true)
                        | _ -> ValueNone

                    match identAndSig with
                    | ValueNone -> ()
                    | ValueSome(ioo, csig, isPropSig) ->
                        match identOrOpName lexed input ioo with
                        | ValueNone -> ()
                        | ValueSome memberName ->
                            let collector = collectorForTypeName lexed input typeName
                            let throwaway = ConstraintCollector()

                            match translateCurriedSig ctx lexed input opens collector throwaway csig with
                            | Error _ -> ()
                            | Ok builder ->
                                // A member whose sig pulled in typars beyond the
                                // type's own can't be instantiated from the
                                // receiver's args alone — skip it.
                                if collector.Count <= arity then
                                    let (CurriedSig(args, _)) = csig
                                    let isProperty = isPropSig || args.Length = 0

                                    let kind =
                                        if isProperty then
                                            MemberKind.Property
                                        else
                                            MemberKind.Method

                                    members.Add
                                        {
                                            Name = memberName
                                            IsStatic = isStatic
                                            IsProperty = isProperty
                                            BuildSignature = builder
                                            Origin = SymbolOrigin.Empty
                                            Key = SymbolKey.MemberKey(declKey, memberName, EqArray.empty, kind)
                                        }

            if members.Count > 0 then
                ctx.TypeMembers.[compiled] <- members

    let private extractTypeSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        (ts: TypeSignature<SyntaxToken>)
        : unit =
        match ts with
        | TypeSignature.Abbrev(typeName, _, rhs) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractAbbrevBody ctx file lexed input opens compiled arity typeName rhs

        | TypeSignature.Record(typeName = typeName; fields = fields) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractRecordBody ctx file lexed input opens compiled arity typeName fields

        | TypeSignature.Union(typeName = typeName; cases = cases; extensions = extensions) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractUnionBody ctx file lexed input opens compiled arity typeName cases
                extractTypeMembers ctx lexed input opens compiled arity typeName extensions

        | TypeSignature.Interface(typeName = typeName) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                // A class/interface shape carries no body (its members are
                // resolved separately via `TryLookupMember`), but registering it
                // here is what makes a nominal external type — `Vesper.Fun`,
                // `EqualityComparer<_>` — resolve through `TryLookupType` instead
                // of `ValueNone` (symbol-resolution-plan §4). `Origin` is stamped
                // by the resolving source (e.g. `ReferencedProject`), not here.
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Class(ExternalClassShape.basic (arity, true, SymbolOrigin.Empty))

        | TypeSignature.Extern(typeName = typeName) ->
            // An `extern` type is either an intrinsic-repr primitive (its sibling
            // `.fs` carries `type x = (# "<repr>" #)`, harvested into
            // `ctx.IntrinsicReprs` before extraction) or an opaque abstract type
            // / real class with no `.fs` binding. The former publishes as a
            // NON-transparent `Intrinsic repr` (a use site resolves to the
            // nominal `TyConst name`, the repr feeding codegen / `subsumes`); the
            // latter falls through to `Class` exactly as a non-extern nominal
            // does (intrinsic-repr-handoff.md — `.fsi`/`.fs` pairing).
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                let short = shortNameOfTypeName lexed input typeName

                match ctx.IntrinsicReprs.TryGetValue short with
                | true, repr -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Intrinsic repr
                | _ ->
                    ctx.TypeShapes.[compiled] <-
                        ExternalTypeShape.Class(ExternalClassShape.basic (arity, false, SymbolOrigin.Empty))

        | TypeSignature.Anon(typeName = typeName)
        | TypeSignature.Class(typeName = typeName)
        | TypeSignature.Struct(typeName = typeName)
        | TypeSignature.AbstractType typeName ->
            // Nominal types with no front-end-modelled body shape (a class, an
            // opaque abstract type). Resolve as a non-interface `Class` so codegen
            // can mint a ref off the origin.
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Class(ExternalClassShape.basic (arity, false, SymbolOrigin.Empty))

        | TypeSignature.Enum(typeName = typeName)
        | TypeSignature.Delegate(typeName = typeName)
        | TypeSignature.TypeExtension(typeName = typeName) ->
            // Enum / delegate / type-extension body shapes land later
            // v1 registers the name+arity plus an `Opaque` residue shape so every registered name
            // carries a shape — no name-without-shape gap, `TryLookupType` total.
            // A contract that actually *names* one is refused at bake time by
            // `mkNominal`'s `Opaque` arm (none does today).
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Opaque arity

    /// Harvest the `open Foo.Bar` clauses from a flat element list. The
    /// caller prepends to its inherited list so a scope's own opens are
    /// tried *first* (newest-first) during resolution.
    let private collectOpens
        (lexed: Lexed)
        (input: string)
        (elems: System.Collections.Immutable.ImmutableArray<ModuleSignatureElement<SyntaxToken>>)
        : string list =
        let acc = ResizeArray<string>()

        for i in 0 .. elems.Length - 1 do
            match elems.[i] with
            | ModuleSignatureElement.Import(ImportDecl.ImportDecl(_, li)) -> acc.Add(longIdentName lexed input li)
            | ModuleSignatureElement.Import(ImportDecl.ImportDeclType _) ->
                // `open type Foo` brings only Foo's static members into scope,
                // not Foo as a prefix. Ignore for v1.
                ()
            | _ -> ()

        // Newest first: a later `open` shadows earlier ones.
        List.ofSeq (Seq.rev acc)

    let rec private extractModuleSigElement
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        // The source-name twin of `path` (see `extractValSig`). Equal to `path`
        // except inside a `ModuleSuffix` module, where `path` carries the compiled
        // `…Module` segment and this carries the source segment.
        (sourcePath: string list)
        (elem: ModuleSignatureElement<SyntaxToken>)
        : unit =
        match elem with
        | ModuleSignatureElement.Val valSig -> extractValSig ctx file lexed input opens path sourcePath valSig

        | ModuleSignatureElement.Type(_, typeSigs) ->
            let (TypeSignatures(first, rest)) = typeSigs
            extractTypeSig ctx file lexed input opens path first

            for i in 0 .. rest.Length - 1 do
                let (_, ts) = rest.[i]
                extractTypeSig ctx file lexed input opens path ts

        | ModuleSignatureElement.Module moduleSig ->
            let (ModuleSignature(attrs, _, access, _, identTok, _, body)) = moduleSig

            if isAccessible access then
                let name = nameOfTok lexed input identTok

                let suffixed =
                    if hasModuleSuffix lexed input attrs then
                        name + "Module"
                    else
                        name

                let childPath = suffixed :: path
                let childSourcePath = name :: sourcePath
                let (ModuleSignatureBody(_, elems, _)) = body
                // The module's own qualified path is itself an implicit open
                // prefix, ahead of the inherited opens but behind the body's.
                let modulePath = String.concat "." (List.rev childPath)

                // An `[<AutoOpen>]` module contributes its qualified path to the
                // contract's ambient prefix set (symbol-resolution-handoff.md, open-resolution).
                if isAutoOpen lexed input attrs then
                    ctx.AutoOpenPrefixes.Add modulePath

                let childOpens = collectOpens lexed input elems @ (modulePath :: opens)

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement ctx file lexed input childOpens childPath childSourcePath elems.[i]

        | _ -> ()

    let private extractNamespaceGroup
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (fileOpens: string list)
        (group: NamespaceDeclGroupSignature<SyntaxToken>)
        : unit =
        let nsPath, elems =
            match group with
            | NamespaceDeclGroupSignature.Named(_, _, li, els) ->
                let parts =
                    [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]

                List.rev parts, els
            | NamespaceDeclGroupSignature.Global(_, _, els) -> [], els

        let nsName = String.concat "." (List.rev nsPath)
        let ownOpens = collectOpens lexed input elems
        // The namespace's qualified path is implicitly in scope; top-level
        // opens outside any namespace group are inherited.
        let opens =
            if nsName.Length = 0 then
                ownOpens @ fileOpens
            else
                ownOpens @ (nsName :: fileOpens)

        for i in 0 .. elems.Length - 1 do
            // A namespace path carries no `ModuleSuffix` rewrite, so source == compiled.
            extractModuleSigElement ctx file lexed input opens nsPath nsPath elems.[i]

    let private extractNamedModuleSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (fileOpens: string list)
        (nm: NamedModuleSignature<SyntaxToken>)
        : unit =
        let (NamedModuleSignature(attrs, _, access, _, li, elems)) = nm

        if isAccessible access then
            let suffix = hasModuleSuffix lexed input attrs

            let sourcePathRev =
                [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]
                |> List.rev

            // ModuleSuffix applies to the innermost segment only.
            let pathRev =
                match sourcePathRev, suffix with
                | head :: rest, true -> (head + "Module") :: rest
                | _ -> sourcePathRev

            let qualifiedSelf = String.concat "." (List.rev pathRev)

            if isAutoOpen lexed input attrs then
                ctx.AutoOpenPrefixes.Add qualifiedSelf

            let ownOpens = collectOpens lexed input elems
            let opens = ownOpens @ (qualifiedSelf :: fileOpens)

            for i in 0 .. elems.Length - 1 do
                extractModuleSigElement ctx file lexed input opens pathRev sourcePathRev elems.[i]

    let extractSymbols (ctx: ExtractCtx) (parsed: ParsedFile) : unit =
        match parsed.Ast with
        | FSharpAst.SignatureFile sf ->
            match sf with
            | SignatureFile.Namespaces groups ->
                for i in 0 .. groups.Length - 1 do
                    // Each namespace decl group starts a fresh open scope.
                    extractNamespaceGroup ctx parsed.File parsed.Lexed parsed.Input [] groups.[i]
            | SignatureFile.NamedModule nm -> extractNamedModuleSig ctx parsed.File parsed.Lexed parsed.Input [] nm
            | SignatureFile.AnonymousModule elems ->
                let opens = collectOpens parsed.Lexed parsed.Input elems

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement ctx parsed.File parsed.Lexed parsed.Input opens [] [] elems.[i]
        | _ -> ctx.Diagnostics.Add(parsed.File, "Skipped: not a signature file")

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS
    /// (`(# "System.Int32" #)` ⇒ `"System.Int32"`). Mirrors
    /// `NameResolution.TypeRegistration.ilIntrinsicString` for the extractor's
    /// `.fs`-harvest path, where the consumer's `PassContext` is not in scope.
    let private ilIntrinsicReprString
        (lexed: Lexed)
        (input: string)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
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
            | StringPart.InvalidText t -> sb.Append(nameOfTok lexed input t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString()

    /// Harvest the per-target intrinsic-representation bindings from a parsed
    /// `.fs` companion (`type exn = (# "System.Exception" #)`) into
    /// `ctx.IntrinsicReprs` (short name ⇒ repr). This is the `.fs` half of the
    /// `.fsi`/`.fs` pairing: the `.fsi` `type exn = extern` deliberately omits
    /// the repr, so the identity lives only here. Run BEFORE the `.fsi`
    /// extraction so the `extern` arm of `extractTypeSig` can publish
    /// `ExternalTypeShape.Intrinsic` (intrinsic-repr-handoff.md).
    ///
    /// A direct CST scrape — NOT `Pipeline.analyse` — because (a) all we need is
    /// the `type <name> = (# "<repr>" #)` shape, and (b) the prim-types `.fs`
    /// carry cons-list augmentation members that trip unimplemented analysis
    /// paths (`CstKeys.firstTokenOfPat: TODO Cons`). A later binding wins a clash.
    let harvestIntrinsicReprs (ctx: ExtractCtx) (parsed: ParsedFile) : unit =
        let implFile =
            match parsed.Ast with
            | FSharpAst.ImplementationFile f -> Some f
            | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
                Some(ImplementationFile.AnonymousModule elems)
            | _ -> None

        match implFile with
        | None -> ()
        | Some f ->
            let nameOf t = nameOfTok parsed.Lexed parsed.Input t

            for (m, _) in CstWalk.walkModuleTree nameOf OpenScope.empty f do
                match m with
                | ModuleElem.Type defs ->
                    for td in defs do
                        match td with
                        | TypeDefn.Abbrev(typeName = TypeName(ident = li); typ = Type.ILIntrinsic(instrParts = parts)) when
                            li.Idents.Length = 1
                            ->
                            ctx.IntrinsicReprs.[nameOf li.Idents.[0]] <-
                                ilIntrinsicReprString parsed.Lexed parsed.Input parts
                        | _ -> ()
                | _ -> ()

    /// F#'s implicit prelude namespaces / `[<AutoOpen>]` modules in
    /// `Microsoft.FSharp.*`. Seeded into `ctx.AutoOpenPrefixes` so the
    /// resulting provider's `IAmbientOpenScope` resolves the prelude the
    /// same way as `[<AutoOpen>]` modules discovered by extraction. Order
    /// matches F#'s prelude open order; earlier entries win on collision.
    let private fsharpCorePreludePrefixes =
        [
            "Microsoft.FSharp.Core.Operators"
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators"
            "Microsoft.FSharp.Core"
            "Microsoft.FSharp.Collections"
            "Microsoft.FSharp.Control"
        ]

    /// Builds an `IExternalSymbolProvider` backed by XParsec.FSharp.Lib.
    /// Returns the provider plus per-file errors so callers can decide
    /// whether to proceed with a partial table or fail loudly. Files that
    /// fail to parse contribute no symbols but do not abort the build.
    let buildProvider (libRoot: string) : Result<IExternalSymbolProvider * (LibFile * string) list, string> =
        match loadAll libRoot with
        | Error e -> Error e
        | Ok loaded ->
            let ctx = ExtractCtx.empty ()

            // Seed the F# prelude ahead of extraction so the discovered
            // `[<AutoOpen>]` modules append after it — earlier entries win
            // on shadowing.
            for prefix in fsharpCorePreludePrefixes do
                ctx.AutoOpenPrefixes.Add prefix

            for file in loaded.Files do
                match parseFileFull file with
                | Error e -> ctx.Diagnostics.Add(file, e)
                | Ok parsed -> extractSymbols ctx parsed

            Ok(ExtractCtx.toProvider ctx, List.ofSeq ctx.Diagnostics)

    /// Lazy cache keyed by `libRoot` so repeated callers parse the lib at
    /// most once per root. Thread-safe via `Lazy<_>` publication.
    let private cachedProviders =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<Result<IExternalSymbolProvider * (LibFile * string) list, string>>
         >(
            StringComparer.Ordinal
        )

    /// Production-path entry point: caches `buildProvider` per `libRoot`.
    /// Tests that need a fresh provider should call `buildProvider` instead.
    let defaultProvider (libRoot: string) : Result<IExternalSymbolProvider * (LibFile * string) list, string> =
        let normalised = Path.GetFullPath libRoot

        let entry =
            cachedProviders.GetOrAdd(normalised, (fun root -> lazy (buildProvider root)))

        entry.Value
