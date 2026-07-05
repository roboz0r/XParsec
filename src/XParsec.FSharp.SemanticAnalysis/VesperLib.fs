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
/// `ExtractCtx`), `VesperLib\TypeTranslate.fs` (the single CST → `FrozenType`
/// translation and the `when`-clause capture), and this file (val-sig /
/// type-sig / module-walker driver, the deferred-body/member/val finalize pass,
/// plus the cached provider). External callers consume the public
/// surface through `VesperLib.*` — type aliases / re-exports below.
///
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

    /// Translate a stashed body-type CST to its `FrozenType` template, degrading the
    /// whole field to `FTUnknown` in two tolerated cases: a genuinely body-less head
    /// (one that raises `BodylessExternalShape` inside `mkNominal`) and an
    /// unsupported structural form (`Error`). A typar reference beyond the type's
    /// declaring arity is NOT screened here — `instantiateDeclaring` degrades such an
    /// out-of-range leaf to `TyUnknown` on its own at use time, so the rest of the
    /// field's structure survives (a per-leaf degrade, finer than the old per-type
    /// `bodyTyparsOk` whole-type Opaque downgrade).
    let private freezeBodyType (ctx: ExtractCtx) (dc: DeferredCtx) (cst: Type<SyntaxToken>) : FrozenType =
        try
            match translateType ctx dc.Lexed dc.Input dc.Opens dc.Typars (ConstraintCollector()) cst with
            | Ok ft -> ft
            | Error _ -> ExternalSymbols.unfreezable
        with BodylessExternalShape _ ->
            ExternalSymbols.unfreezable

    /// Split a frozen `interface <ty>` declaration into the `(compiled-name,
    /// type-args)` pair `ExternalClassShape.FrozenInterfaces` holds (args over the
    /// declaring typars, `FTTypar(Declaring,i)`) — the `.fsi` analogue of the
    /// metadata layer's `buildClassInterfaces`. `None` for a non-nominal freeze (an
    /// unresolved name → `FTUnknown`), which carries no witness.
    let private nominalInterface (ft: FrozenType) : (string * FrozenType[]) option =
        match ft with
        | FTClass(k, args)
        | FTRecord(k, args)
        | FTUnion(k, args) -> Some(SymbolKeyOps.qualifiedName k, args.AsSpan().ToArray())
        | _ -> None

    /// Freeze a list of `interface <ty>` impl CSTs into the `(compiled-name, args)`
    /// pairs `FrozenInterfaces` holds, dropping any that don't freeze to a nominal
    /// witness. Shared by the union and class finalize arms.
    let private freezeInterfaces
        (ctx: ExtractCtx)
        (dc: DeferredCtx)
        (ifaces: Type<SyntaxToken> list)
        : (string * FrozenType[])[] =
        ifaces
        |> List.choose (fun t -> nominalInterface (freezeBodyType ctx dc t))
        |> List.toArray

    /// Does a `[<Struct>]`-attributed type declaration carry the attribute? A
    /// `[<Struct>] type X = …` (the ATTRIBUTE form, e.g. the struct-seq types) parses
    /// through the Class/Anon arm rather than `TypeSignature.Struct` (the
    /// `struct … end` form), so its value-type-ness is on the `TypeName`'s
    /// attributes, not the syntactic body. Shares `AttributeDecode` with the
    /// name-resolution pass (here the resolver is `nameOfTok lexed input`, the
    /// extractor has only `lexed`/`input`). Without this an external struct in a
    /// signature encodes as `CLASS` not `ELEMENT_TYPE_VALUETYPE` and a consumer's
    /// member-ref misses the value-type method (`MissingMethodException` / "value type
    /// mismatch").
    let private typeNameHasStructAttr (lexed: Lexed) (input: string) (typeName: TypeName<SyntaxToken>) : bool =
        let (TypeName(attributes = attrs)) = typeName
        (AttributeDecode.decodeClassAttributes (nameOfTok lexed input) attrs).IsValueType

    /// Translate a stashed member-signature CST to its two-axis `ExternalSignature`
    /// template, splitting the head `FTFun(params, ret)` (or treating the whole
    /// result as the value, for a property). `ValueNone` means **drop the member**:
    /// an unsupported structural form (`Error`). A genuinely body-less head
    /// (`BodylessExternalShape`) is kept but degraded to `unit -> FTUnknown`,
    /// matching the prior finalize tolerance.
    ///
    /// A member typar beyond the declaring type's own (`AppendFormatted: 'T -> unit`
    /// on the niladic `Formatter`, or an explicit `member Foo<'a>`) is no longer
    /// dropped: the member's collector interns it at an index `>= declaringArity`, so
    /// `reaxisMethodTypars` flips it onto the METHOD axis and the returned
    /// `MethodArity` (`collector.Count - declaringArity`) publishes the member's own
    /// generic-parameter count — the T8 Step 6 generic-member surface. The count is
    /// read off the collector AFTER the translate, so an implicit method typar
    /// interned during the signature walk (no explicit `<'T>`) is included.
    let private freezeMemberSig
        (ctx: ExtractCtx)
        (isProperty: bool)
        (declaringArity: int)
        (dm: DeferredMember)
        : ExternalSignature voption =
        let dc = dm.Ctx

        let methodArityNow () =
            max 0 (dc.Typars.Count - declaringArity)

        try
            match translateCurriedSig ctx dc.Lexed dc.Input dc.Opens dc.Typars (ConstraintCollector()) dm.Signature with
            | Ok frozen ->
                let frozen = FrozenTypeBridge.reaxisMethodTypars declaringArity frozen

                let parameters, ret =
                    if isProperty then
                        FTConst("unit", EqArray.empty), frozen
                    else
                        match frozen with
                        | FTFun(p, r) -> p, r
                        // A non-property member whose sig isn't a `FTFun` is folded as
                        // a nullary value rather than fabricating a parameter slot.
                        | other -> FTConst("unit", EqArray.empty), other

                ValueSome(ExternalSignature.make (declaringArity, methodArityNow (), parameters, ret))
            | Error _ -> ValueNone
        with BodylessExternalShape _ ->
            ValueSome(
                ExternalSignature.make (
                    declaringArity,
                    methodArityNow (),
                    FTConst("unit", EqArray.empty),
                    ExternalSymbols.unfreezable
                )
            )

    /// Resolve a `RawConstraint`'s typar names against the val's typar collector,
    /// dropping entries that reference an undeclared typar. `Trait` entries fold to
    /// an index + kind; `MemberTrait` / `Default` / `Coercion` translate their
    /// target `Type<SyntaxToken>` to a `FrozenType` template over the val's
    /// declaring typars (the same indexing the val signature uses), realised at
    /// instantiation time via `instantiateDeclaring`. Entries whose target fails
    /// translation silently drop — the constraint would be unusable at Instantiate
    /// anyway. Runs in the finalize pass, so the registry is complete.
    let private resolveConstraints
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (raw: RawConstraint list)
        : ExternalConstraint list =
        // Sink for any constraints a target type carries (exotic, not surfaced).
        let throwaway = ConstraintCollector()

        // A target naming a body-less (`Opaque`) type raises `BodylessExternalShape`;
        // fold it to `Error` so the constraint drops rather than aborting the build
        // (the prior closure path deferred the raise past resolution).
        let translate t =
            try
                translateType ctx lexed input opens typars throwaway t
            with BodylessExternalShape _ ->
                Error "body-less target"

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
                    // Any translation failure drops the whole entry — better to
                    // under-stamp the trait than to mis-stamp it.
                    let mutable failed = false
                    let argFts = ResizeArray<FrozenType>(argTys.Length)

                    for t in argTys do
                        if not failed then
                            match translate t with
                            | Error _ -> failed <- true
                            | Ok ft -> argFts.Add ft

                    if failed then
                        None
                    else
                        match translate retTy with
                        | Error _ -> None
                        | Ok retFt ->
                            Some(
                                ExternalConstraint.MemberTrait(
                                    EqArray.ofResizeArray indexBuf,
                                    memberName,
                                    argFts.ToArray(),
                                    retFt
                                )
                            )
            | RawConstraint.Default(n, target) ->
                match typars.TryIndexOf n with
                | ValueNone -> None
                | ValueSome i ->
                    match translate target with
                    | Error _ -> None
                    | Ok ft -> Some(ExternalConstraint.Default(i, ft))
            | RawConstraint.Coercion(n, target) ->
                match typars.TryIndexOf n with
                | ValueNone -> None
                | ValueSome i ->
                    match translate target with
                    | Error _ -> None
                    | Ok ft -> Some(ExternalConstraint.Coercion(i, ft))
        )

    /// Finalize one stashed `val`: translate its signature CST to a `FrozenType`
    /// template, resolve its `when` clauses, build the complete `ExternalSymbol`, and
    /// register it (plus, for a `ModuleSuffix` module, the source-name alias). Runs in
    /// the finalize pass, so the registry is complete and a forward-referenced type
    /// resolves. A signature that names a body-less (`Opaque`) type — `mkNominal`
    /// raises `BodylessExternalShape` — or fails structurally is dropped to
    /// `ctx.Skipped` rather than minting a placeholder or aborting the build (the old
    /// closure path deferred that raise to use time; here it's a per-val skip).
    let private finalizeVal (ctx: ExtractCtx) (dv: DeferredVal) : unit =
        let dc = dv.Ctx
        let constraints = ConstraintCollector()

        let translated =
            try
                translateCurriedSig ctx dc.Lexed dc.Input dc.Opens dc.Typars constraints dv.Signature
            with BodylessExternalShape compiled ->
                Error(sprintf "signature names body-less type '%s'" compiled)

        match translated with
        | Error e ->
            // Skip so unresolved names don't masquerade as opaque TyConsts.
            // Per-val skips go to `Skipped`, not `Diagnostics`.
            ctx.Skipped.Add(dv.File, sprintf "%s: %s" dv.Compiled e)
        | Ok template ->
            let resolved =
                resolveConstraints ctx dc.Lexed dc.Input dc.Opens dc.Typars (constraints.Snapshot())

            // Snapshot the typar count AFTER `resolveConstraints`, not before: a
            // `when`-clause-only typar — a PHANTOM typar present in no parameter/
            // result, only inside a coercion target (`fold`'s enumerator `'E` /
            // element `'T` in `'S :> IStructSeq<'T,'E>`) — is interned into the
            // collector while that target is translated, which happens inside
            // `resolveConstraints`. Reading the count beforehand under-counted the
            // symbol's typars: the phantom's constraint index then overran the
            // `typarCount`-sized fresh-TyVar array (`IndexOutOfRange` at
            // `Instantiate`). Counting after makes the extracted symbol carry every
            // generalised typar — the contract analogue of the impl-side
            // `mkMethodQuantEnv` dependent-typar pass — and satisfies
            // `makeInstantiate`'s invariant that every constraint index lands in
            // `freshTvs` (its `.Count`).
            let typarCount = dc.Typars.Count

            // Capture the SOURCE arity for a module FUNCTION. The `.fsi`'s
            // `CurriedSig`/`ArgsSpec` already encodes the grouping the bare curried
            // `template` loses (a tupled group `a * b ->` is one `ArgsSpec` of width
            // 2; a single tuple param `(a*b) ->` is width 1), so each group's
            // `*`-separated width is the arity. The matching parameter type comes from
            // peeling exactly one `FTFun` per group off the template (peeling no
            // further, so a function-typed RESULT stays whole). The flat compiled form
            // is derived from this `ValRepr` on demand (`TastLower.compiledOf`) at the
            // codegen boundary — it is never stored, since it is fully determined by
            // the `ValRepr`. A `val` with no groups is a value, not a function — left
            // `ValueNone`.
            let valReprOpt =
                let (CurriedSig(argGroups, _)) = dv.Signature

                let arities =
                    [
                        for i in 0 .. argGroups.Length - 1 ->
                            let struct (ArgsSpec(specs, _), _) = argGroups.[i]
                            specs.Length
                    ]

                let paramTys, resultTy = TastLower.peelArrowDomains arities.Length template

                if List.isEmpty arities || List.length paramTys <> List.length arities then
                    ValueNone
                else
                    ValueSome(TastLower.externalValRepr typarCount (List.zip arities paramTys) resultTy)

            let sym: ExternalSymbol =
                // FrozenType-scheme migration (step 4): build via the data builder. The
                // relocated `ExternalSymbols.schemeInstantiate` derives the same
                // `Instantiate` closure `makeInstantiate` did (identical constraint-group
                // order), so behaviour is unchanged; only `ValRepr` is overlaid (the
                // builder defaults it to `ValueNone`).
                { ExternalSymbols.scheme dv.Compiled template typarCount resolved with
                    ValRepr = valReprOpt
                }

            ctx.Symbols.[dv.Compiled] <- sym

            // Source-name alias for a `ModuleSuffix` module's members
            // (`List.fold` alongside the compiled `ListModule.fold`); only when
            // it differs and nothing already claims it (first registration wins).
            // The alias keeps the SAME compiled-name key — both forms denote one
            // symbol identity; `stack`'s stampSymbol re-mints with the wrapping
            // package's assembly, so this default never leaks past `wrap`.
            match dv.Source with
            | ValueSome source when source <> dv.Compiled && not (ctx.Symbols.ContainsKey source) ->
                ctx.Symbols.[source] <-
                    { sym with
                        Name = source
                        Key = SymbolKeyOps.valueKeyOf None dv.Compiled
                    }
            | _ -> ()

    // A member's `argSig` is rendered by the SHARED spelling grammar
    // (`ExternalSymbols.argSigOfParameters`) — one renderer for every
    // `FrozenType`-shaped producer, so this contract layer and the TS-manifest
    // provider cannot drift on overload identity. The contract layer once left
    // `argSig` empty, which made `argSig.Length` (the codegen `ExternalMember`
    // arg count + the front-end optional-fill `fullCount`) read 0 for every
    // parameter'd member — so a `sink.Text(s)` call pushed no argument and
    // underflowed the stack; it is rebuilt from the frozen signature in
    // `finalizeDeferred` once the parameter type is known.

    /// Fold a method/ctor's frozen parameter types into the single `.NET`-tupled
    /// `Parameters` form an `ExternalSignature` carries: none ⇒ `unit`, one ⇒ itself,
    /// several ⇒ a tuple. The inverse of `argSigOfParameters` / the metadata layer's
    /// `frozenParams` (`MetadataSymbols`), here so the contract extractor can freeze a
    /// `new: … -> T` constructor without reaching across the codegen-layer boundary.
    let private frozenParamsOf (ps: FrozenType[]) : FrozenType =
        match ps.Length with
        | 0 -> FTConst("unit", EqArray.empty)
        | 1 -> ps.[0]
        | _ -> FTTuple(EqArray.ofArray ps)

    /// Freeze the deferred body / member / val CSTs stashed during extraction into
    /// `FrozenType` templates, in place, once the registry is complete.
    /// A body / val may forward-reference a type
    /// declared later in the package, so this can only run after every shape is
    /// registered. `VesperLib.ExtractCtx.toProvider` runs it before lifting the
    /// context to a provider. Order matters: type-shape bodies first (an
    /// abbreviation referenced by a later body / member / val expands against its
    /// finalized entry), then members, then vals. The shape iteration mutates
    /// `ctx.TypeShapes` in place.
    let finalizeDeferred (ctx: ExtractCtx) : unit =
        let shapeKeys = ctx.TypeShapes.Keys |> Seq.toArray

        for k in shapeKeys do
            let shape = ctx.TypeShapes.[k]

            let finalized =
                match shape, ctx.DeferredBodies.TryGetValue k with
                | ExternalTypeShape.Record(arity, fields, origin), (true, DeferredBody.Record(dc, csts)) ->
                    let fields' =
                        fields
                        |> Array.mapi (fun i f ->
                            { f with
                                Frozen = freezeBodyType ctx dc csts.[i]
                            }
                        )

                    ExternalTypeShape.Record(arity, fields', origin)
                | ExternalTypeShape.Union(arity, cases, _, origin), (true, DeferredBody.Union(dc, caseCsts, ifaceCsts)) ->
                    let cases' =
                        cases
                        |> Array.mapi (fun i c ->
                            { c with
                                FrozenFieldTypes = caseCsts.[i] |> Array.map (freezeBodyType ctx dc)
                            }
                        )

                    // Freeze the union's `interface <ty>` impls so `tryForInEnumerator`
                    // can match a bare cons-list's `interface seq<'T>` against the
                    // enumerable capability (the `Class` arm's `FrozenInterfaces` step).
                    ExternalTypeShape.Union(arity, cases', freezeInterfaces ctx dc ifaceCsts, origin)
                | ExternalTypeShape.Abbrev(arity, _), (true, DeferredBody.Abbrev(dc, rhs)) ->
                    ExternalTypeShape.Abbrev(arity, freezeBodyType ctx dc rhs)
                // A class's deferred `inherit <type>` base + `interface <type>` impls,
                // frozen now the registry is complete (either may forward-reference a
                // sibling). The members ride the separate `DeferredMembers` channel (loop
                // below); this loop fills `FrozenBaseType` (read by a consumer's subtype
                // walk `Engine.subtypeParentOf` → `instantiateBaseType` to reconcile
                // through the contract inherit chain — the Step-8 JS exception hierarchy)
                // and `FrozenInterfaces` (read by the interface-impl witness
                // `tryInterfaceWitness`' external arm to recover a phantom typar from a
                // struct seq's `IStructSeq<'T,'E>` impl).
                | ExternalTypeShape.Class shape, (true, DeferredBody.Class(dc, baseOpt, ifaces, _)) ->
                    ExternalTypeShape.Class
                        { shape with
                            FrozenBaseType = baseOpt |> ValueOption.map (freezeBodyType ctx dc)
                            FrozenInterfaces = freezeInterfaces ctx dc ifaces
                        }
                | _ -> shape

            ctx.TypeShapes.[k] <- finalized

        // Snapshot the keys: a member list is rebuilt (dropped members removed) and
        // written back, so we can't enumerate the dictionary while mutating it.
        let memberKeys = ctx.TypeMembers.Keys |> Seq.toArray

        for key in memberKeys do
            match ctx.DeferredMembers.TryGetValue key with
            | true, deferred ->
                let members = ctx.TypeMembers.[key]
                let kept = ResizeArray<ExternalMember>(members.Count)

                for i in 0 .. members.Count - 1 do
                    let m = members.[i]
                    let s = m.Signature

                    match freezeMemberSig ctx m.IsValueMember s.DeclaringArity deferred.[i] with
                    | ValueSome sign ->
                        // Rebuild the member key's `argSig` from the now-frozen parameters
                        // (extraction stamped it empty — the signature was still deferred).
                        // A property has no parameters, so its key stays empty-`argSig`
                        // (matching the metadata layer). `MethodArity` is likewise only
                        // known post-freeze (the member's own typars surface during the
                        // signature walk), so it is propagated off the frozen signature
                        // here, overwriting the extraction-time `0` placeholder.
                        let m' =
                            if m.IsValueMember then
                                { m with
                                    Signature = sign
                                    MethodArity = sign.MethodArity
                                }
                            else
                                let key' =
                                    match m.Key with
                                    | SymbolKey.MemberKey(decl, name, _, kind) ->
                                        SymbolKey.MemberKey(
                                            decl,
                                            name,
                                            ExternalSymbols.argSigOfParameters sign.Parameters,
                                            kind
                                        )
                                    | other -> other

                                { m with
                                    Signature = sign
                                    Key = key'
                                    MethodArity = sign.MethodArity
                                }

                        kept.Add m'
                    | ValueNone -> ()

                ctx.TypeMembers.[key] <- kept
            | _ -> ()

        // A contract INTERFACE carries its (now-finalized) members in the shape too.
        // A nominal class serves its members only through `TryLookupMember` (the
        // shape stays the bodiless `basic`), but `interface … with` conformance
        // (`Unification.checkInterfaceConformance`) reads `shape.Members` directly —
        // matching the metadata layer, where an interface's shape carries its
        // members. Without this an `IFormatSink` impl in a package build saw zero
        // required members and reported every implemented member as undefined.
        let interfaceShapeKeys = ctx.TypeShapes.Keys |> Seq.toArray

        for k in interfaceShapeKeys do
            match ctx.TypeShapes.[k] with
            | ExternalTypeShape.Class shape when shape.IsInterface && Array.isEmpty shape.Members ->
                match ctx.TypeMembers.TryGetValue k with
                | true, members when members.Count > 0 ->
                    ctx.TypeShapes.[k] <-
                        ExternalTypeShape.Class
                            { shape with
                                Members = members.ToArray()
                            }
                | _ -> ()
            | _ -> ()

        // Constructors: a class's deferred `new: … -> T` sigs freeze into `.ctor`
        // members (the shape kind a `constructor-as-function` application — `inferExternalCtorOn`
        // probes `TryLookupMembers(type, ".ctor")`). Runs AFTER the member loop with the
        // signature frozen directly (no `DeferredMembers` slot), so it neither re-indexes
        // that loop nor is reprocessed. The `.ctor` member mirrors the metadata layer's
        // shape (`MetadataSymbols`): `Name = ".ctor"`, instance, `MemberKind.Method`,
        // `Parameters`/`Return` from the sig. This is what lets a BCL-free provider type
        // `InvalidOperationException "msg"` without host metadata.
        for k in shapeKeys do
            match ctx.DeferredBodies.TryGetValue k with
            | true, DeferredBody.Class(dc, _, _, ctors) when not (List.isEmpty ctors) ->
                let arity =
                    match ctx.TypeShapes.TryGetValue k with
                    | true, ExternalTypeShape.Class shape -> shape.Arity
                    | _ -> 0

                let declKey = SymbolKeyOps.qualifiedTypeKeyOf None k arity

                let ctorMembers =
                    [
                        for (paramCsts, retCst) in ctors do
                            let parameters = paramCsts |> Array.map (freezeBodyType ctx dc) |> frozenParamsOf
                            let ret = freezeBodyType ctx dc retCst

                            ExternalMember.ctor
                                declKey
                                (ExternalSignature.make (arity, 0, parameters, ret))
                                (ExternalSymbols.argSigOfParameters parameters)
                                SymbolOrigin.Empty
                                []
                    ]

                let merged =
                    match ctx.TypeMembers.TryGetValue k with
                    | true, existing ->
                        let r = ResizeArray<ExternalMember>(existing)
                        r.AddRange ctorMembers
                        r
                    | _ -> ResizeArray<ExternalMember>(ctorMembers)

                ctx.TypeMembers.[k] <- merged
            | _ -> ()

        // Vals last: a val signature / constraint target may name an abbreviation,
        // record, or union whose template the loops above just filled. Source order
        // is preserved so the `ModuleSuffix` source-name alias stays first-wins.
        for dv in ctx.DeferredVals do
            finalizeVal ctx dv

    module ExtractCtx =
        let empty = VesperLibTyparCapture.ExtractCtx.empty

        /// Finalize the deferred CST bodies / members into `FrozenType` templates
        /// (`finalizeDeferred`), then lift the context to an
        /// `IExternalSymbolProvider`. The two-phase split exists only because the
        /// `CST → FrozenType` translation lives a compile unit later than
        /// `VesperLibTyparCapture.ExtractCtx.toProvider`.
        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            finalizeDeferred ctx
            VesperLibTyparCapture.ExtractCtx.toProvider ctx

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

    /// Build the *source*-qualified name for a val — the name the front end
    /// actually writes (`Set.empty`). Unlike `compiledNameForVal` it IGNORES
    /// `[<CompiledName(_)>]` (which renames the member at the IL boundary, e.g.
    /// `empty` ⇒ `Empty`): a consumer writes `Set.empty`, never `Set.Empty`, so
    /// the source-alias key must carry the written ident, only the module path
    /// being source-rewritten (`SetModule` ⇒ `Set`). The compiled-name entry
    /// (which respects `[<CompiledName>]`) stays the symbol's identity key.
    let private sourceNameForVal
        (lexed: Lexed)
        (input: string)
        (path: string list)
        (ident: IdentOrOp<SyntaxToken>)
        : string voption =
        match identOrOpName lexed input ident with
        | ValueNone -> ValueNone
        | ValueSome n ->
            let qualifier = String.concat "." (List.rev path)

            if qualifier.Length = 0 then
                ValueSome n
            else
                ValueSome(qualifier + "." + n)

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
                // Stash only: the signature is translated, its constraints resolved,
                // and the `ExternalSymbol` built in `finalizeDeferred`, once the
                // registry is complete (the signature / a constraint target may
                // forward-reference a type declared later in the package). The
                // collector is seeded with the val's explicit `<'T>` typars here so
                // the finalize walk reads the same indices for them; it interns the
                // remaining body typars and captures the `when` clauses.
                let collector = TyparCollector()
                registerExplicitTypars lexed input collector typars

                ctx.DeferredVals.Add
                    {
                        Ctx =
                            {
                                Lexed = lexed
                                Input = input
                                Opens = opens
                                Typars = collector
                            }
                        Compiled = compiled
                        Source = sourceNameForVal lexed input sourcePath ident
                        File = file
                        Signature = signature
                    }

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

                let baseName =
                    if qualifier.Length = 0 then
                        short
                    else
                        qualifier + "." + short

                // Arity-suffix generic types (`Vesper.Choice`2`) so an arity-
                // overloaded type doesn't collapse onto its bare compiled name in
                // `ctx.TypeShapes` / the reverse case index. This matches the emitted
                // metadata name (`TypeRegistry.keyFor`) and the consumer's arity-
                // suffixed `keysFor` probe in `tryResolveExternalType`. Non-generic
                // types keep their bare name.
                let compiled = SymbolKeyOps.arityName baseName arity

                // First declaration wins on a *short-name* collision; arity-overloaded
                // types share the short name, so only the first arity is reachable by
                // bare short name (the consumer resolves the rest by arity-key).
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
        // Full-defer: register the abbreviation shape (placeholder template) and
        // stash the RHS CST; the `toProvider` finalize pass translates it once the
        // registry is complete (the RHS may forward-reference a type declared later
        // in the package). The collector is seeded with the type's `<'T>` typars so
        // the finalize walk reads the same declaring indices; an RHS typar beyond
        // the declared arity degrades that body to `FTUnknown` in `freezeBodyType`.
        let collector = collectorForTypeName lexed input typeName
        ctx.TypeShapes.[compiled] <- ExternalTypeShape.Abbrev(arity, deferredTemplate)

        ctx.DeferredBodies.[compiled] <-
            DeferredBody.Abbrev(
                {
                    Lexed = lexed
                    Input = input
                    Opens = opens
                    Typars = collector
                },
                rhs
            )

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
        // Full-defer: field names + mutability come straight from the CST; the
        // field-type CSTs are stashed for the finalize pass to translate. A field
        // type that's unsupported / body-less / over-arity degrades to `FTUnknown`
        // per-field in `freezeBodyType` rather than downgrading the whole record.
        let collector = collectorForTypeName lexed input typeName
        let shapes = ResizeArray<ExternalFieldShape>(fields.Length)
        // The per-field type CSTs, index-aligned with `shapes`.
        let csts = ResizeArray<Type<SyntaxToken>>(fields.Length)

        for i in 0 .. fields.Length - 1 do
            let (RecordField(_, mutableTok, _, identTok, _, fieldTy)) = fields.[i]
            shapes.Add(ExternalFieldShape.create (nameOfTok lexed input identTok, mutableTok.IsSome))
            csts.Add fieldTy

        // `Origin` is filled later by `ReferencedProject.wrap` (which knows the
        // package's assembly + namespace from the manifest); the extractor records
        // `Empty`.
        ctx.TypeShapes.[compiled] <- ExternalTypeShape.Record(arity, shapes.ToArray(), SymbolOrigin.Empty)

        ctx.DeferredBodies.[compiled] <-
            DeferredBody.Record(
                {
                    Lexed = lexed
                    Input = input
                    Opens = opens
                    Typars = collector
                },
                csts.ToArray()
            )

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
        (interfaces: Type<SyntaxToken> list)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let caseShapes = ResizeArray<ExternalCaseShape>(cases.Length)
        // The per-case field-type CSTs (one array per case, index-aligned with
        // `caseShapes`); stashed in `ctx.DeferredBodies` for the finalize pass.
        let caseCsts = ResizeArray<Type<SyntaxToken>[]>(cases.Length)
        let mutable err = None

        // Operator-named cases (the cons-list's `([])` / `(::)`) take their
        // canonical *ctor* names `Empty`/`Cons` — the names `FreezeExpr` mints and
        // codegen (both the CLR recipes and the JS external-union path) resolves
        // through — via the shared `OperatorNames.unionCaseCtorName`, so the
        // extracted contract's case names match a locally-compiled union's exactly.
        // The reverse bare-ctor-name index (`TyparCapture.toProvider`) excludes the
        // cons-list's cases, so `Empty`/`Cons` here cannot shadow a user union's
        // ctor in bare-name resolution (the role the old `op_Nil`/`op_ColonColon`
        // form served).
        let caseName (ioo: IdentOrOp<SyntaxToken>) : string voption =
            OperatorNames.unionCaseCtorName (nameOfTok lexed input) ioo

        for i in 0 .. cases.Length - 1 do
            if err.IsNone then
                let (UnionTypeCase(_, data)) = cases.[i]

                match data with
                | UnionTypeCaseData.Nullary ident ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add(ExternalCaseShape.create (n, [||]))
                        caseCsts.Add [||]

                | UnionTypeCaseData.Nary(ident, _, fields, _) ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(fields.Length)
                        let fieldCsts = ResizeArray<Type<SyntaxToken>>(fields.Length)

                        for j in 0 .. fields.Length - 1 do
                            let nameOpt, fieldTy =
                                match fields.[j] with
                                | UnionTypeField.Unnamed t -> ValueNone, t
                                | UnionTypeField.Named(identTok, _, t) -> ValueSome(nameOfTok lexed input identTok), t

                            names.Add nameOpt
                            fieldCsts.Add fieldTy

                        caseShapes.Add(ExternalCaseShape.create (n, names.ToArray()))
                        caseCsts.Add(fieldCsts.ToArray())

                | UnionTypeCaseData.GadtNullary(name = ident) ->
                    // GADT-syntax nullary (`([]): 'T list`): the explicit return
                    // type is the declaring union and carries no field, so it
                    // models exactly as an ordinary nullary case. A true
                    // type-refining return type is out of scope — the same stance
                    // the front-end's `TypeRegistration.inspectCaseData` takes.
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add(ExternalCaseShape.create (n, [||]))
                        caseCsts.Add [||]

                | UnionTypeCaseData.GadtNary(name = ident; sign = UncurriedSig(args = ArgsSpec(specs, _))) ->
                    // GADT-syntax n-ary (`(::): Head: 'T * Tail: 'T list -> 'T list`):
                    // the fields are the signature's args; the return type names the
                    // declaring union and is ignored (true type-refining GADTs are
                    // out of scope, exactly as in the front-end).
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(specs.Length)
                        let fieldCsts = ResizeArray<Type<SyntaxToken>>(specs.Length)

                        for j in 0 .. specs.Length - 1 do
                            let (ArgSpec(_, nameSpec, fieldTy)) = specs.[j]

                            let nameOpt =
                                match nameSpec with
                                | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(nameOfTok lexed input id)
                                | ValueNone -> ValueNone

                            names.Add nameOpt
                            fieldCsts.Add fieldTy

                        caseShapes.Add(ExternalCaseShape.create (n, names.ToArray()))
                        caseCsts.Add(fieldCsts.ToArray())

        // A structurally-broken case (an unresolvable case name) still downgrades
        // the whole union to `Opaque` — there's no per-case name to register and
        // `TryLookupUnionCase` must stay total. Field *types*, by contrast, defer
        // and degrade per-field in the finalize pass.
        match err with
        | Some e -> skipBodyOpaque ctx file compiled arity e
        | None ->
            // A `[<RequireQualifiedAccess>]` union's cases are NOT brought into scope
            // bare; record it so the reverse case-name index marks its cases, and the
            // consumer's bare-name resolution rejects them.
            let (TypeName(attrs, _, _, _, _, _)) = typeName

            if isRequireQualifiedAccess lexed input attrs then
                ctx.RqaTypes.Add compiled |> ignore

            // `Origin` is filled later by `ReferencedProject.wrap` (which knows the
            // package's assembly + namespace from the manifest); the extractor
            // records `Empty`.
            // `interfaces` start empty; the finalize pass freezes the deferred
            // `interface <ty>` CSTs into them (forward-references resolve once the
            // registry is complete, exactly as the `Class` arm does).
            ctx.TypeShapes.[compiled] <- ExternalTypeShape.Union(arity, caseShapes.ToArray(), [||], SymbolOrigin.Empty)

            ctx.DeferredBodies.[compiled] <-
                DeferredBody.Union(
                    {
                        Lexed = lexed
                        Input = input
                        Opens = opens
                        Typars = collector
                    },
                    caseCsts.ToArray(),
                    interfaces
                )

    /// Extract the augmentation `member`s declared inside a type body's
    /// `with`-block (`member Value: 'T` / `member IsSome: bool` on `Option`)
    /// into `ctx.TypeMembers`, keyed by the type's qualified compiled name.
    /// Each member's signature is translated over the *type's* typar collector
    /// (so `'T` substitutes through the enclosing type's args at a use site); the
    /// builder is stashed in `ctx.DeferredMembers` and the finalize pass freezes it
    /// into the member's `ExternalMember.Signature` template the consumer's
    /// `resolveFieldStep` instantiates. Scope: instance/static `member` property/method sigs,
    /// generic or not — a member's own typars (explicit `<'a>` or an implicit `'T`) are
    /// published on the method axis (Step 6); a member whose signature fails to translate
    /// is skipped (not faked), exactly like the val path.
    let private extractTypeMembers
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        if elems.Length = 0 then
            ()
        else
            // Split the qualified compiled name into the declaring `TypeKey`
            // (ns, simple name) so each member carries a best-effort identity;
            // the asm slot is stamped later by the wrapping source. The same split
            // the finalize-pass ctor loop applies — both route through the shared
            // `qualifiedTypeKeyOf` so the `.ctor` and ordinary members of a type
            // carry the identical declaring key.
            let declKey = SymbolKeyOps.qualifiedTypeKeyOf None compiled arity

            let members = ResizeArray<ExternalMember>()
            // The per-member signature CSTs, index-aligned with `members`;
            // stashed in `ctx.DeferredMembers` for the finalize pass to translate.
            let memberCsts = ResizeArray<DeferredMember>()

            for i in 0 .. elems.Length - 1 do
                let memberSig =
                    match elems.[i] with
                    | TypeSignatureElement.Member(signature = s) -> ValueSome(false, s)
                    // An `abstract member` slot (the canonical interface-member form,
                    // `type IFormatSink = abstract member Text: …`). The parser keeps
                    // it as a distinct `Abstract` element; published as an ordinary
                    // (non-static) instance member so a consumer's `sink.Text(…)` call
                    // and `interface … with` conformance check resolve against it.
                    | TypeSignatureElement.Abstract(signature = s) -> ValueSome(false, s)
                    | TypeSignatureElement.StaticMember(signature = s) -> ValueSome(true, s)
                    | _ -> ValueNone

                match memberSig with
                | ValueNone -> ()
                | ValueSome(isStatic, sign) ->
                    // Property/method sigs, generic or not: a member's OWN typars
                    // (explicit `<'a>` via `typarDefns`, or an implicit `'T` the
                    // signature names) are published on the method axis (Step 6) —
                    // the finalize `reaxisMethodTypars` flips any typar the member
                    // interns beyond the declaring type's own onto `FTTypar(Method,_)`.
                    let identAndSig =
                        match sign with
                        | MemberSig.MethodOrPropSig(ident = ioo; typarDefns = defns; sign = csig) ->
                            ValueSome(ioo, defns, csig, false)
                        | MemberSig.PropSig(ident = ioo; typarDefns = defns; sign = csig) ->
                            ValueSome(ioo, defns, csig, true)

                    match identAndSig with
                    | ValueNone -> ()
                    | ValueSome(ioo, defns, csig, isPropSig) ->
                        match identOrOpName lexed input ioo with
                        | ValueNone -> ()
                        | ValueSome memberName ->
                            // Full-defer: stash the member + its signature CST. The
                            // finalize pass (`freezeMemberSig`) translates the
                            // signature and DROPS the member only if it fails to
                            // translate structurally. The collector is seeded with the
                            // type's own typars (so the finalize walk reads the same
                            // declaring indices) and THEN the member's explicit `<'a>`
                            // typars, so they take method-axis positions (indices
                            // `>= arity`) in declared order ahead of any implicit
                            // appearance-order typar — the producer's declared-first
                            // method-typar order (`GeneralizedTypars.canonical`).
                            let collector = collectorForTypeName lexed input typeName
                            registerExplicitTypars lexed input collector defns
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
                                    Storage =
                                        if isProperty then
                                            MemberStorage.Property
                                        else
                                            MemberStorage.Method
                                    // Deferred: the signature CST is stashed in
                                    // `ctx.DeferredMembers` and frozen by the `toProvider`
                                    // finalize pass once the registry is complete (a sig may
                                    // forward-reference a type declared later). `deferred`
                                    // records the arities the finalize pass needs.
                                    Signature = ExternalSignature.deferred (arity, 0)
                                    // Placeholder: the real method-typar count is only
                                    // known once the signature is walked, so the finalize
                                    // pass (`freezeMemberSig`) overwrites this off the
                                    // frozen signature's `MethodArity`.
                                    MethodArity = 0
                                    Origin = SymbolOrigin.Empty
                                    Key = SymbolKey.MemberKey(declKey, memberName, EqArray.empty, kind)
                                    // The `.fsi` contract layer doesn't publish optional-parameter
                                    // defaults yet.
                                    OptionalDefaults = []
                                    IsOptional = false
                                }

                            memberCsts.Add
                                {
                                    Ctx =
                                        {
                                            Lexed = lexed
                                            Input = input
                                            Opens = opens
                                            Typars = collector
                                        }
                                    Signature = csig
                                }

            if members.Count > 0 then
                ctx.TypeMembers.[compiled] <- members
                ctx.DeferredMembers.[compiled] <- memberCsts

    /// F# infers an interface from a bodied type whose members are *all* abstract
    /// (`type IFormatSink = abstract member …`, no `interface`/`class`/`begin`
    /// keyword) — the parser hands this to us as `TypeSignature.Anon`/`Class`, so
    /// the interface-ness can't be read off the keyword. A body with ≥1 abstract
    /// member and no concrete member / ctor / field / inherit IS such an interface;
    /// publish it as one so a consumer's `interface … with` impl resolves
    /// (`resolveInterfaceImpls` rejects a non-interface shape, dropping the impl).
    let private bodyIsInterface (elems: TypeElementsSignature<SyntaxToken>) : bool =
        let mutable hasAbstract = false
        let mutable hasConcrete = false

        for e in elems do
            match e with
            | TypeSignatureElement.Abstract _ -> hasAbstract <- true
            | TypeSignatureElement.Member _
            | TypeSignatureElement.StaticMember _
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Inherit _
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _ -> hasConcrete <- true
            | _ -> ()

        hasAbstract && not hasConcrete

    /// Register a class-shaped type from a member body: the bodiless `basic`
    /// `Class` shape plus its deferred inherit/interface/ctor body and extracted
    /// members. Shared by the `Anon`/`Class` arm and the `extern with …`
    /// capability-surface arm (a non-intrinsic `extern` type whose `with` body
    /// publishes interfaces + members exactly as a bodied class does).
    let private extractBodiedClassLike
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (elements: TypeElementsSignature<SyntaxToken>)
        : unit =
        // `type X = abstract member …` parses as an implicit-body `Anon`/`Class`
        // (no `interface` keyword), but an all-abstract body IS an interface.
        let isInterface = bodyIsInterface elements

        // A `[<Struct>]`-attributed class-shaped type (the struct-seq
        // `ArraySeq`/`MapSeq`/… nodes) is a value type — publish the flag so a
        // consumer encodes it `ELEMENT_TYPE_VALUETYPE`, mirroring the
        // `struct … end` arm above and the metadata layer's `Type.IsValueType`.
        let isValueType = typeNameHasStructAttr lexed input typeName

        ctx.TypeShapes.[compiled] <-
            ExternalTypeShape.Class(
                { ExternalClassShape.basic (arity, isInterface, SymbolOrigin.Empty) with
                    Flags =
                        { ExternalClassFlags.Default with
                            IsValueType = isValueType
                        }
                }
            )

        // An `inherit <type>` clause, any directly-declared `interface <type>`
        // impls, and any `new: … -> T` constructors are deferred (like the body
        // templates) so a base / interface / ctor type that forward-references a
        // sibling resolves once the registry is complete; the finalize pass
        // freezes the base into `FrozenBaseType`, each interface into
        // `FrozenInterfaces`, and each ctor into a `.ctor` member. Only a class
        // that declares one of them registers a deferred body — the
        // previously-empty common case is untouched.
        let inheritBase =
            elements
            |> Seq.tryPick (fun e ->
                match e with
                | TypeSignatureElement.Inherit(ClassInheritsDecl(typ = t)) -> Some t
                | _ -> None
            )

        let interfaces =
            [
                for e in elements do
                    match e with
                    | TypeSignatureElement.Interface(InterfaceSpec(typ = t)) -> t
                    | _ -> ()
            ]

        let ctors =
            [
                for e in elements do
                    match e with
                    | TypeSignatureElement.Constructor(signature = UncurriedSig(ArgsSpec(args, _), _, retTy)) ->
                        let paramTys = [| for ArgSpec(typ = t) in args -> t |]
                        (paramTys, retTy)
                    | _ -> ()
            ]

        match inheritBase, interfaces, ctors with
        | None, [], [] -> ()
        | _ ->
            let collector = collectorForTypeName lexed input typeName

            ctx.DeferredBodies.[compiled] <-
                DeferredBody.Class(
                    {
                        Lexed = lexed
                        Input = input
                        Opens = opens
                        Typars = collector
                    },
                    (match inheritBase with
                     | Some t -> ValueSome t
                     | None -> ValueNone),
                    interfaces,
                    ctors
                )

        extractTypeMembers ctx lexed input opens compiled arity typeName elements

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
                // A union's trailing `with interface <ty> with …` impls ride the
                // shared extension-elements list (no union-specific parser field);
                // capture the `interface <ty>` head types so the finalize pass can
                // freeze them into the union shape's `interfaces` (the `Class` arm's
                // `interfaces` capture, mirrored). The impl bodies (the `member`
                // elements) still flow through `extractTypeMembers`.
                let interfaceCsts =
                    match extensions with
                    | ValueSome(TypeExtensionElementsSignature(_, elems, _)) ->
                        [
                            for e in elems do
                                match e with
                                | TypeSignatureElement.Interface(InterfaceSpec(typ = t)) -> t
                                | _ -> ()
                        ]
                    | ValueNone -> []

                extractUnionBody ctx file lexed input opens compiled arity typeName cases interfaceCsts

                match extensions with
                | ValueSome(TypeExtensionElementsSignature(_, elems, _)) ->
                    extractTypeMembers ctx lexed input opens compiled arity typeName elems
                | ValueNone -> ()

        | TypeSignature.Interface(typeName = typeName) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                // A class/interface shape carries no body (its members are
                // resolved separately via `TryLookupMember`), but registering it
                // here is what makes a nominal external type — `Vesper.Fun`,
                // `EqualityComparer<_>` — resolve through `TryLookupType` instead
                // of `ValueNone`. `Origin` is stamped
                // by the resolving source (e.g. `ReferencedProject`), not here.
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Class(ExternalClassShape.basic (arity, true, SymbolOrigin.Empty))

        | TypeSignature.Extern(typeName = typeName; members = members) ->
            // An `extern` type is either an intrinsic-repr primitive (its sibling
            // `.fs` carries `type x = (# "<repr>" #)`, harvested into
            // `ctx.IntrinsicReprs` before extraction) or an opaque abstract type
            // / real class with no `.fs` binding. The former publishes as a
            // NON-transparent `Intrinsic repr` (a use site resolves to the
            // nominal `TyConst name`, the repr feeding codegen / `subsumes`); the
            // latter falls through to `Class` exactly as a non-extern nominal
            // does.
            //
            // A trailing `with interface … / member …` on a NON-intrinsic extern
            // publishes a capability surface: it registers exactly as a bodied
            // class/interface (`extractBodiedClassLike`), so a consumer's
            // structural probe sees the members and `FrozenInterfaces`.
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                let short = shortNameOfTypeName lexed input typeName
                let isIntrinsic = ctx.IntrinsicBaseReprs.ContainsKey short

                // `short` (the `.fsi` name) is the platform-invariant `canon` face;
                // intrinsic-ness is decided by the BASE `.fs` companion
                // (`IntrinsicBaseReprs`), so a target that omits a primitive's repr still
                // publishes it as an `Intrinsic` with `platform = None`. The `platform`
                // face is the compiling target's `(# … #)` repr (`IntrinsicReprs`).
                // `arity` rides along (the structural constructors `'T []`/`byref` are
                // intrinsics of arity ≥ 1); `PlatformTypes` treats `platform = None` as
                // fatal only when `arity = 0`.
                let registerIntrinsic () =
                    let platform =
                        match ctx.IntrinsicReprs.TryGetValue short with
                        | true, repr -> Some repr
                        | _ -> None

                    ctx.TypeShapes.[compiled] <- ExternalTypeShape.Intrinsic(short, arity, platform)

                match members with
                | ValueSome(TypeExtensionElementsSignature(_, elems, _)) when not (Seq.isEmpty elems) ->
                    // A trailing `with interface … / member …` publishes a capability
                    // surface: register as a bodied class/interface (`extractBodiedClassLike`)
                    // so a consumer's structural probe sees the members and `FrozenInterfaces`.
                    // This admits BOTH a capability INTERFACE (`abstract member …`) and a
                    // CONCRETE member surface (`member Poke: int -> int`) on an intrinsic — a
                    // concrete `(# … #)`-bound member is a general platform-binding capability
                    // (the member's body is served as a member-keyed inline splice), not a
                    // structural mutation of the primitive: the `ExternalTypeShape.Intrinsic`
                    // shape carries no member slots, so the dual-faced Class is the sole shape
                    // able to publish them while the `CapabilityFace` keeps the canonical
                    // primitive identity for `subsumes` / codegen.
                    extractBodiedClassLike ctx lexed input opens compiled arity typeName elems

                    // For an intrinsic, attach the platform face from the repr so the Class
                    // reconciles to its platform spelling exactly as an `Intrinsic` does. A
                    // target whose `.fs` omits the repr (JS) leaves `CapabilityFace = ValueNone`,
                    // so the canonical identity stands. A non-intrinsic `extern` stays a plain
                    // Class (no face).
                    if isIntrinsic then
                        match ctx.IntrinsicReprs.TryGetValue short with
                        | true, platform ->
                            match ctx.TypeShapes.TryGetValue compiled with
                            | true, ExternalTypeShape.Class shape ->
                                ctx.TypeShapes.[compiled] <-
                                    ExternalTypeShape.Class
                                        { shape with
                                            CapabilityFace = ValueSome { Canon = short; Platform = platform }
                                        }
                            | _ -> ()
                        | _ -> ()
                | _ ->
                    // No member body. A primitive/capability anchor (`IntrinsicBaseReprs`)
                    // publishes as `Intrinsic`; an `extern` with no base companion repr is a
                    // real opaque `Class`.
                    if isIntrinsic then
                        registerIntrinsic ()
                    else
                        ctx.TypeShapes.[compiled] <-
                            ExternalTypeShape.Class(ExternalClassShape.basic (arity, false, SymbolOrigin.Empty))

        | TypeSignature.Struct(typeName = typeName) ->
            // A `type X = struct … end` value type. Same nominal `Class` shape as a
            // reference class, but the contract must publish its value-type-ness so a
            // consumer's encoder emits `ELEMENT_TYPE_VALUETYPE` (not `CLASS`) for it —
            // without that flag a referenced-package struct in any signature faults the
            // loader with "value type mismatch". The metadata
            // layer reads the same flag off `Type.IsValueType`; here it rides the
            // syntactic `struct … end` form (a `[<Struct>]`-attributed `Class`/`Anon`
            // would need attribute decode — its canonical surface is this form).
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                let shape =
                    { ExternalClassShape.basic (arity, false, SymbolOrigin.Empty) with
                        Flags =
                            { ExternalClassFlags.Default with
                                IsValueType = true
                            }
                    }

                ctx.TypeShapes.[compiled] <- ExternalTypeShape.Class shape

        | TypeSignature.Anon(typeName = typeName; elements = elements)
        | TypeSignature.Class(typeName = typeName; elements = elements) ->
            // A nominal class with a member body (`Vesper.Set`'s `Set<'T>`). The
            // shape stays the bodiless `basic` `Class` (members are served through
            // `TryLookupMember`, not the shape), but the body's `member` /
            // `static member` sigs ARE extracted so a *consumer* can resolve them —
            // notably the static operators (`op_Addition` / `op_Subtraction`) an
            // SRTP `+` / `-` on the type dispatches to (`drainSrtpBounds`). Without
            // this the class's members were silently dropped (only `Union` bodies
            // extracted members), so `s + t` on an imported `Set` left its result
            // typar unresolved.
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractBodiedClassLike ctx lexed input opens compiled arity typeName elements

        | TypeSignature.AbstractType typeName ->
            // An opaque abstract type (`type T`) with no body shape. Resolve as a
            // non-interface `Class` so codegen can mint a ref off the origin.
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
                // contract's ambient prefix set.
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
        // The dependency providers' ambient prefixes (`Vesper`, …) seed the file's
        // open scope at lowest priority — fileOpens flows to the tail of each
        // descended scope (`ownOpens @ (nsName :: fileOpens)`), so an explicit
        // `open` / the enclosing namespace still wins. This lets a `.fsi` reference
        // a dependency's ambiently-available type (`Fun`2` / `Fun`3` from Core) by
        // bare name, matching the consumer front end's composite resolution.
        let fileOpens = ctx.DependencyAmbientPrefixes

        match parsed.Ast with
        | FSharpAst.SignatureFile sf ->
            match sf with
            | SignatureFile.Namespaces groups ->
                for i in 0 .. groups.Length - 1 do
                    // Each namespace decl group starts a fresh open scope.
                    extractNamespaceGroup ctx parsed.File parsed.Lexed parsed.Input fileOpens groups.[i]
            | SignatureFile.NamedModule nm ->
                extractNamedModuleSig ctx parsed.File parsed.Lexed parsed.Input fileOpens nm
            | SignatureFile.AnonymousModule elems ->
                let opens = collectOpens parsed.Lexed parsed.Input elems @ fileOpens

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

    /// Harvest the intrinsic-representation bindings from a parsed `.fs` companion
    /// (`type exn = (# "System.Exception" #)`) into `dest` (short name ⇒ repr).
    /// This is the `.fs` half of the `.fsi`/`.fs` pairing: the `.fsi` `type exn =
    /// extern` deliberately omits the repr, so the identity lives only here. Run
    /// BEFORE the `.fsi` extraction so the `extern` arm of `extractTypeSig` can
    /// publish `ExternalTypeShape.Intrinsic`. The caller harvests the per-target
    /// `<base>.<target>.fs` ⇒ `IntrinsicReprs` (the `platform` face); the `canon`
    /// face is the `.fsi` name itself, so a target override repoints codegen WITHOUT
    /// moving the unifier's identity key.
    ///
    /// A direct CST scrape — NOT `Pipeline.analyse` — because (a) all we need is
    /// the `type <name> = (# "<repr>" #)` shape, and (b) the prim-types `.fs`
    /// carry cons-list augmentation members that trip unimplemented analysis
    /// paths (`CstKeys.firstTokenOfPat: TODO Cons`). A later binding wins a clash.
    let harvestIntrinsicReprsInto
        (dest: System.Collections.Generic.Dictionary<string, string>)
        (parsed: ParsedFile)
        : unit =
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
                            dest.[nameOf li.Idents.[0]] <- ilIntrinsicReprString parsed.Lexed parsed.Input parts
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
