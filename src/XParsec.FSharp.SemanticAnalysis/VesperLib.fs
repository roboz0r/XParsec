namespace XParsec.FSharp.SemanticAnalysis

open System
open System.IO
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open VesperLibTyparCapture
open VesperLibTypeTranslate

/// Extracts an `IExternalSymbolProvider` from a package's `.fsi` contract files. Files are
/// accumulated one at a time into one `ExtractCtx`, then lifted to a provider.
module VesperLib =

    type LibFile = VesperLibManifest.LibFile
    type ParsedFile = VesperLibManifest.ParsedFile

    let libFile = VesperLibManifest.libFile
    let parseFileFull = VesperLibManifest.parseFileFull

    type ExtractCtx = VesperLibTyparCapture.ExtractCtx

    let private freezeBodyType (ctx: ExtractCtx) (dc: DeferredCtx) (cst: Type<SyntaxToken>) : FrozenType =
        try
            match translateType ctx dc.Lexed dc.Opens dc.Typars (ConstraintCollector()) cst with
            | Ok ft -> ft
            | Error _ -> ExternalSignature.unfreezable
        with BodylessExternalShape _ ->
            ExternalSignature.unfreezable

    /// Freeze `interface <ty>` impl CSTs, with args over the declaring typars as
    /// `FTTypar(Declaring,i)`. A non-nominal freeze carries no witness and is dropped.
    let private freezeInterfaces
        (ctx: ExtractCtx)
        (dc: DeferredCtx)
        (ifaces: Type<SyntaxToken> list)
        : EqArray<FrozenInterface> =
        EqArray.ofSeq
            [
                for t in ifaces do
                    match FrozenInterface.TryOfFrozen(freezeBodyType ctx dc t) with
                    | ValueSome i -> i
                    | ValueNone -> ()
            ]

    /// A `[<Struct>] type X = …` (the ATTRIBUTE form) parses through the Class/Anon arm, not
    /// the `struct … end` form, so its value-type-ness is on the `TypeName`'s attributes.
    let private classFlagsOfTypeName (lexed: Lexed) (typeName: TypeName<SyntaxToken>) : ExternalClassFlags =
        let (TypeName(attributes = attrs)) = typeName
        let decoded = AttributeDecode.decodeClassAttributes (SyntaxToken.nameIn lexed) attrs

        { ExternalClassFlags.Default with
            Declared =
                { DeclaredClassFlags.Default with
                    IsSealed = decoded.IsSealed
                    AllowNullLiteral = decoded.AllowNullLiteral
                }
            IsValueType = decoded.IsValueType
        }

    /// A member's published `ExternalSignature`. `ValueNone` drops the member; a body-less shape
    /// degrades to `unit -> FTUnknown`.
    let private freezeMemberSig
        (ctx: ExtractCtx)
        (declaringTyparArity: int)
        (dm: DeferredMember)
        : ExternalSignature voption =
        let dc = dm.Ctx

        let signatureOf (groupDomains: FrozenType list) (ret: FrozenType) =
            let methodTyparArity = max 0 (dc.Typars.Count - declaringTyparArity)

            if dm.IsSetter then
                ExternalSignature.setter declaringTyparArity methodTyparArity groupDomains ret
            else
                ExternalSignature.ofGroups (declaringTyparArity, methodTyparArity, groupDomains, ret)

        let reaxis = FrozenTypeBridge.reaxisMethodTypars declaringTyparArity

        try
            match translateSigGroups ctx dc.Lexed dc.Opens dc.Typars (ConstraintCollector()) dm.Signature with
            | Ok(groupDomains, ret) -> ValueSome(signatureOf (List.map reaxis groupDomains) (reaxis ret))
            | Error _ -> ValueNone
        with BodylessExternalShape _ ->
            ValueSome(signatureOf [ ExternalSignature.unitFrozen ] ExternalSignature.unfreezable)

    /// A non-`Trait` entry translates its target to a template over the val's declaring
    /// typars. An undeclared typar or an untranslatable target drops the whole entry.
    let private resolveConstraints
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (typars: TyparCollector)
        (raw: RawConstraint list)
        : ExternalConstraint list =
        // Sink for any constraints a target type carries (exotic, not surfaced).
        let throwaway = ConstraintCollector()

        // A target naming an unmodelled-body type raises `BodylessExternalShape`.
        let translate t =
            try
                translateType ctx lexed opens typars throwaway t
            with BodylessExternalShape(_, _) ->
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
                                    EqArray.ofResizeArray argFts,
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

    /// A signature naming a body-less type is dropped to `ctx.Skipped`.
    let private finalizeVal (ctx: ExtractCtx) (dv: DeferredVal) : unit =
        let dc = dv.Ctx
        let constraints = ConstraintCollector()

        let translated =
            try
                translateCurriedSig ctx dc.Lexed dc.Opens dc.Typars constraints dv.Signature
            with BodylessExternalShape(compiled, reason) ->
                Error(sprintf "signature names '%s', which is %s" compiled reason.Description)

        match translated with
        | Error e ->
            // Skip so unresolved names don't masquerade as opaque TyConsts.
            ctx.Skipped.Add(dv.File, sprintf "%s: %s" (SymbolKeyOps.qualifiedName (SymbolKey.Binding dv.Key)) e)
        | Ok template ->
            let resolved =
                resolveConstraints ctx dc.Lexed dc.Opens dc.Typars (constraints.Snapshot())

            // Snapshot the typar count AFTER the constraints resolve: a PHANTOM typar,
            // present in no parameter/result and only inside a coercion target (`'E` in
            // `'S :> IStructSeq<'T,'E>`), is interned while that target is translated.
            let typarCount = dc.Typars.Count

            // The SOURCE arity of a module function: each `ArgsSpec` group's `*`-separated
            // width (`a * b ->` is one group of width 2, `(a*b) ->` is width 1), with its
            // parameter type from peeling exactly one `FTFun` per group off the template.
            let valReprOpt =
                let (CurriedSig(argGroups, _)) = dv.Signature

                let arities =
                    [
                        for i in 0 .. argGroups.Length - 1 ->
                            let struct (ArgsSpec(specs, _), _) = argGroups.[i]
                            specs.Length
                    ]

                let paramTys, resultTy = TastLower.peelFunDomains arities.Length template

                if List.isEmpty arities || List.length paramTys <> List.length arities then
                    ValueNone
                else
                    ValueSome(TastLower.externalValRepr typarCount (List.zip arities paramTys) resultTy)

            let sym: ExternalSymbol =
                { ExternalSymbols.scheme dv.Key.Decl dv.Key.Name template typarCount resolved with
                    ValRepr = valReprOpt
                }

            ctx.Symbols.[sym.Name] <- sym

            // Source-name alias for a `ModuleSuffix` module's members: `List.fold` alongside
            // the compiled `ListModule.fold`.
            match dv.Source with
            | ValueSome source when source <> sym.Name && not (ctx.Symbols.ContainsKey source) ->
                ctx.Symbols.[source] <- { sym with Name = source }
            | _ -> ()

    /// Freeze the body / member / val CSTs the declaration just walked stashed, then DROP
    /// them. Declarations resolve top-down as F# does: a body reaches its own `and`-group and
    /// everything declared before it, and nothing declared after — which is no longer
    /// deferred by the time a later declaration is walked.
    let private finalizeDeferred (ctx: ExtractCtx) : unit =
        let bodyKeys = ctx.DeferredBodies.Keys |> Seq.toArray
        // A member list is rebuilt (dropped members removed) and written back, so we can't
        // enumerate while mutating.
        let memberKeys = ctx.DeferredMembers.Keys |> Seq.toArray

        for k in bodyKeys do
            let shape = ctx.TypeShapes.[k]

            let finalized =
                match shape, ctx.DeferredBodies.[k] with
                | ExternalTypeShape.Record(arity, fields, origin, isValueType), DeferredBody.Record(dc, csts) ->
                    let fields' =
                        fields
                        |> EqArray.mapi (fun i f ->
                            { f with
                                Frozen = freezeBodyType ctx dc csts.[i]
                            }
                        )

                    ExternalTypeShape.Record(arity, fields', origin, isValueType)
                | ExternalTypeShape.Union(arity, cases, _, origin), DeferredBody.Union(dc, caseCsts, ifaceCsts) ->
                    let cases' =
                        cases
                        |> EqArray.mapi (fun i c ->
                            { c with
                                FrozenFieldTypes = caseCsts.[i] |> Array.map (freezeBodyType ctx dc) |> EqArray.ofArray
                            }
                        )

                    // Freeze the union's `interface <ty>` impls so a bare cons-list's
                    // `interface seq<'T>` matches the enumerable capability.
                    ExternalTypeShape.Union(arity, cases', freezeInterfaces ctx dc ifaceCsts, origin)
                | ExternalTypeShape.Abbrev(arity, _), DeferredBody.Abbrev(dc, rhs) ->
                    ExternalTypeShape.Abbrev(arity, freezeBodyType ctx dc rhs)
                // A class's deferred `inherit <type>` base + `interface <type>` impls.
                | ExternalTypeShape.Class shape, DeferredBody.Class(dc, baseOpt, ifaces, _) ->
                    ExternalTypeShape.Class
                        { shape with
                            FrozenBaseType = baseOpt |> ValueOption.map (freezeBodyType ctx dc)
                            FrozenInterfaces = freezeInterfaces ctx dc ifaces
                        }
                // A SCALAR primitive's declared `interface`s, frozen onto its intrinsic surface.
                | ExternalTypeShape.Intrinsic ishape, DeferredBody.Class(dc, _, ifaces, _) ->
                    ExternalTypeShape.Intrinsic
                        { ishape with
                            Class =
                                ValueSome
                                    {
                                        Heritable = false
                                        BaseType = ValueNone
                                        Interfaces = freezeInterfaces ctx dc ifaces
                                        Members = EqArray.empty
                                    }
                        }
                | _ -> shape

            ctx.TypeShapes.[k] <- finalized

        for key in memberKeys do
            match ctx.TypeMembers.TryGetValue key with
            | true, members ->
                let deferred = ctx.DeferredMembers.[key]
                let kept = ResizeArray<ExternalMember>(members.Count)

                for i in 0 .. members.Count - 1 do
                    let m = members.[i]
                    let s = m.Signature

                    match freezeMemberSig ctx s.DeclaringTyparArity deferred.[i] with
                    | ValueSome sign ->
                        // Rebuild the member key's `ArgSig` from the now-frozen groups:
                        // extraction stamped it empty, the signature still being deferred.
                        kept.Add
                            { m with
                                Signature = sign
                                Key =
                                    { m.Key with
                                        ArgSig = ExternalSignature.argSigOf sign
                                        MethodTyparArity = sign.MethodTyparArity
                                    }
                                MethodTyparArity = sign.MethodTyparArity
                            }
                    | ValueNone -> ()

                ctx.TypeMembers.[key] <- kept
            | _ -> ()

        // A contract INTERFACE carries its (now-finalized) members in the shape too: a
        // nominal class serves members only through `TryLookupMember`, but the
        // `interface … with` conformance check reads `shape.Members` directly.
        for k in memberKeys do
            match ctx.TypeShapes.[k] with
            | ExternalTypeShape.Class shape when shape.IsInterface && shape.Members.IsEmpty ->
                match ctx.TypeMembers.TryGetValue k with
                | true, members when members.Count > 0 ->
                    ctx.TypeShapes.[k] <-
                        ExternalTypeShape.Class
                            { shape with
                                Members = EqArray.ofResizeArray members
                            }
                | _ -> ()
            | _ -> ()

        // Constructors: a class's deferred `new: … -> T` sigs freeze into `.ctor` members,
        // named `.ctor` and instance as the metadata layer spells them.
        for k in bodyKeys do
            match ctx.DeferredBodies.[k] with
            | DeferredBody.Class(dc, _, _, ctors) when not (List.isEmpty ctors) ->
                let arity =
                    match ctx.TypeShapes.TryGetValue k with
                    | true, ExternalTypeShape.Class shape -> shape.TyparArity
                    | _ -> 0

                let declKey = SymbolKeyOps.qualifiedTypeKeyOf k arity

                let ctorMembers =
                    [
                        for (paramCsts, retCst) in ctors do
                            let parameters =
                                paramCsts
                                |> Array.map (freezeBodyType ctx dc)
                                |> EqArray.ofArray
                                |> ExternalSignature.tupledParams

                            let ret = freezeBodyType ctx dc retCst

                            ExternalMember.ctor
                                declKey
                                (ExternalSignature.make (arity, 0, parameters, ret))
                                (ExternalSignature.argSigOfParameters parameters)
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

        // Heritable primitives (`extern class with …`: `obj`/`exn`): republish ONCE, now that
        // both surfaces are complete, the base frozen by the shape loop and the `.ctor`s by
        // the ctor loop.
        for KeyValue(compiled, struct (canon, platform)) in ctx.PendingIntrinsicClasses do
            match ctx.TypeShapes.TryGetValue compiled with
            | true, ExternalTypeShape.Class shape ->
                // A heritable primitive's `.ctor`s are authored against the CANON
                // (`Vesper.exn`) but EMIT against the platform class (`System.Exception`).
                // Only the declaring key is rebased; the SIGNATURE stays canon.
                let platformDecl = SymbolKeyOps.qualifiedTypeKeyOf platform 0

                let ctors =
                    match ctx.TypeMembers.TryGetValue compiled with
                    | true, ms ->
                        ms
                        |> Seq.filter (fun m -> m.Name = ".ctor")
                        |> Seq.map (fun m ->
                            { m with
                                Key = { m.Key with Decl = platformDecl }
                            }
                        )
                        |> EqArray.ofSeq
                    | _ -> EqArray.empty

                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Intrinsic
                        {
                            Id =
                                {
                                    Canon = canon
                                    TyparArity = shape.TyparArity
                                    Platform = IntrinsicPlatform.Repr platform
                                }
                            Class =
                                ValueSome
                                    {
                                        Heritable = true
                                        BaseType = shape.FrozenBaseType
                                        Interfaces = shape.FrozenInterfaces
                                        Members = ctors
                                    }
                        }
            | _ -> ()

        // Capability interfaces (`disposable`/`equatable`/`comparable`): republish ONCE as an
        // `IntrinsicInterface`, now that the member surface is populated.
        for KeyValue(compiled, struct (canon, platform)) in ctx.PendingCapabilityInterfaces do
            match ctx.TypeShapes.TryGetValue compiled with
            | true, ExternalTypeShape.Class shape ->
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.IntrinsicInterface
                        {
                            Canon = canon
                            TyparArity = shape.TyparArity
                            Platform = platform
                            Members = shape.Members
                            // The capability's own inherited interfaces: `type enumerator<'T> =
                            // extern interface with inherit Vesper.disposable`.
                            Interfaces = shape.FrozenInterfaces
                            Origin = shape.Origin
                        }
            | _ -> ()

        // Vals last: a val signature / constraint target may name an abbreviation,
        // record, or union whose template the loops above just filled. Source order
        // is preserved so the `ModuleSuffix` source-name alias stays first-wins.
        for dv in ctx.DeferredVals do
            finalizeVal ctx dv

        ctx.DeferredBodies.Clear()
        ctx.DeferredMembers.Clear()
        ctx.DeferredVals.Clear()
        ctx.PendingIntrinsicClasses.Clear()
        ctx.PendingCapabilityInterfaces.Clear()

    module ExtractCtx =
        let empty = VesperLibTyparCapture.ExtractCtx.empty

        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            VesperLibTyparCapture.ExtractCtx.toProvider ctx

    let private isAccessible (access: Access<SyntaxToken> voption) : bool =
        match access with
        | ValueNone
        | ValueSome(Access.Public _) -> true
        | ValueSome(Access.Internal _)
        | ValueSome(Access.Private _) -> false

    /// The identity of a val: its declaring container (`decl`, with `ModuleSuffix` already baked
    /// into the module names) plus its compiled simple name, which `[<CompiledName(_)>]` sets.
    let private bindingKeyForVal
        (lexed: Lexed)
        (decl: ModuleContainer)
        (attrs: Attributes<SyntaxToken> voption)
        (ident: IdentOrOp<SyntaxToken>)
        : BindingKey voption =
        let identName =
            match tryCompiledName lexed attrs with
            | ValueSome n -> ValueSome n
            | ValueNone -> OperatorNames.ofDeclaredName (SyntaxToken.nameIn lexed) ident

        match identName with
        | ValueNone -> ValueNone
        | ValueSome n -> ValueSome(SymbolKeyOps.bindingKeyOf decl n)

    /// The *source*-qualified name for a val, the one the front end writes. IGNORES
    /// `[<CompiledName(_)>]`: a consumer writes `Set.empty`, not `Set.Empty`.
    let private sourceNameForVal (lexed: Lexed) (path: string list) (ident: IdentOrOp<SyntaxToken>) : string voption =
        match OperatorNames.ofDeclaredName (SyntaxToken.nameIn lexed) ident with
        | ValueNone -> ValueNone
        | ValueSome n ->
            let qualifier = String.concat "." (List.rev path)

            if qualifier.Length = 0 then
                ValueSome n
            else
                ValueSome(qualifier + "." + n)

    let private extractValSig
        (ctx: ExtractCtx)
        (file: OriginPath)
        (lexed: Lexed)
        (opens: string list)
        (decl: ModuleContainer)
        // The *source* module path (no `ModuleSuffix` rewrite). Differs from the compiled
        // container `decl` only inside a `[<CompilationRepresentation(ModuleSuffix)>]` module,
        // where `List` ⇒ `ListModule`.
        (sourcePath: string list)
        (valSig: ValSig<SyntaxToken>)
        : unit =
        let (ValSig(attrs, _, _, access, _, ident, typars, _, signature, _)) = valSig

        if not (isAccessible access) then
            ()
        else
            match bindingKeyForVal lexed decl attrs ident with
            | ValueNone -> ()
            | ValueSome key ->
                // Stash only: the signature is translated in the finalize pass, once the
                // registry is complete. Seeding the val's explicit `<'T>` typars here makes
                // that pass read the same indices.
                let collector = TyparCollector()
                registerExplicitTypars lexed collector typars

                ctx.DeferredVals.Add
                    {
                        Ctx =
                            {
                                Lexed = lexed
                                Opens = opens
                                Typars = collector
                            }
                        Key = key
                        Source = sourceNameForVal lexed sourcePath ident
                        File = file
                        Signature = signature
                    }

    let private registerPrefixTypars
        (lexed: Lexed)
        (typars: TyparCollector)
        (prefix: PrefixTypars<SyntaxToken> voption)
        : unit =
        let register (t: Typar<SyntaxToken>) =
            match t with
            | Typar.Named(_, identTok)
            | Typar.Static(_, identTok) -> typars.IndexOf(SyntaxToken.nameIn lexed identTok) |> ignore
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

    let private shortNameOfTypeName (lexed: Lexed) (typeName: TypeName<SyntaxToken>) : string =
        let (TypeName(_, _, _, ident, _, _)) = typeName

        if ident.Idents.Length = 0 then
            ""
        else
            SyntaxToken.nameIn lexed ident.Idents.[ident.Idents.Length - 1]

    let private registerTypeDecl
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (decl: ModuleContainer)
        (typeName: TypeName<SyntaxToken>)
        : struct (string * int) voption =
        let (TypeName(_, _, prefix, ident, defns, _)) = typeName

        if ident.Idents.Length = 0 then
            ValueNone
        else
            let short = SyntaxToken.nameIn lexed ident.Idents.[ident.Idents.Length - 1]

            if short.Length = 0 then
                ValueNone
            else
                let arity = typeNameTypars defns prefix

                // The containment the walker descended: namespace at the root, one
                // `InModule` per enclosing module.
                let key =
                    SymbolKeyOps.typeKeyOfContainer (ModuleRules.typeContainerOf decl) short arity

                // The arity suffix plus the `+`-nesting of a module-held type
                // (`Vesper.Choice`2`), the same string the emitted `TypeDef` carries.
                let compiled = SymbolKeyOps.typeMetaName key

                // First declaration wins on a *short-name* collision; arity-overloaded
                // types share the short name, so only the first arity is reachable by
                // bare short name (the consumer resolves the rest by arity-key).
                if not (ctx.Types.ContainsKey short) then
                    ctx.Types.[short] <- (arity, compiled)

                ctx.TypeKeys.[compiled] <- key
                ValueSome(struct (compiled, arity))

    let private collectorForTypeName (lexed: Lexed) (typeName: TypeName<SyntaxToken>) : TyparCollector =
        let (TypeName(_, _, prefix, _, defns, _)) = typeName
        let collector = TyparCollector()
        registerPrefixTypars lexed collector prefix
        registerExplicitTypars lexed collector defns
        collector

    /// Record *why* in `ctx.Skipped` AND register the `Unmodelled` shape, so a type whose
    /// name+arity are known leaves no name-without-shape gap.
    let private skipBodyUnmodelled
        (ctx: ExtractCtx)
        (file: OriginPath)
        (compiled: string)
        (arity: int)
        (reason: string)
        : unit =
        ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled reason)
        ctx.TypeShapes.[compiled] <- ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed reason, arity)

    let private extractAbbrevBody
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken>)
        : unit =
        // Full-defer: register a placeholder abbreviation shape and stash the RHS CST; the
        // finalize pass translates it once the registry is complete (the RHS may
        // forward-reference a later type).
        let collector = collectorForTypeName lexed typeName
        ctx.TypeShapes.[compiled] <- ExternalTypeShape.Abbrev(arity, deferredTemplate)

        ctx.DeferredBodies.[compiled] <-
            DeferredBody.Abbrev(
                {
                    Lexed = lexed
                    Opens = opens
                    Typars = collector
                },
                rhs
            )

    let private extractRecordBody
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (fields: RecordFields<SyntaxToken>)
        : unit =
        // Full-defer: field names + mutability come straight from the CST; the field-type
        // CSTs are stashed for the finalize pass, where an unsupported field type degrades
        // that field alone.
        let collector = collectorForTypeName lexed typeName
        let shapes = ResizeArray<ExternalFieldShape>(fields.Length)
        // The per-field type CSTs, index-aligned with `shapes`.
        let csts = ResizeArray<Type<SyntaxToken>>(fields.Length)

        for i in 0 .. fields.Length - 1 do
            let (RecordField(_, mutableTok, _, identTok, _, fieldTy)) = fields.[i]
            shapes.Add(ExternalFieldShape.create (SyntaxToken.nameIn lexed identTok, mutableTok.IsSome))
            csts.Add fieldTy

        // `Origin` is stamped later by the resolving source; the extractor records `Empty`.
        ctx.TypeShapes.[compiled] <-
            ExternalTypeShape.Record(
                arity,
                EqArray.ofResizeArray shapes,
                SymbolOrigin.Empty,
                (classFlagsOfTypeName lexed typeName).IsValueType
            )

        ctx.DeferredBodies.[compiled] <-
            DeferredBody.Record(
                {
                    Lexed = lexed
                    Opens = opens
                    Typars = collector
                },
                csts.ToArray()
            )

    let private extractUnionBody
        (ctx: ExtractCtx)
        (file: OriginPath)
        (lexed: Lexed)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (cases: UnionTypeCases<SyntaxToken>)
        (interfaces: Type<SyntaxToken> list)
        : unit =
        let collector = collectorForTypeName lexed typeName
        let caseShapes = ResizeArray<ExternalCaseShape>(cases.Length)
        // The per-case field-type CSTs (one array per case, index-aligned with
        // `caseShapes`); stashed in `ctx.DeferredBodies` for the finalize pass.
        let caseCsts = ResizeArray<Type<SyntaxToken>[]>(cases.Length)
        let mutable err = None

        // Operator-named cases (the cons-list's `([])` / `(::)`) take their canonical
        // *ctor* names `Empty`/`Cons`, matching a locally-compiled union's case names.
        let caseName (ioo: IdentOrOp<SyntaxToken>) : string voption =
            OperatorNames.unionCaseCtorName (SyntaxToken.nameIn lexed) ioo

        for i in 0 .. cases.Length - 1 do
            if err.IsNone then
                let (UnionTypeCase(_, data)) = cases.[i]

                match data with
                | UnionTypeCaseData.Nullary ident ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add(ExternalCaseShape.create (n, EqArray.empty))
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
                                | UnionTypeField.Named(identTok, _, t) ->
                                    ValueSome(SyntaxToken.nameIn lexed identTok), t

                            names.Add nameOpt
                            fieldCsts.Add fieldTy

                        caseShapes.Add(ExternalCaseShape.create (n, EqArray.ofResizeArray names))
                        caseCsts.Add(fieldCsts.ToArray())

                | UnionTypeCaseData.GadtNullary(name = ident) ->
                    // GADT-syntax nullary (`([]): 'T list`): the explicit return type is the
                    // declaring union and carries no field, so it models as an ordinary
                    // nullary case.
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add(ExternalCaseShape.create (n, EqArray.empty))
                        caseCsts.Add [||]

                | UnionTypeCaseData.GadtNary(name = ident; sign = UncurriedSig(args = ArgsSpec(specs, _))) ->
                    // GADT-syntax n-ary (`(::): Head: 'T * Tail: 'T list -> 'T list`):
                    // the fields are the signature's args; the return type names the
                    // declaring union and is ignored.
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(specs.Length)
                        let fieldCsts = ResizeArray<Type<SyntaxToken>>(specs.Length)

                        for j in 0 .. specs.Length - 1 do
                            let (ArgSpec(_, nameSpec, fieldTy)) = specs.[j]

                            let nameOpt =
                                match nameSpec with
                                | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(SyntaxToken.nameIn lexed id)
                                | ValueNone -> ValueNone

                            names.Add nameOpt
                            fieldCsts.Add fieldTy

                        caseShapes.Add(ExternalCaseShape.create (n, EqArray.ofResizeArray names))
                        caseCsts.Add(fieldCsts.ToArray())

        // A structurally-broken case (an unresolvable case name) downgrades the whole union:
        // there is no per-case name to register.
        match err with
        | Some e -> skipBodyUnmodelled ctx file compiled arity e
        | None ->
            let (TypeName(attrs, _, _, _, _, _)) = typeName

            if isRequireQualifiedAccess lexed attrs then
                ctx.RqaTypes.Add compiled |> ignore

            // `Origin` is stamped later by the resolving source; the extractor records
            // `Empty`. The interfaces start empty because the finalize pass freezes them.
            ctx.TypeShapes.[compiled] <-
                ExternalTypeShape.Union(arity, EqArray.ofResizeArray caseShapes, EqArray.empty, SymbolOrigin.Empty)

            ctx.DeferredBodies.[compiled] <-
                DeferredBody.Union(
                    {
                        Lexed = lexed
                        Opens = opens
                        Typars = collector
                    },
                    caseCsts.ToArray(),
                    interfaces
                )

    /// An enum body needs no finalize pass: a case value is a literal, never a type
    /// reference, so nothing here can forward-reference a later declaration.
    let private extractEnumBody
        (ctx: ExtractCtx)
        (file: OriginPath)
        (lexed: Lexed)
        (compiled: string)
        (arity: int)
        (cases: EnumTypeCases<SyntaxToken>)
        : unit =
        let nameOf = SyntaxToken.nameIn lexed
        let caseShapes = ResizeArray<ExternalEnumCaseShape>(cases.Length)
        let mutable err = None

        for i in 0 .. cases.Length - 1 do
            if err.IsNone then
                let (EnumTypeCase(ident = ident; constValue = v)) = cases.[i]
                let name = nameOf ident

                match EnumCaseValues.tryResolve nameOf v with
                // The declaring package's own compilation reported WHICH literal form failed.
                | Error _ -> err <- Some(sprintf "enum case '%s' has no constant value" name)
                | Ok lit ->
                    let value =
                        match lit with
                        | TEnumLiteral.Int v -> ExternalEnumCaseValue.IntVal(snd (TEnumCases.integralValue v))
                        | TEnumLiteral.String s -> ExternalEnumCaseValue.StringVal s

                    caseShapes.Add { Name = name; Value = value }

        // One unreadable case downgrades the whole enum: a partial case table would answer
        // `E.C1` for the cases that survived and "no such case" for the rest.
        match err with
        | Some e -> skipBodyUnmodelled ctx file compiled arity e
        | None ->
            // `Origin` is stamped later by the resolving source; the extractor records `Empty`.
            ctx.TypeShapes.[compiled] <- ExternalTypeShape.Enum(EqArray.ofResizeArray caseShapes, SymbolOrigin.Empty)

    /// Extract the augmentation `member`s declared inside a type body's `with`-block
    /// (`member Value: 'T` on `Option`), translated over the *type's* typar collector.
    let private extractTypeMembers
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        if elems.Length = 0 then
            ()
        else
            let declKey = SymbolKeyOps.qualifiedTypeKeyOf compiled arity

            let members = ResizeArray<ExternalMember>()
            // The per-member signature CSTs, index-aligned with `members`;
            // stashed in `ctx.DeferredMembers` for the finalize pass to translate.
            let memberCsts = ResizeArray<DeferredMember>()

            for i in 0 .. elems.Length - 1 do
                let memberSig =
                    match elems.[i] with
                    | TypeSignatureElement.Member(signature = s) -> ValueSome(false, s)
                    // An `abstract member` slot (`type IFormatSink = abstract member Text: …`)
                    // is a distinct CST element; published as an ordinary instance member so
                    // a `sink.Text(…)` call and the conformance check resolve against it.
                    | TypeSignatureElement.Abstract(signature = s) -> ValueSome(false, s)
                    | TypeSignatureElement.StaticMember(signature = s) -> ValueSome(true, s)
                    | _ -> ValueNone

                match memberSig with
                | ValueNone -> ()
                | ValueSome(isStatic, sign) ->
                    // A member's OWN typars (explicit `<'a>` via `typarDefns`, or an implicit
                    // `'T` the signature names) go on the method axis: the finalize pass flips
                    // any typar interned beyond the declaring type's own to `FTTypar(Method,_)`.
                    let identAndSig =
                        match sign with
                        | MemberSig.MethodOrPropSig(ident = ioo; typarDefns = defns; sign = csig) ->
                            {|
                                Ident = ioo
                                TyparDefns = defns
                                Sig = csig
                                WithClause = ValueNone
                            |}
                        | MemberSig.PropSig(ident = ioo; typarDefns = defns; sign = csig; getSet = gs) ->
                            {|
                                Ident = ioo
                                TyparDefns = defns
                                Sig = csig
                                WithClause = ValueSome gs
                            |}

                    match OperatorNames.ofDeclaredName (SyntaxToken.nameIn lexed) identAndSig.Ident with
                    | ValueNone -> ()
                    | ValueSome memberName ->
                        // Full-defer: stash the member + its signature CST. The collector
                        // takes the type's own typars and THEN the member's explicit `<'a>`
                        // ones, so those land at indices `>= arity` in declared order.
                        let collector = collectorForTypeName lexed typeName
                        registerExplicitTypars lexed collector identAndSig.TyparDefns
                        let csig = identAndSig.Sig
                        let (CurriedSig(args, _)) = csig
                        // A property carries an object argument and nothing else, so a
                        // signature with arguments declares a method.
                        let takesArgs = args.Length > 0

                        let publish
                            (m:
                                {|
                                    Name: string
                                    IsProperty: bool
                                    IsSetter: bool
                                |})
                            =
                            let kind =
                                if m.IsProperty then
                                    MemberKind.Property
                                else
                                    MemberKind.Method

                            // A setter's groups collapse to the ONE parameter vector it is
                            // dispatched with, however many the getter's signature wrote.
                            let argGroupCount = if m.IsSetter then 1 else args.Length

                            members.Add
                                { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey m.Name EqArray.empty 0 kind) with
                                    IsStatic = isStatic
                                    Storage =
                                        if m.IsProperty then
                                            MemberStorage.Property
                                        else
                                            MemberStorage.Method
                                    Signature = ExternalSignature.deferred (arity, 0, argGroupCount)
                                }

                            memberCsts.Add
                                {
                                    Ctx =
                                        {
                                            Lexed = lexed
                                            Opens = opens
                                            Typars = collector
                                        }
                                    Signature = csig
                                    IsSetter = m.IsSetter
                                }

                        match identAndSig.WithClause with
                        | ValueNone ->
                            publish
                                {|
                                    Name = memberName
                                    IsProperty = not takesArgs
                                    IsSetter = false
                                |}
                        | ValueSome getSet ->
                            let halves = AccessorNames.halvesOf (SyntaxToken.nameIn lexed) getSet

                            if halves.Getter.IsSome then
                                publish
                                    {|
                                        Name =
                                            if takesArgs then
                                                AccessorNames.getterName memberName
                                            else
                                                memberName
                                        IsProperty = not takesArgs
                                        IsSetter = false
                                    |}

                            if halves.Setter.IsSome then
                                publish
                                    {|
                                        Name = AccessorNames.setterName memberName
                                        IsProperty = false
                                        IsSetter = true
                                    |}

            if members.Count > 0 then
                ctx.TypeMembers.[compiled] <- members
                ctx.DeferredMembers.[compiled] <- memberCsts

    /// F# infers an interface from a bodied type whose members are *all* abstract
    /// (`type IFormatSink = abstract member …`, no `interface`/`class`/`begin` keyword),
    /// which parses as `TypeSignature.Anon`/`Class`, so no keyword carries the answer.
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

    /// Register a class-shaped type from a member body: the bodiless `basic` `Class` shape
    /// plus its deferred inherit/interface/ctor body and extracted members. An
    /// `extern … with` capability surface publishes the same way.
    let private extractBodiedClassLike
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (isInterface: bool)
        (typeName: TypeName<SyntaxToken>)
        (elements: TypeElementsSignature<SyntaxToken>)
        : unit =
        ctx.TypeShapes.[compiled] <-
            ExternalTypeShape.Class(
                { ExternalClassShape.basic (arity, isInterface, SymbolOrigin.Empty) with
                    Flags = classFlagsOfTypeName lexed typeName
                }
            )

        // The `inherit <type>` clause, `interface <type>` impls and `new: … -> T` ctors are
        // deferred: any of them may forward-reference a sibling.
        let inheritClause =
            elements
            |> Seq.tryPick (fun e ->
                match e with
                | TypeSignatureElement.Inherit(ClassInheritsDecl(typ = t)) -> Some t
                | _ -> None
            )

        // For an INTERFACE, an `inherit <ty>` clause is interface inheritance (`enumerator
        // inherit disposable`), NOT a base class, because interfaces have no base type, so it is
        // routed into `FrozenInterfaces`. For a CLASS (`exn inherit obj`) it is the base.
        let inheritBase = if isInterface then None else inheritClause

        let interfaces =
            [
                match (if isInterface then inheritClause else None) with
                | Some t -> t
                | None -> ()
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
            let collector = collectorForTypeName lexed typeName

            ctx.DeferredBodies.[compiled] <-
                DeferredBody.Class(
                    {
                        Lexed = lexed
                        Opens = opens
                        Typars = collector
                    },
                    (match inheritBase with
                     | Some t -> ValueSome t
                     | None -> ValueNone),
                    interfaces,
                    ctors
                )

        extractTypeMembers ctx lexed opens compiled arity typeName elements

    /// A concrete member on an INTRINSIC `extern` type must be declared `inline`: the
    /// primitive has no representation in the output to carry a method, so a use site can
    /// only splice the body its sibling `.fs` gives it.
    let private requireInlineExternMembers
        (ctx: ExtractCtx)
        (file: OriginPath)
        (name: string)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        for e in elems do
            match e with
            | TypeSignatureElement.Member(inlineToken = ValueNone)
            | TypeSignatureElement.StaticMember(inlineToken = ValueNone) ->
                ctx.Diagnostics.Add(file, IntrinsicHost.memberNeedsInline name)
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _ ->
                ctx.Diagnostics.Add(file, IntrinsicHost.cannotDeclare name IntrinsicHost.Construct.Override)
            | TypeSignatureElement.Member _
            | TypeSignatureElement.StaticMember _
            // `abstract` declares a slot and `val` storage, so neither is a body. `new` on a
            // heritable primitive (`obj`/`exn`) names a target-provided constructor.
            | TypeSignatureElement.Abstract _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Inherit _
            | TypeSignatureElement.Interface _ -> ()

    let private registerUnmodelled
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (decl: ModuleContainer)
        (typeName: TypeName<SyntaxToken>)
        (reason: UnmodelledReason)
        : unit =
        match registerTypeDecl ctx lexed decl typeName with
        | ValueNone -> ()
        | ValueSome(struct (compiled, arity)) ->
            ctx.TypeShapes.[compiled] <- ExternalTypeShape.Unmodelled(reason, arity)

    let private extractTypeSig
        (ctx: ExtractCtx)
        (file: OriginPath)
        (lexed: Lexed)
        (opens: string list)
        (decl: ModuleContainer)
        (ts: TypeSignature<SyntaxToken>)
        : unit =
        match ts with
        | TypeSignature.Abbrev(typeName = typeName; typ = rhs) ->
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) -> extractAbbrevBody ctx lexed opens compiled arity typeName rhs

        | TypeSignature.Record(typeName = typeName; fields = fields) ->
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) -> extractRecordBody ctx lexed opens compiled arity typeName fields

        | TypeSignature.Union(typeName = typeName; cases = cases; extensions = extensions) ->
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                // A union's trailing `with interface <ty> with …` impls ride the shared
                // extension-elements list; there is no union-specific parser field.
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

                extractUnionBody ctx file lexed opens compiled arity typeName cases interfaceCsts

                match extensions with
                | ValueSome(TypeExtensionElementsSignature(_, elems, _)) ->
                    extractTypeMembers ctx lexed opens compiled arity typeName elems
                | ValueNone -> ()

        | TypeSignature.Interface(typeName = typeName) ->
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                // A class/interface shape carries no body, so its members resolve separately
                // via `TryLookupMember` and this registration only answers `TryLookupType`.
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Class(ExternalClassShape.basic (arity, true, SymbolOrigin.Empty))

        | TypeSignature.Extern(typeName = typeName; kindTag = kindTag; members = members) ->
            // `extern` DECLARES that the platform supplies the representation, so the type is
            // an intrinsic on every target. Whether THIS target supplies it is the separate
            // question `ctx.IntrinsicReprs` answers, pre-extracted from this target's `.fs`.
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                let short = shortNameOfTypeName lexed typeName

                // `short` (the `.fsi` name) is the platform-invariant `canon` key, so a
                // target that omits the repr still publishes the type, marked unsupported.
                let registerIntrinsic () =
                    let platform =
                        match ctx.IntrinsicReprs.TryGetValue short with
                        | true, repr -> IntrinsicPlatform.Repr repr
                        | _ -> IntrinsicPlatform.Unsupported ctx.Target

                    let canon = SymbolKeyOps.intrinsicCanonKey compiled

                    ctx.TypeShapes.[compiled] <-
                        ExternalTypeShape.Intrinsic(
                            // A member-less `extern class` (`Attribute`) declares no surface but
                            // is still heritable, and only the shape carries that.
                            match kindTag with
                            | ValueSome(ExternKind.Class _) -> IntrinsicShape.HeritableClass(canon, arity, platform)
                            | _ -> IntrinsicShape.Scalar(canon, arity, platform)
                        )

                match members with
                | ValueSome(TypeExtensionElementsSignature(_, elems, _)) when not (Seq.isEmpty elems) ->
                    // A trailing `with interface … / member …` publishes a capability surface:
                    // register as a bodied class/interface, since `ExternalTypeShape.Intrinsic`
                    // carries no member slots. Interface-ness is the EXPLICIT tag, not inferred.
                    let isInterface =
                        match kindTag with
                        | ValueSome(ExternKind.Interface _) -> true
                        | _ -> false

                    requireInlineExternMembers ctx file short elems

                    extractBodiedClassLike ctx lexed opens compiled arity isInterface typeName elems

                    // RECORD a one-shot finalize-time republish, since neither surface is
                    // complete at extraction.
                    match kindTag with
                    // An untagged `extern with member …`: a scalar. Re-registers the
                    // `Intrinsic` shape the bodied-class extraction overwrote; the members
                    // ride their own table, not the shape. A declared `interface` needs no
                    // repr here — a target that binds none marks the type unsupported, so
                    // no source it compiles can name the type and reach the capability.
                    | ValueNone -> registerIntrinsic ()
                    | ValueSome tag ->
                        match ctx.IntrinsicReprs.TryGetValue short with
                        | true, platform ->
                            let canon = SymbolKeyOps.intrinsicCanonKey compiled

                            match tag with
                            // `extern class with …` (obj/exn): a heritable PRIMITIVE.
                            | ExternKind.Class _ -> ctx.PendingIntrinsicClasses.[compiled] <- struct (canon, platform)
                            // `extern interface with …` (`disposable`/`equatable`/`comparable`):
                            // republishes to an `IntrinsicInterface`.
                            | ExternKind.Interface _ ->
                                ctx.PendingCapabilityInterfaces.[compiled] <- struct (canon, platform)
                        | _ -> ()
                | _ -> registerIntrinsic ()

        | TypeSignature.Struct(typeName = typeName) ->
            // A `type X = struct … end` value type: the same nominal `Class` shape as a
            // reference class, plus the value-type-ness a consumer's encoder needs to emit
            // `ELEMENT_TYPE_VALUETYPE` rather than `CLASS`.
            match registerTypeDecl ctx lexed decl typeName with
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
            // A nominal class with a member body (`Vesper.Set`'s `Set<'T>`). The shape stays
            // the bodiless `basic` `Class`, but the body's sigs ARE extracted, notably the
            // `op_Addition` an SRTP `+` hits.
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                let isInterface = bodyIsInterface elements
                extractBodiedClassLike ctx lexed opens compiled arity isInterface typeName elements

        | TypeSignature.AbstractType typeName ->
            // An opaque abstract type (`type T`) with no body shape. Resolve as a
            // non-interface `Class` so codegen can mint a ref off the origin.
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                ctx.TypeShapes.[compiled] <-
                    ExternalTypeShape.Class(ExternalClassShape.basic (arity, false, SymbolOrigin.Empty))

        | TypeSignature.Enum(typeName = typeName; cases = cases) ->
            match registerTypeDecl ctx lexed decl typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) -> extractEnumBody ctx file lexed compiled arity cases

        // No body shape: register the name+arity plus the gap, so every registered name
        // carries a shape and a use site can say which form is missing.
        | TypeSignature.Delegate(typeName = typeName) ->
            registerUnmodelled ctx lexed decl typeName UnmodelledReason.Delegate
        | TypeSignature.TypeExtension(typeName = typeName) ->
            registerUnmodelled ctx lexed decl typeName UnmodelledReason.TypeExtension

    let private collectOpens
        (lexed: Lexed)
        (elems: System.Collections.Immutable.ImmutableArray<ModuleSignatureElement<SyntaxToken>>)
        : string list =
        let acc = ResizeArray<string>()

        for i in 0 .. elems.Length - 1 do
            match elems.[i] with
            | ModuleSignatureElement.Import(ImportDecl.ImportDecl(_, li)) -> acc.Add(longIdentName lexed li)
            | ModuleSignatureElement.Import(ImportDecl.ImportDeclType _) ->
                // `open type Foo` brings only Foo's static members into scope,
                // not Foo as a prefix. Not handled.
                ()
            | _ -> ()

        // Newest first: a later `open` shadows earlier ones.
        List.ofSeq (Seq.rev acc)

    /// The short name of a type signature that declares a real NOMINAL type, which a
    /// `module` of the same name collides with.
    let private nominalTypeSigName (lexed: Lexed) (ts: TypeSignature<SyntaxToken>) : string voption =
        let named (tn: TypeName<SyntaxToken>) =
            match shortNameOfTypeName lexed tn with
            | "" -> ValueNone
            | n -> ValueSome n

        match ts with
        | TypeSignature.Record(typeName = tn)
        | TypeSignature.Union(typeName = tn)
        | TypeSignature.Anon(typeName = tn)
        | TypeSignature.Class(typeName = tn)
        | TypeSignature.Struct(typeName = tn) -> named tn
        | TypeSignature.Abbrev _
        | TypeSignature.Interface _
        | TypeSignature.Enum _
        | TypeSignature.Delegate _
        | TypeSignature.TypeExtension _
        | TypeSignature.AbstractType _
        | TypeSignature.Extern _ -> ValueNone

    /// Runs BEFORE extraction: the `…Module` suffix rule reads the answer when it mints a
    /// module's compiled name, and `module Foo` may be written above the `type Foo` it hits.
    let rec private noteNominalTypeSigNames
        (lexed: Lexed)
        (names: System.Collections.Generic.HashSet<string>)
        (elems: ModuleSignatureElements<SyntaxToken>)
        : unit =
        let note (ts: TypeSignature<SyntaxToken>) =
            match nominalTypeSigName lexed ts with
            | ValueSome n -> names.Add n |> ignore
            | ValueNone -> ()

        for i in 0 .. elems.Length - 1 do
            match elems.[i] with
            | ModuleSignatureElement.Type(_, TypeSignatures(first, rest)) ->
                note first

                for j in 0 .. rest.Length - 1 do
                    let (_, ts) = rest.[j]
                    note ts
            | ModuleSignatureElement.Module(ModuleSignature(body = ModuleSignatureBody(_, inner, _))) ->
                noteNominalTypeSigNames lexed names inner
            | _ -> ()

    let rec private extractModuleSigElement
        (ctx: ExtractCtx)
        (file: OriginPath)
        (naming: ModuleNaming)
        (lexed: Lexed)
        (opens: string list)
        // The COMPILED containment the walker has descended: the namespace at the root,
        // one `InModule` per enclosing module, `ModuleSuffix` already baked in.
        (decl: ModuleContainer)
        // The source-name twin of `decl`. Equal to it except inside a `ModuleSuffix`
        // module, where `decl` carries the compiled `…Module` segment.
        (sourcePath: string list)
        (elem: ModuleSignatureElement<SyntaxToken>)
        : unit =
        match elem with
        | ModuleSignatureElement.Val valSig -> extractValSig ctx file lexed opens decl sourcePath valSig

        // The whole `type A … and B …` group is walked before it is finalized, so the two
        // may name each other; a plain `type` is a group of one and names only what precedes it.
        | ModuleSignatureElement.Type(_, typeSigs) ->
            let (TypeSignatures(first, rest)) = typeSigs
            extractTypeSig ctx file lexed opens decl first

            for i in 0 .. rest.Length - 1 do
                let (_, ts) = rest.[i]
                extractTypeSig ctx file lexed opens decl ts

        | ModuleSignatureElement.Module moduleSig ->
            let (ModuleSignature(attrs, _, access, _, identTok, _, body)) = moduleSig

            if isAccessible access then
                let name = SyntaxToken.nameIn lexed identTok
                let compiledModuleName = ModuleRules.compiledModuleNameOf naming attrs name

                let childDecl =
                    ModuleContainer.InModule(SymbolKeyOps.moduleKeyOf decl compiledModuleName)

                let childSourcePath = name :: sourcePath
                // The containment a WRITTEN name is resolved against: the source path names
                // the module, the container carries its compiled (`…Module`-suffixed) chain.
                ctx.ModuleContainers.[String.concat "." (List.rev childSourcePath)] <-
                    ModuleRules.typeContainerOf childDecl

                let (ModuleSignatureBody(_, elems, _)) = body
                // The module's own qualified path is itself an implicit open
                // prefix, ahead of the inherited opens but behind the body's.
                let modulePath = SymbolKeyOps.containerFullName childDecl

                if isAutoOpen lexed attrs then
                    ctx.AutoOpenPrefixes.Add modulePath

                let childOpens = collectOpens lexed elems @ (modulePath :: opens)

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement ctx file naming lexed childOpens childDecl childSourcePath elems.[i]

        | _ -> ()

        finalizeDeferred ctx

    let private extractNamespaceGroup
        (ctx: ExtractCtx)
        (file: OriginPath)
        (naming: ModuleNaming)
        (lexed: Lexed)
        (fileOpens: string list)
        (group: NamespaceDeclGroupSignature<SyntaxToken>)
        : unit =
        let nsSegments, elems =
            match group with
            | NamespaceDeclGroupSignature.Named(_, _, li, els) ->
                [ for i in 0 .. li.Idents.Length - 1 -> SyntaxToken.nameIn lexed li.Idents.[i] ], els
            | NamespaceDeclGroupSignature.Global(_, _, els) -> [], els

        let nsName = String.concat "." nsSegments
        let ownOpens = collectOpens lexed elems
        // The namespace's qualified path is implicitly in scope; top-level
        // opens outside any namespace group are inherited.
        let opens =
            if nsName.Length = 0 then
                ownOpens @ fileOpens
            else
                ownOpens @ (nsName :: fileOpens)

        let decl = SymbolKeyOps.inNamespace nsName

        for i in 0 .. elems.Length - 1 do
            // A namespace path carries no `ModuleSuffix` rewrite, so source == compiled.
            extractModuleSigElement ctx file naming lexed opens decl (List.rev nsSegments) elems.[i]

    let private extractNamedModuleSig
        (ctx: ExtractCtx)
        (file: OriginPath)
        (naming: ModuleNaming)
        (lexed: Lexed)
        (fileOpens: string list)
        (nm: NamedModuleSignature<SyntaxToken>)
        : unit =
        let (NamedModuleSignature(attrs, _, access, _, li, elems)) = nm

        if isAccessible access then
            let segments =
                [ for i in 0 .. li.Idents.Length - 1 -> SyntaxToken.nameIn lexed li.Idents.[i] ]

            // `module A.B.C` declares module `C` in namespace `A.B`. The `…Module` suffix
            // applies to that module segment only.
            let decl =
                match List.rev segments with
                | [] -> SymbolKeyOps.inNamespace ""
                | last :: revNs ->
                    let name = ModuleRules.compiledModuleNameOf naming attrs last

                    ModuleContainer.InModule(SymbolKeyOps.moduleInNamespace (String.concat "." (List.rev revNs)) name)

            // `module A.B.C`'s source path is what a written `A.B.C.T` names it by; the
            // container is the compiled chain.
            if not (List.isEmpty segments) then
                ctx.ModuleContainers.[String.concat "." segments] <- ModuleRules.typeContainerOf decl

            let qualifiedSelf = SymbolKeyOps.containerFullName decl

            if isAutoOpen lexed attrs then
                ctx.AutoOpenPrefixes.Add qualifiedSelf

            let ownOpens = collectOpens lexed elems
            let opens = ownOpens @ (qualifiedSelf :: fileOpens)
            let sourcePathRev = List.rev segments

            for i in 0 .. elems.Length - 1 do
                extractModuleSigElement ctx file naming lexed opens decl sourcePathRev elems.[i]

    let extractSymbols (ctx: ExtractCtx) (parsed: ParsedFile) : unit =
        // The dependency providers' ambient prefixes (`Vesper`, …) seed the file's open
        // scope at lowest priority by appending to the END of each descended scope, so an
        // explicit `open` / the enclosing namespace still wins.
        let fileOpens = ctx.DependencyAmbientPrefixes

        match parsed.Ast with
        | FSharpAst.SignatureFile sf ->
            let nominals = System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)

            let naming: ModuleNaming =
                {
                    Lexed = parsed.Lexed
                    IsNominalTypeName = nominals.Contains
                }

            match sf with
            | SignatureFile.Namespaces groups ->
                for i in 0 .. groups.Length - 1 do
                    match groups.[i] with
                    | NamespaceDeclGroupSignature.Named(elements = els)
                    | NamespaceDeclGroupSignature.Global(elements = els) ->
                        noteNominalTypeSigNames parsed.Lexed nominals els

                for i in 0 .. groups.Length - 1 do
                    // Each namespace decl group starts a fresh open scope.
                    extractNamespaceGroup ctx parsed.File naming parsed.Lexed fileOpens groups.[i]
            | SignatureFile.NamedModule nm ->
                let (NamedModuleSignature(elements = els)) = nm
                noteNominalTypeSigNames parsed.Lexed nominals els
                extractNamedModuleSig ctx parsed.File naming parsed.Lexed fileOpens nm
            | SignatureFile.AnonymousModule elems ->
                noteNominalTypeSigNames parsed.Lexed nominals elems
                let opens = collectOpens parsed.Lexed elems @ fileOpens

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement
                        ctx
                        parsed.File
                        naming
                        parsed.Lexed
                        opens
                        (SymbolKeyOps.inNamespace "")
                        []
                        elems.[i]
        | _ -> ctx.Diagnostics.Add(parsed.File, "Skipped: not a signature file")

    /// Extract the intrinsic-representation bindings from a parsed `.fs` companion
    /// (`type exn = (# "System.Exception" #)`) into `dest` (short name ⇒ repr). Run BEFORE
    /// the `.fsi` extraction, whose `type exn = extern` omits the repr.
    let extractIntrinsicReprsInto
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
        | Some f -> IntrinsicReprs.ofImplementationInto dest (SyntaxToken.nameIn parsed.Lexed) f
