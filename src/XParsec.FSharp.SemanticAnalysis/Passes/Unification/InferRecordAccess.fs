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

module internal UnificationInferRecordAccess =

    /// A member named `memberName` on an *intrinsic* receiver (`TyConst`) whose
    /// `(# "…" #)` binding canonicalises to a BCL type (`tryExternalReceiver`).
    /// Yields the canonical BCL name, the receiver's type args, and the single-
    /// pick member — so the consuming arm resolves the member without re-running
    /// the canonicalisation or the provider lookup. Declines (the arm falls
    /// through to the array / other `TyConst` cases) when the receiver isn't an
    /// intrinsic mapped to a BCL type, or that type has no such member.
    [<return: Struct>]
    let private (|IntrinsicBclMember|_|)
        (ctx: PassContext)
        (memberName: string)
        (ty: SemType)
        : struct (string * EqArray<SemType> * ExternalMember) voption =
        match tryExternalReceiver ctx ty with
        | ValueSome(clsQual, args) ->
            match ctx.Provider.TryLookupMember(clsQual, memberName) with
            | ValueSome m -> ValueSome(struct (clsQual, args, m))
            | ValueNone -> ValueNone
        | ValueNone -> ValueNone

    let rec inferRecord
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        let candidate =
            match qualifier with
            | Some typeName ->
                match ctx.Types.Record.TryGetValue typeName with
                | true, info -> ValueSome info
                | false, _ ->
                    ctx.Error(key, sprintf "Unknown record type qualifier: %s" typeName)
                    ValueNone
            | None ->
                let cand, count = findUniqueRecordByFieldSet ctx names

                match cand with
                | ValueSome _ -> cand
                | ValueNone ->
                    if count = 0 then
                        ctx.Error(key, sprintf "No record type matches the field set: %s" (String.concat ", " names))
                    else
                        ctx.Error(
                            key,
                            sprintf
                                "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                count
                        )

                    ValueNone

        match candidate with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome info ->
            // Fresh typars per literal so independent literals get independent
            // vars; each initialiser unifies against the field type *under this
            // substitution*, pinning a `'a` field to the initialiser's type.
            let args, subst = freshNamedInstance ctx info.TypeParams

            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                | None -> ctx.Error(CstKeys.ofExpr e, sprintf "Type '%s' has no field '%s'" info.Name fieldName)

            TyRecord(info.Key, args)

    and inferRecordClone
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep srcTy with
        | TyRecord(recKey, srcArgs) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                // Clone preserves the source's arg list — overrides unify
                // against the substituted field type (`'a` → source's arg).
                let subst = mkNamedTypeSubst info.TypeParams srcArgs

                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                    | None -> ctx.Error(CstKeys.ofExpr e, sprintf "Type '%s' has no field '%s'" info.Name fieldName)

                TyRecord(recKey, srcArgs)
            | ValueNone ->
                ctx.Error(key, sprintf "Unknown record type '%s'" (SymbolKeyOps.simpleName recKey))

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord(recKey, srcArgs)
        | _ ->
            ctx.Error(key, "Record clone requires the source expression to be a record")

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// One step of dot-access resolution; deferred when the receiver is a free
    /// TyVar. For a generic receiver `(b : Box<int>).Value`, the declared field /
    /// member type `'a` is substituted against the receiver's arg list so `Value`
    /// types as `int`, not a free typar.
    /// Resolve an instance member on a project-local class/union, or emit a
    /// static-hint-aware diagnostic. Shared by the `TyClass` / `TyUnion` arms of
    /// `resolveFieldStep` — the only thing that differs between them is the
    /// registry consulted and the "Unknown … type" wording on a registry miss.
    and resolveLocalInstanceMember
        (ctx: PassContext)
        (diagKey: NodeKey)
        (typeName: string)
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        (members: TypeMemberInfo[])
        (memberName: string)
        : SemType =
        match members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
        | Some m -> instantiateMemberCall ctx (typeParams, args) m.EffectiveMethodTypars m.Type
        | None ->
            if members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic) then
                errorTy
                    ctx
                    diagKey
                    (sprintf
                        "Member '%s' on type '%s' is static; access it via '%s.%s'"
                        memberName
                        typeName
                        typeName
                        memberName)
            else
                errorTy ctx diagKey (sprintf "Type '%s' has no instance member '%s'" typeName memberName)

    /// Resolve `memberName` on a typar receiver through an
    /// interface the typar is coerced to (`'T :> IFace`). Scans the root's
    /// `Coercion` constraints; for each whose target zonks to a project-local
    /// *interface* `TyClass`, walks its members (and inherited interface members)
    /// for `memberName`. On a hit, records the interface key in
    /// `TyparInterfaceCall` (keyed by the access node) so Freeze emits a
    /// `CallVia.Interface` dispatch, and returns the member's instantiated type.
    /// `ValueNone` (the caller parks the access) when no coercion names a local
    /// interface declaring the member — an external interface coercion or a
    /// genuinely-unresolved typar both fall through to the existing path.
    and tryTyparInterfaceMember
        (ctx: PassContext)
        (diagKey: NodeKey)
        (root: TypeVar)
        (memberName: string)
        : SemType voption =
        let rec scan (cs: SemanticConstraint list) : SemType voption =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep target with
                    | TyClass(ifaceKey, ifaceArgs) ->
                        // Resolve by the interface's key, not a bare name: an
                        // arity-overloaded interface (`Fun`2`/`Fun`3`) has no bare alias, so a
                        // bare read would miss a `'T :> Fun<…>` bound's local interface.
                        match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                        | ValueSome info when info.IsInterface ->
                            match tryClassChainMember ctx ifaceKey ifaceArgs memberName with
                            | ValueSome mty ->
                                ctx.Resolution.TyparInterfaceCall.Set(diagKey, (ifaceKey, ifaceArgs))
                                ValueSome mty
                            | ValueNone -> scan rest
                        | _ ->
                            // External-interface coercion (`'T :> Vesper.Fun<int,int>`):
                            // the interface is not in the local registry, so resolve the
                            // member through the provider on the qualified key. The
                            // receiver stays a typar (never grounds to the interface), so
                            // we record `TyparInterfaceCall` — *not* `ExternalAccess` —
                            // exactly as the local path does, and Freeze emits the same
                            // `CallVia.Interface` dispatch (now on an external `TypeSpec`).
                            let ifaceQual = SymbolKeyOps.qualifiedName ifaceKey

                            match ctx.Provider.TryLookupMember(ifaceQual, memberName) with
                            | ValueSome m when not m.IsStatic ->
                                ctx.Resolution.TyparInterfaceCall.Set(diagKey, (ifaceKey, ifaceArgs))
                                ValueSome(ExternalSymbols.openSignature m (ifaceArgs.AsSpan().ToArray()))
                            | _ -> scan rest
                    | _ -> scan rest
                | _ -> scan rest

        scan root.Constraints

    and resolveFieldStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (memberName: string) : SemType =
        match resolveStep rTy with
        | TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = memberName) with
                | Some field -> instantiateMember (info.TypeParams, args) field.Type
                | None -> errorTy ctx diagKey (sprintf "Type '%s' has no field '%s'" info.Name memberName)
            | ValueNone -> errorTy ctx diagKey (sprintf "Unknown record type '%s'" (SymbolKeyOps.simpleName recKey))
        | TyClass(clsKey, args) ->
            // Resolve by the (arity-qualified) key, not the bare name: an
            // arity-overloaded receiver (`Fun\`2`/`Fun\`3`) has no bare alias, so a
            // bare read would miss. `clsSimple` survives only for the diagnostic path.
            let clsSimple = SymbolKeyOps.simpleName clsKey

            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info ->
                // Walk the inheritance chain (derived members shadow inherited).
                // On a total miss, fall back to the single-class diagnostic so
                // the static-access hint still references the receiver's own
                // class rather than some ancestor.
                match tryClassChainMember ctx clsKey args memberName with
                | ValueSome ty -> ty
                | ValueNone ->
                    // An explicit `val x: T` instance field read (struct enumerator
                    // state). Instantiate the field's declared type
                    // with the receiver's type args, mirroring the member path.
                    match info.InstanceFields |> Array.tryFind (fun f -> f.Name = memberName) with
                    | Some fld -> instantiateMember (info.TypeParams, args) fld.Type
                    | None ->
                        resolveLocalInstanceMember ctx diagKey clsSimple info.TypeParams args info.Members memberName
            | ValueNone ->
                // Not a project-local class — an *external* type (e.g. a BCL
                // `TyClass("…EqualityComparer`1", [int])` produced by a prior static
                // access). Resolve the instance member through the provider and
                // record it for Freeze.
                let clsQual = SymbolKeyOps.qualifiedName clsKey

                // Commit a resolved external instance member `m` whose signature is written
                // over ITS declaring type's typars, instantiated with `memberArgs`: the
                // receiver's own args for an own member; the supertype's args-as-reached for
                // an INHERITED one (`Base<int>`'s `[int]` for a `Child : Base<int>`).
                // This is the COMMIT of a single-candidate member (no overload set to pick
                // from), so freshen the method typars per use site exactly as the
                // multi-candidate `commitExternalOverload` does — NOT the open,
                // marker-preserving `openSignature`. A generic instance method called at two
                // instantiations would otherwise share one inert `TyTypar(Method,_)` that no
                // per-call solution can touch (rigid-vs-concrete mismatch); a non-generic
                // member is byte-identical either way.
                let commitExternalMember (m: ExternalMember) (memberArgs: EqArray<SemType>) : SemType =
                    let memberSig =
                        ExternalSymbols.instantiateSignature m (memberArgs.AsSpan().ToArray()) ctx.CurrentLevel

                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            Storage = m.Storage
                            Signature = memberSig
                            OptionalDefaults = m.OptionalDefaults
                        }
                    )

                    memberSig

                match ctx.Provider.TryLookupMember(clsQual, memberName) with
                | ValueSome m when not m.IsStatic -> commitExternalMember m args
                | _ ->

                    // An own-member miss may still resolve as a member INHERITED from an external
                    // base interface/class: the TS-manifest provider stores heritage un-flattened,
                    // so walk the receiver's supertypes (the metadata layer's `TryLookupMember`
                    // already sees inherited members via `GetInterfaces()`/`inherit`). Commit with
                    // the supertype's args so a generic base member (`Base<int>.value`) resolves at
                    // the receiver's instantiation.
                    match tryExternalInheritedMember ctx rTy memberName with
                    | ValueSome(struct (m, memberArgs)) -> commitExternalMember m memberArgs
                    | ValueNone ->
                        // A homed external `TyClass` whose home is ABSENT from the
                        // compilation: the provider stack has NO shape for it at all
                        // (`TryLookupType` also misses) and the key carries a home assembly
                        // — the fingerprint of a refs-table identity minted by one package's
                        // provider whose HOME manifest was never stacked. Name the missing
                        // package rather than emit a generic no-such-member (the plain
                        // "Unknown class type" is for an in-stack type genuinely lacking the
                        // member).
                        match ctx.Provider.TryLookupType clsQual, SymbolKeyOps.keyAsm clsKey with
                        | ValueNone, Some home ->
                            errorTy
                                ctx
                                diagKey
                                (sprintf
                                    "type '%s' is referenced from package '%s' but that package is not part of the compilation"
                                    clsSimple
                                    home)
                        | _ -> errorTy ctx diagKey (sprintf "Unknown class type '%s'" clsQual)
        | TyUnion(unionKey, args) ->
            // Union instance member access — mirrors the `TyClass` arm
            // against the union's augmentation members.
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                resolveLocalInstanceMember
                    ctx
                    diagKey
                    (SymbolKeyOps.simpleName unionKey)
                    info.TypeParams
                    args
                    info.Members
                    memberName
            | ValueNone ->
                // Not a project-local union — an *external* one (e.g. a referenced
                // `Vesper.Option` whose `IsSome`/`Value`/`IsNone` augmentation
                // members the contract provider publishes). Resolve through the
                // provider and record it for Freeze, exactly as the external
                // `TyClass` arm does.
                let unionQual = SymbolKeyOps.qualifiedName unionKey

                match ctx.Provider.TryLookupMember(unionQual, memberName) with
                | ValueSome m when not m.IsStatic ->
                    // Single-candidate commit — freshen method typars per use site, as the
                    // external-`TyClass` arm above (shared defect: `openSignature` leaves an
                    // inert method-typar marker that cross-contaminates across call sites).
                    let memberSig =
                        ExternalSymbols.instantiateSignature m (args.AsSpan().ToArray()) ctx.CurrentLevel

                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            Storage = m.Storage
                            Signature = memberSig
                            OptionalDefaults = m.OptionalDefaults
                        }
                    )

                    memberSig
                | _ ->
                    // The provider knows the union but not this member → a real
                    // member miss; otherwise the type itself is unknown.
                    match ctx.Provider.TryLookupType unionQual with
                    | ValueSome(ExternalTypeShape.Union _) ->
                        errorTy ctx diagKey (sprintf "Type '%s' has no instance member '%s'" unionQual memberName)
                    | _ -> errorTy ctx diagKey (sprintf "Unknown union type '%s'" unionQual)
        | TyVar tv ->
            let root = UnionFind.find tv

            // The receiver is a generic typar (`'T`) constrained to
            // an interface (`'T :> IFace`). The typar never grounds to a nominal, so
            // the `PendingDotAccess` drain would never fire (and the binding wouldn't
            // generalise); instead resolve the member *now* through the interface the
            // typar is coerced to. The constraint's target zonks to the interface's
            // `TyClass` (a project-local interface is registered in `Types.Class`
            // with `IsInterface` set). Record the interface key so Freeze mints a
            // `CallVia.Interface` dispatch (codegen → `constrained. callvirt`).
            match tryTyparInterfaceMember ctx diagKey root memberName with
            | ValueSome ty -> ty
            | ValueNone ->
                let resultTv = freshTyVar ctx

                let access =
                    {
                        MemberName = memberName
                        UseKey = diagKey
                        ResultTv = resultTv
                    }

                root.PendingDotAccess <- access :: root.PendingDotAccess
                TyVar resultTv
        // `arr.Length` on a rank-1 intrinsic array resolves to the core
        // `GetArrayLength` inline function (scheme `'T[] -> int`), grounding the
        // call so `InlineExpansion` can splice the source `ldlen` — the same path as
        // `arr.[i]`/`GetArray`. No member metadata on the intrinsic `'T[]`.
        // An instance member on an *intrinsic* receiver whose `(# "…" #)` binding
        // maps it to a BCL type (`"hello".TryCopyTo(span)` / `s.Length`): resolve
        // through the provider by the canonical BCL name (`IntrinsicBclMember`,
        // routed via `prim-types-string.fs`), recording it for Freeze exactly as
        // the external `TyClass` arm does. The single-pick member suffices for a
        // name with one overload; an arg-overloaded name (`string.CopyTo`) is
        // picked arg-aware earlier by `tryInferExternalInstanceMethodCall`. A
        // member-name miss declines the pattern, so arrays (`"[]"`) / byref
        // (`"&"`) — and any unknown member — fall through to the arms below.
        | IntrinsicBclMember ctx memberName (clsQual, args, m) ->
            if not m.IsStatic then
                let memberSig = ExternalSymbols.openSignature m (args.AsSpan().ToArray())

                ctx.Resolution.ExternalAccess.Set(
                    diagKey,
                    {
                        Key = m.Key
                        IsStatic = false
                        Storage = m.Storage
                        Signature = memberSig
                        OptionalDefaults = m.OptionalDefaults
                    }
                )

                memberSig
            else
                errorTy ctx diagKey (sprintf "Type '%s' has no instance member '%s'" clsQual memberName)
        | TyConst(name, _) when name = RuntimeNames.arrayName 1 && memberName = "Length" ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetArrayLength" with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx diagKey (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel) (TyFun(rTy, resultTy))
                resultTy
            | ValueNone ->
                errorTy ctx diagKey "Array 'Length' intrinsic 'GetArrayLength' is not in scope (Vesper.Core missing?)"
        | _ -> errorTy ctx diagKey (sprintf "Cannot read member '%s' from non-record non-class type" memberName)

    and inferFieldAccess
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let fieldName = ctx.NameOf fieldTok
        let rTy = infer ctx receiver
        resolveFieldStep ctx key rTy fieldName

    /// `arr.[i]` — the receiver is a rank-1 array `'T[]` and the index an `int`;
    /// the result is the element type. The element stays a fresh var unified
    /// against the receiver so an as-yet-unresolved receiver (a bare `[]`) is
    /// pinned from context the same way an array literal is.
    and inferIndexedLookup
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (index: Expr<SyntaxToken>)
        : SemType =
        let recvTy = infer ctx receiver
        let idxTy = infer ctx index

        // `arr.[i]` resolves to the core `GetArray` inline function, exactly as an
        // operator resolves through `inferInfix`: instantiate its scheme
        // (`'T[] -> int -> 'T`) and unify against `arr -> idx -> result`. That pins
        // the array element type, the `int` index, and the result — and (like every
        // resolved call) grounds the types so `InlineExpansion` can splice the
        // source `ldelem` at the use site. The mnemonic never originates here.
        let getArrayIndex () =
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetArray" with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)

                unify
                    ctx
                    key
                    (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                    (TyFun(recvTy, TyFun(idxTy, resultTy)))

                resultTy
            | ValueNone -> errorTy ctx key "Array indexing intrinsic 'GetArray' is not in scope (Vesper.Core missing?)"

        // String indexing (`s.[i]`) on a target with no BCL `string` metadata (JS):
        // `get_Chars` does not resolve, so route to the `GetString` inline intrinsic —
        // the string analogue of `GetArray`, with scheme `string -> int -> char`. It
        // unifies the receiver against `string` (NOT `'T[]`), so the spurious
        // string-vs-array mismatch the `GetArray` fallback would raise never happens.
        // On CLR `get_Chars` resolves first, so a string never reaches here.
        let getStringIndex () =
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetString" with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)

                unify
                    ctx
                    key
                    (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                    (TyFun(recvTy, TyFun(idxTy, resultTy)))

                resultTy
            | ValueNone -> getArrayIndex ()

        // A non-class, non-`get_Chars` receiver: a `string` routes to `GetString`,
        // everything else (arrays, still-free metavars) to `GetArray`.
        let stringOrArrayIndex () =
            match resolveStep recvTy with
            | TyConst("string", _) -> getStringIndex ()
            | _ -> getArrayIndex ()

        // An indexer on an *external* receiver is its BCL `get_Item` (or, for a
        // `string` intrinsic, `get_Chars`) accessor — it can't go through
        // `GetArray`/`ldelem`. Resolve it through the provider, record it in
        // `ExternalAccess`, and return the *element* type. The accessor's return is
        // either by-ref (`Span<char>.get_Item : T&`, needs an `ldobj` deref at
        // Freeze) or by-value (`string.get_Chars : char`, `List<T>.get_Item : T`,
        // `ITuple.get_Item : obj`) — detect it from the resolved signature so the
        // unify RHS (and Freeze's lowering) match. A project-local class, an
        // intrinsic array, or a still-free receiver keeps the `GetArray` path.
        let resolveExternalIndexer (clsQual: string) (clsArgs: SemType[]) (accessorName: string) : SemType voption =
            match ctx.Provider.TryLookupMember(clsQual, accessorName) with
            | ValueSome m when not m.IsStatic ->
                let memberSig = ExternalSymbols.openSignature m clsArgs

                ctx.Resolution.ExternalAccess.Set(
                    key,
                    {
                        Key = m.Key
                        IsStatic = false
                        Storage = MemberStorage.Method
                        Signature = memberSig
                        // An indexer's accessor takes no omittable optionals.
                        OptionalDefaults = []
                    }
                )

                // The accessor is `idx -> ret`; `ret` is `T&` (byref) or `T` (value).
                let retIsByref =
                    match memberSig with
                    | TyFun(_, TyConst(n, _)) when n = RuntimeNames.byrefName -> true
                    | _ -> false

                let resultTy = TyVar(freshTyVar ctx)

                let rhsRet =
                    if retIsByref then
                        TyConst(RuntimeNames.byrefName, EqArray.singleton resultTy)
                    else
                        resultTy

                unify ctx key memberSig (TyFun(idxTy, rhsRet))
                ValueSome resultTy
            | _ -> ValueNone

        // An index-signature receiver (`{ [k: K]: V }` on an external interface / class /
        // anonymous object) reads through the `GetIndex` intrinsic — the `$0[$1]` bracket
        // form, the JS analogue of `GetArray`'s `ldelem`. There is no `get_Item` method on
        // such an object (bracket IS the accessor), so this fires BEFORE the `get_Item` /
        // `getArrayIndex` attempts. `GetIndex`'s scheme is `'T -> 'K -> 'V` with three
        // INDEPENDENT typars, so unifying it against `recv -> idx -> result` alone leaves
        // `'V` free — the declared key/value are pinned separately from the provider entry.
        let tryIndexSignature (clsQual: string) (clsArgs: SemType[]) : SemType voption =
            match ctx.Provider.TryLookupIndexSignature clsQual with
            | [] -> ValueNone
            | entries ->
                // Realise each entry's key/value template against the receiver's args
                // (`Dict<number>`'s value `'V` → `number`).
                let realised =
                    entries
                    |> List.map (fun (kF, vF) ->
                        FrozenTypeBridge.instantiateDeclaring kF clsArgs,
                        FrozenTypeBridge.instantiateDeclaring vF clsArgs
                    )

                // Select the entry whose key type matches the index expression's type — a
                // string index picks the string-keyed sig, a numeric one the number-keyed
                // sig. A single entry is used as-is; an unresolved / non-matching index
                // defaults to the first (Node's `process.env` is a single string entry).
                let keyTy, valTy =
                    match realised with
                    | [ single ] -> single
                    | _ ->
                        let matched =
                            match resolveStep idxTy with
                            | TyConst(idxName, _) ->
                                realised
                                |> List.tryFind (fun (k, _) ->
                                    match resolveStep k with
                                    | TyConst(kn, _) -> kn = idxName
                                    | _ -> false
                                )
                            | _ -> None

                        match matched with
                        | Some e -> e
                        | None -> List.head realised

                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetIndex" with
                | ValueSome sym ->
                    let resultTy = TyVar(freshTyVar ctx)

                    unify
                        ctx
                        key
                        (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                        (TyFun(recvTy, TyFun(idxTy, resultTy)))

                    // Pin `'K`/`'V` (which the generic scheme leaves free) to the declared
                    // key/value, so `x.[k]` reads the declared element type (`string |
                    // undefined`), not a fresh var.
                    unify ctx key idxTy keyTy
                    unify ctx key resultTy valTy
                    ValueSome resultTy
                | ValueNone ->
                    ValueSome(
                        errorTy ctx key "Index-signature intrinsic 'GetIndex' is not in scope (Vesper.Core missing?)"
                    )

        match resolveStep recvTy with
        | TyClass(clsKey, clsArgs) when (TypeRegistry.tryClassByKey ctx.Types clsKey).IsNone ->
            let clsQual = SymbolKeyOps.qualifiedName clsKey
            let clsArgsArr = clsArgs.AsSpan().ToArray()

            match tryIndexSignature clsQual clsArgsArr with
            | ValueSome resultTy -> resultTy
            | ValueNone ->
                match resolveExternalIndexer clsQual clsArgsArr "get_Item" with
                | ValueSome resultTy -> resultTy
                | ValueNone -> getArrayIndex ()
        // A rank-1 array `'T[]` (a bare `TyConst("[]", [elem])`, NOT a `TyClass`) reads
        // through the intrinsic array's `get_Item` member accessor — the member-inline
        // twin of the free `GetArray`. The receiver-side lookup key is the array's bare
        // member-contract identity, `RuntimeNames.arrayContractName` (see there for why
        // it's the backtick-escaped `` ``[]`` `` and how the consumer/harvest/receiver
        // keys agree). `TryLookupMember` lands the identical member the contract and the
        // harvest store share, and Freeze lowers it through
        // `TExpr.ExternalMember(get_Item)` (whose harvested `ldelem` body splices to the
        // same `arr[i]`). A MISS — the contract half absent (a non-JS target, or a key
        // disagreement) — falls back to the free `GetArray` path UNCHANGED, so nothing
        // regresses if resolution doesn't hit.
        | TyConst(name, elemArgs) when name = RuntimeNames.arrayName 1 ->
            match resolveExternalIndexer RuntimeNames.arrayContractName (elemArgs.AsSpan().ToArray()) "get_Item" with
            | ValueSome resultTy -> resultTy
            | ValueNone -> getArrayIndex ()
        | _ ->
            // An intrinsic receiver mapped to a BCL type — `string` (`s.[i]`), whose
            // indexer accessor is `System.String.get_Chars(int) : char`. When that does
            // not resolve (the JS target — `string`'s platform repr is the bare
            // `"string"`, so `tryExternalReceiver` declines), a `string` falls to the
            // `GetString` intrinsic, anything else to `GetArray`.
            match tryExternalReceiver ctx recvTy with
            | ValueSome(clsQual, clsArgs) ->
                match resolveExternalIndexer clsQual (clsArgs.AsSpan().ToArray()) "get_Chars" with
                | ValueSome resultTy -> resultTy
                | ValueNone -> stringOrArrayIndex ()
            | ValueNone -> stringOrArrayIndex ()

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`, whose
    /// head segment NameResolution resolved as a local binding; the remaining
    /// segments are a field-access chain.
    and inferLongIdentFieldChain (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueSome rb -> instantiateBinding ctx rb
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Diagnose against the LongIdent's overall key — there's no
            // separate sub-expression NodeKey for an intermediate segment.
            currTy <- resolveFieldStep ctx key currTy segName

        currTy

    /// The type of a folded field chain MINUS its *last* segment — the receiver
    /// of a folded-LongIdent instance method call (`w.Write(arg)` parses with
    /// `fn = LongIdent [w; Write]`, the member being the last segment). Mirrors
    /// `inferLongIdentFieldChain` but stops one short, so the last segment can be
    /// resolved arg-aware as an overloaded instance method instead of falling to
    /// the single-pick field step. The head is assumed a local binding (the
    /// caller guards on it).
    and inferLongIdentReceiverPrefix (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueSome rb -> instantiateBinding ctx rb
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 2 do
            currTy <- resolveFieldStep ctx key currTy (ctx.NameOf li.Idents.[i])

        currTy
