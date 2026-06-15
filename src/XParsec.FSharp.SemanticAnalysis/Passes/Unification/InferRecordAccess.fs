namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
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
        | Some m -> instantiateMemberCall ctx (typeParams, args) m.MethodTypeParams m.Type
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
            // Local lookup by the bare simple name; the external provider by the
            // qualified compiled name (an external `TyClass` carries a qualified key).
            let clsSimple = SymbolKeyOps.simpleName clsKey

            match TypeRegistry.tryClass ctx.Types clsSimple with
            | ValueSome info ->
                // Walk the inheritance chain (derived members shadow inherited).
                // On a total miss, fall back to the single-class diagnostic so
                // the static-access hint still references the receiver's own
                // class rather than some ancestor.
                match tryClassChainMember ctx clsSimple args memberName with
                | ValueSome ty -> ty
                | ValueNone ->
                    // An explicit `val x: T` instance field read (struct enumerator
                    // state, B-7-adjacent). Instantiate the field's declared type
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

                match ctx.Provider.TryLookupMember(clsQual, memberName) with
                | ValueSome m when not m.IsStatic ->
                    let memberSig = ExternalSymbols.openSignature m (args.AsSpan().ToArray())

                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            IsProperty = m.IsProperty
                            Signature = memberSig
                            OptionalDefaults = m.OptionalDefaults
                        }
                    )

                    memberSig
                | _ -> errorTy ctx diagKey (sprintf "Unknown class type '%s'" clsQual)
        | TyUnion(unionKey, args) ->
            // Union instance member access (P3d.3) — mirrors the `TyClass` arm
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
                // `TyClass` arm does (vesper-lib-test-plan Gap 2 Layer A).
                let unionQual = SymbolKeyOps.qualifiedName unionKey

                match ctx.Provider.TryLookupMember(unionQual, memberName) with
                | ValueSome m when not m.IsStatic ->
                    let memberSig = ExternalSymbols.openSignature m (args.AsSpan().ToArray())

                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            IsProperty = m.IsProperty
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
                        IsProperty = m.IsProperty
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
                unify ctx diagKey (sym.Instantiate ctx.CurrentLevel) (TyFun(rTy, resultTy))
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
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(recvTy, TyFun(idxTy, resultTy)))
                resultTy
            | ValueNone -> errorTy ctx key "Array indexing intrinsic 'GetArray' is not in scope (Vesper.Core missing?)"

        // An indexer on an *external* class (`span.[i]` on a `Span<char>`) is the
        // BCL `get_Item` accessor — for a ref struct it is `get_Item(i) : T&` with no
        // by-value accessor, so it can't go through `GetArray`/`ldelem`. Resolve it
        // through the provider, record it in `ExternalAccess` (`FreezeExpr` lowers it
        // like an external instance call + a byref deref), and return the *element*
        // type — the by-ref is erased at the value position. A project-local class,
        // an intrinsic array, or a still-free receiver keeps the `GetArray` path.
        match resolveStep recvTy with
        | TyClass(clsKey, clsArgs) when (TypeRegistry.tryClass ctx.Types (SymbolKeyOps.simpleName clsKey)).IsNone ->
            let clsQual = SymbolKeyOps.qualifiedName clsKey

            match ctx.Provider.TryLookupMember(clsQual, "get_Item") with
            | ValueSome m when not m.IsStatic ->
                let memberSig = ExternalSymbols.openSignature m (clsArgs.AsSpan().ToArray())

                ctx.Resolution.ExternalAccess.Set(
                    key,
                    {
                        Key = m.Key
                        IsStatic = false
                        IsProperty = false
                        Signature = memberSig
                        // An indexer's `get_Item` takes no omittable optionals.
                        OptionalDefaults = []
                    }
                )

                // `get_Item : idx -> T&`; unify against `idx -> (resultTy)&` to pin
                // the index type and read out the element `resultTy` (byref erased).
                let resultTy = TyVar(freshTyVar ctx)
                let byrefTy = TyConst(RuntimeNames.byrefName, EqArray.singleton resultTy)
                unify ctx key memberSig (TyFun(idxTy, byrefTy))
                resultTy
            | _ -> getArrayIndex ()
        | _ -> getArrayIndex ()

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
