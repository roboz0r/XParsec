namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// The narrow codegen-facing view of an `IExternalSymbolProvider`: type/member shapes and
/// a module function's open signature, but not `Instantiate` / constraints / inline bodies.
module CodegenSymbols =

    /// A provider may key a generic type BARE (`Vesper.Option`, contract layer) or
    /// arity-suffixed (`Vesper.Option`1`, metadata layer): probe the key as-is, then a
    /// name-equal arity-0 key, which renders bare.
    let private reconciledLookup (probe: SymbolKey -> 'a voption) (key: SymbolKey) : 'a voption =
        match probe key with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let qual = SymbolKeyOps.qualifiedName key
            let bare = SymbolKeyOps.bareName qual

            if bare = qual then
                ValueNone
            else
                probe (SymbolKeyOps.qualifiedTypeKey bare 0)

    /// The shape a resolved `SymbolKey` names, over either registration convention.
    let lookupTypeByKey (symbols: ICodegenSymbols) (key: SymbolKey) : ExternalTypeShape voption =
        reconciledLookup symbols.TryLookupType key

    /// The settled layout of a REFERENCED type, over either registration convention:
    /// `Unanswered` for a non-type key, and for a name this compilation emits itself.
    let externalLayout (symbols: ICodegenSymbols) (key: SymbolKey) : TypeLayout =
        let settled (k: SymbolKey) =
            match k with
            | SymbolKey.Type t -> symbols.IsValueType t
            | _ -> ValueNone

        reconciledLookup settled key |> TypeLayout.ofAnswer

    /// `false` is the floor: a `VALUETYPE`/`CLASS` tag is emitted for a non-type key and for
    /// an unanswered type alike.
    let isValueType (symbols: ICodegenSymbols) (key: SymbolKey) : bool =
        externalLayout symbols key = TypeLayout.Value

    let ofProvider (provider: IExternalSymbolProvider) : ICodegenSymbols =
        let storeShape (k: SymbolKey) : ExternalTypeShape voption = provider.TryLookupType k

        { new ICodegenSymbols with
            member _.TryLookupType key = provider.TryLookupType key

            member _.TryLookupMemberByKey key = provider.TryLookupMemberByKey key

            member _.TryLookupCtor(declKey, chosen, arity) =
                match chosen with
                | ValueSome ck ->
                    // The recorded identity IS the ctor, so a by-key fetch, never a re-pick that
                    // could disagree with the overload the front end committed to. A heritable
                    // primitive's key arrives platform-valid, so nothing is rebased here.
                    provider.TryLookupMemberByKey(SymbolKeyOps.asMemberKey "ClrProvider: external ctor" ck)
                | ValueNone ->
                    // A ctor node with no recorded identity: the printf `%a`/`%t` scratch
                    // (`new StringBuilder()`) or an external-base `inherit exn(msg)`, which has
                    // no `TExpr.New`. Arity alone picks it because neither shape is overloaded.
                    provider.TryLookupMembers(declKey, ".ctor")
                    |> EqArray.tryFind (fun m -> m.Key.ArgSig.Length = arity)

            // `enumerator<'T>.MoveNext` reconciles to `IEnumerator`1`, but `MoveNext` is
            // declared on the non-generic `IEnumerator`, and a member-ref parented on the
            // generic one faults at runtime. `ValueNone` ⇒ nothing to rebase.
            member _.TryRebaseCapabilityMember key =
                match key with
                | SymbolKey.Member {
                                       Decl = declKey
                                       Name = memberName
                                       Kind = kind
                                   } ->
                    match reconciledLookup storeShape (SymbolKey.Type declKey) with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                        let members =
                            provider.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey platform 0, memberName)

                        let declaredOn (m: ExternalMember) = SymbolKeyOps.typeMetaName m.Key.Decl

                        if members |> EqArray.exists (fun m -> declaredOn m = platform) then
                            ValueNone
                        else
                            members
                            |> EqArray.tryFind (fun m -> m.Key.Kind = kind)
                            |> ValueOption.map (fun m -> SymbolKey.Member m.Key)
                    | _ -> ValueNone
                | _ -> ValueNone

            member _.TryLookupOpenSignature key =
                match provider.TryLookupByKey key with
                | ValueNone -> ValueNone
                | ValueSome sym ->
                    // A project-local symbol has no home assembly for a ref to name; the
                    // caller falls back.
                    if sym.Origin.Home = Origin.Unstamped then
                        ValueNone
                    else
                        let os = OpenSignature.ofSymbol sym

                        ValueSome
                            {
                                Origin = sym.Origin
                                Signature = os.Signature
                                MethodTyparArity = os.MethodTyparArity
                                ValRepr = sym.ValRepr
                                Constraints = os.Constraints
                            }

            member _.TryPlatformRepr canon =
                IntrinsicTypeMap.tryPlatformRepr canon provider.IntrinsicTypeMap

            // The target's layout overrides the declaration's request, so it leads.
            member _.IsValueType key =
                match provider.IsValueType key with
                | ValueSome _ as settled -> settled
                | ValueNone ->
                    provider.TryLookupType(SymbolKey.Type key)
                    |> ValueOption.bind ExternalSymbols.declaredValueType

            member _.Platform = provider.Platform
        }
