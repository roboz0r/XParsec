namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// The codegen-facing adapter over the front end's `IExternalSymbolProvider`.
/// `ClrEnv` holds an `ICodegenSymbols` rather than the
/// full provider, so the emission code can no longer reach `Instantiate` / constraints /
/// inline bodies — it sees only the type/member shapes (whose `FrozenType` templates it
/// reads) and the open signature of a module-level function. This is the "dual view over
/// one provider": the same backing `IExternalSymbolProvider`, projected to the narrow
/// emission surface. The open-signature projection runs `Inline.openMethodSignature`
/// (which lives past `ExternalSymbols` in the compile order, hence the adapter lands here
/// rather than in the front-end module) and freezes its `TyVar`-free result.
module CodegenSymbols =

    /// Reconcile the bare-vs-arity-suffixed registration split a key can land under: a
    /// provider may key a generic type BARE (`Vesper.Option`, contract layer) or
    /// arity-suffixed (`Vesper.Option`1`, metadata layer). Probe the key as-is, then a
    /// name-equal arity-0 key (which renders bare). The SINGLE place the two conventions
    /// meet; `probe` is the single-probe channel it retries.
    let private reconciledLookup
        (probe: SymbolKey -> ExternalTypeShape voption)
        (key: SymbolKey)
        : ExternalTypeShape voption =
        match probe key with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let qual = SymbolKeyOps.qualifiedName key
            let bare = SymbolKeyOps.bareName qual

            if bare = qual then
                ValueNone
            else
                probe (SymbolKeyOps.qualifiedTypeKey bare 0)

    /// The shape a resolved `SymbolKey` names, reconciling the bare-vs-arity-suffixed
    /// registration split. The SINGLE spelling of that probe: `ClrEnv`'s type/member refs
    /// and `CapabilityCoSlots`' capability recognition both go through it.
    let lookupTypeByKey (symbols: ICodegenSymbols) (key: SymbolKey) : ExternalTypeShape voption =
        reconciledLookup symbols.TryLookupType key

    let ofProvider (provider: IExternalSymbolProvider) : ICodegenSymbols =
        // The store's own key-addressed type-shape probe (`ExternalTypeShape voption`
        // directly, unlike the resolver face's `struct (TypeKey * shape)`), reconciled the
        // same way `lookupTypeByKey` reconciles the codegen face — so the capability rebase
        // below resolves an `IntrinsicInterface` face under either registration convention.
        let storeShape (k: SymbolKey) : ExternalTypeShape voption = provider.TryLookupType k

        { new ICodegenSymbols with
            member _.TryLookupType key = provider.TryLookupType key

            member _.TryLookupMemberByKey key = provider.TryLookupMemberByKey key

            member _.TryLookupCtor(declKey, chosen, arity) =
                match chosen with
                | ValueSome ck ->
                    // The recorded identity IS the ctor. A heritable primitive records it
                    // against the CANON (`Vesper.exn`) while emission runs against the
                    // platform class; the metadata layer canonicalises ctor PARAM types but
                    // keys the decl under the platform type, so only the decl differs — rebase
                    // it through `IntrinsicForwardRepr` and the by-key fetch is exact. A
                    // normal external ctor's decl is no canon, so the rebase misses and the
                    // direct fetch already resolved it.
                    let mk = SymbolKeyOps.asMemberKey "ClrProvider: external ctor" ck

                    provider.TryLookupMemberByKey mk
                    |> ValueOption.orElseWith (fun () ->
                        match provider.IntrinsicForwardRepr.TryGetValue(SymbolKey.Type mk.Decl) with
                        | true, platformRepr ->
                            provider.TryLookupMemberByKey
                                { mk with
                                    Decl = SymbolKeyOps.qualifiedTypeKeyOf platformRepr 0
                                }
                        | _ -> ValueNone
                    )
                | ValueNone ->
                    // A synthesised ctor with no recorded identity (printf's scratch
                    // `StringBuilder` `new()`, the `PrintfFormat` literal `new(text)`): the
                    // sole ctor of this arity. Not overload disambiguation — these types have
                    // no same-arity ambiguity; the pick runs behind the seam and one member
                    // leaves it.
                    provider.TryLookupMembers(declKey, ".ctor")
                    |> Array.tryFind (fun m -> m.Key.ArgSig.Length = arity)
                    |> function
                        | Some m -> ValueSome m
                        | None -> ValueNone

            // A member resolved against a capability face (`enumerator<'T>.MoveNext`)
            // reconciles to a BCL platform face (`IEnumerator`1`); but the member's TRUE
            // declaring type may be a *base* of that face (`MoveNext` on the non-generic
            // `IEnumerator`), so a face-parented member-ref faults at runtime. Re-resolve
            // on the platform face's metadata hierarchy — whose `TryLookupMembers` reports
            // each member's real declaring type — and rebase onto the base member of the
            // requested `kind`. `ValueNone` when the declaring type is not a capability
            // interface, the member is declared on the face itself (no rebase), or no base
            // member of that `kind` exists (decline rather than mint a wrong ref).
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

                        if members |> Array.exists (fun m -> declaredOn m = platform) then
                            ValueNone
                        else
                            match members |> Array.tryFind (fun m -> m.Key.Kind = kind) with
                            | Some m -> ValueSome(SymbolKey.Member m.Key)
                            | None -> ValueNone
                    | _ -> ValueNone
                | _ -> ValueNone

            member _.TryLookupOpenSignature key =
                match provider.TryLookupByKey key with
                | ValueNone -> ValueNone
                | ValueSome sym ->
                    // A project-local symbol (no home assembly) the provider never owns:
                    // mirror the old `emitExternalCall` guard and let the caller fall back.
                    if not sym.Origin.Home.IsStamped then
                        ValueNone
                    else
                        let os = Inline.openMethodSignature sym

                        ValueSome
                            {
                                Origin = sym.Origin
                                Signature = os.Signature
                                MethodTyparArity = os.MethodTyparArity
                                ValRepr = sym.ValRepr
                                Constraints = os.Constraints
                            }

            member _.IntrinsicForwardRepr = provider.IntrinsicForwardRepr
        }
