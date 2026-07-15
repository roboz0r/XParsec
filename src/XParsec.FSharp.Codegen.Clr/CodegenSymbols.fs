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

    /// The shape a resolved `SymbolKey` names, probing the qualified compiled name then
    /// its bare (arity-suffix-stripped) form — the two registration conventions the
    /// metadata layer (suffixed) and the contract layer (bare) use. The SINGLE spelling of
    /// that probe: `ClrEnv`'s type/member refs and `CapabilityCoSlots`' capability
    /// recognition both go through it.
    let lookupTypeByKey (symbols: ICodegenSymbols) (key: SymbolKey) : ExternalTypeShape voption =
        let qual = SymbolKeyOps.qualifiedName key

        match symbols.TryLookupType qual with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let bare = SymbolKeyOps.bareName qual

            if bare = qual then
                ValueNone
            else
                symbols.TryLookupType bare

    let ofProvider (provider: IExternalSymbolProvider) : ICodegenSymbols =
        { new ICodegenSymbols with
            member _.TryLookupType name = provider.TryLookupType name

            // `ICodegenSymbols` is string-addressed; the store face is key-addressed, so
            // bridge by minting the lookup key from the compiled name. Arity 0 is lossless
            // for an already-suffixed generic name (`arityName` is a no-op on it), and the
            // key needs no home: identity is the qualified name alone.
            member _.TryLookupMember(typeName, memberName) =
                provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey typeName 0, memberName)

            member _.TryLookupMembers(typeName, memberName) =
                provider.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey typeName 0, memberName)

            member _.TryLookupOpenSignature name =
                match provider.TryLookup name with
                | ValueNone -> ValueNone
                | ValueSome sym ->
                    match sym.Origin.Home with
                    // A project-local symbol (no home assembly) the provider never owns:
                    // mirror the old `emitExternalCall` guard and let the caller fall back.
                    | Origin.Unstamped -> ValueNone
                    | Origin.InAssembly _ ->
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
