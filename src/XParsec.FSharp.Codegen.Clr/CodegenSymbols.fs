namespace XParsec.FSharp.Codegen.Clr

open Vesper
open XParsec.FSharp.SemanticAnalysis

/// The narrow codegen-facing view of an `IExternalSymbolProvider`: type/member shapes and
/// a module function's open signature, but not `Instantiate` / constraints / inline bodies.
module CodegenSymbols =

    /// The settled layout of `key`: the target's, else the `[<Struct>]` its declaration asked
    /// for. `AnalysedAssembly.Visibility` publishes this compilation's own declarations, so
    /// they settle here too. `Unsettled` for a non-type key.
    let externalLayout (symbols: ICodegenSymbols) (key: TypeKey) : TypeLayout =
        symbols.IsValueType key |> TypeLayout.ofSettled

    /// `false` is the floor: an `Unsettled` layout is tagged `CLASS`.
    let isValueType (symbols: ICodegenSymbols) (key: TypeKey) : bool =
        externalLayout symbols key = TypeLayout.Value

    let ofProvider (provider: IExternalSymbolProvider) : ICodegenSymbols =
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
                    |> Block.tryFind (fun m -> m.Key.ArgSig.Length = arity)

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
                    match provider.TryLookupType declKey with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                        let members =
                            provider.TryLookupMembers(SymbolKeyOps.qualifiedTypeKeyOf platform.Value 0, memberName)

                        let declaredOn (m: ExternalMember) = SymbolKeyOps.typeMetaName m.Key.Decl

                        if members |> Block.exists (fun m -> declaredOn m = platform.Value) then
                            ValueNone
                        else
                            members
                            |> Block.tryFind (fun m -> m.Key.Kind = kind)
                            |> ValueOption.map (fun m -> SymbolKey.Member m.Key)
                    | _ -> ValueNone
                | _ -> ValueNone

            member _.TryLookupOpenSignature key =
                match provider.TryLookupByKey key with
                | ValueNone -> ValueNone
                | ValueSome sym ->
                    // A project-local symbol has no home assembly for a ref to point to; the
                    // caller falls back.
                    if sym.Origin.Home = SymbolHome.Unstamped then
                        ValueNone
                    else
                        ValueSome
                            {
                                Origin = sym.Origin
                                Signature = sym.Scheme
                                Scheme = sym.Generics
                                EmittedName = sym.EmittedName
                                ValRepr = sym.ValRepr
                            }

            member _.DeclarationsOf m = provider.Scope.DeclarationsOf m

            member _.TryPlatformTypeId canon =
                IntrinsicTypeMap.tryPlatformTypeId canon provider.IntrinsicTypeMap

            // The target's layout overrides the declaration's request, so it leads.
            member _.IsValueType key =
                match provider.IsValueType key with
                | ValueSome _ as settled -> settled
                | ValueNone -> provider.TryLookupType key |> ValueOption.bind ExternalSymbols.declaredValueType

            member _.Platform = provider.Platform
        }
