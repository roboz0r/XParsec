namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationTranslate
open NameResolutionContainers
open NameResolutionLongIdent

// Resolving an `inherit` clause's PARENT, at registration time and in the scope the clause is
// written. The parent is an arbitrary written name reached through the same opens-aware engine
// as any other type reference, so the miss cases are as much of the work as the hits.

module NameResolutionInheritParent =

    /// An `inherit` parent resolved through a provider, rather than the project-local type
    /// registry. The arm is fixed at construction, so a caller matches on it rather than
    /// re-inspecting a surface.
    [<RequireQualifiedAccess>]
    type private ProviderBase =
        /// A class published by a prior file of this assembly, or by a reference.
        | Class of TypeKey
        /// A heritable primitive whose contract declares `.ctor`s (`exn`): the derived class
        /// inherits by CANON, and an `inherit` argument list checks against those ctors.
        | HeritableCanon of IntrinsicIdentity
        /// A heritable primitive with no declared ctor (`Attribute`): the derived class
        /// inherits the PLATFORM type the identity names.
        | HeritablePlatform of IntrinsicIdentity
        /// An external interface, which `inherit` cannot name.
        | Interface of TypeKey

    /// A published type at the written arity, else a heritable primitive. Any other shape
    /// declines, so the open-prefix scan continues past it.
    let private providerBaseOf (arity: int) (key: TypeKey) (shape: ExternalTypeShape) : ProviderBase voption =
        match shape with
        | ExternalTypeShape.Class info when info.TyparArity = arity ->
            if info.IsInterface then
                ValueSome(ProviderBase.Interface key)
            else
                ValueSome(ProviderBase.Class key)
        // A capability at ANY arity: the rejection is the same whatever was written.
        | ExternalTypeShape.IntrinsicInterface _ -> ValueSome(ProviderBase.Interface key)
        | _ ->
            ExternalSymbols.intrinsicClassOf shape
            |> ValueOption.map (fun (struct (id, surface)) ->
                if surface.Members |> EqArray.exists (fun m -> m.Name = ".ctor") then
                    ProviderBase.HeritableCanon id
                else
                    ProviderBase.HeritablePlatform id
            )

    /// Resolve an `inherit` clause's parent type to a nominal. The caller runs it under the
    /// derived class's typar scope, so a `'a` in the clause is the class's prototype TyVar.
    /// Diagnoses (and returns `ValueNone`) when the parent is an interface, a non-class type,
    /// an unknown name, a multi-segment name, or a shape with no nominal head; the
    /// diagnostic is reported at `inhTok` where the written shape retains no name token.
    let resolveInheritParent (ctx: PassContext) (inhTok: SyntaxToken) (t: Type<SyntaxToken>) : BaseParent voption =
        let rec nameAndArgs (t: Type<SyntaxToken>) : (LongIdent<SyntaxToken> * SemType list) voption =
            match t with
            | Type.ParenType(typ = inner) -> nameAndArgs inner
            | Type.NamedType li -> ValueSome(li, [])
            | Type.GenericType(longIdent = li; typeArgs = args) ->
                let targs =
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateType ctx at
                            | TypeArg.Measure _ -> ()
                    ]

                ValueSome(li, targs)
            | Type.SuffixedType(baseType = bt; longIdent = li) -> ValueSome(li, [ translateType ctx bt ])
            | _ -> ValueNone

        let diagnose (tok: SyntaxToken) (kind: Kind) = ctx.Report(tok, kind)

        match nameAndArgs t with
        | ValueNone ->
            // A written shape with no name to resolve (`inherit (int * int)`): classify what
            // it translates to, so the rejection names the kind it is.
            BaseEligibility.classify
                (BaseEligibility.isInterfaceKey ctx)
                (BaseEligibility.isHeritableCanon ctx)
                (translateType ctx t)
            |> BaseEligibility.admit ctx inhTok
        | ValueSome(li, targs) ->
            let nameTok = li.Idents.[li.Idents.Length - 1]
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeNamed

            if li.Idents.Length <> 1 then
                let qual = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                diagnose nameTok (Kind.NotYetSupported(sprintf "inheriting from a qualified base type '%s'" qual))
                ValueNone
            else
                let name = ctx.NameOf nameTok

                let reject (verdict: BaseVerdict) =
                    BaseEligibility.admit ctx nameTok verdict

                // The contract's ctor-bearing intrinsic surface for the name, as its canon.
                let tryCtorBearingCanon () =
                    match
                        tryPickExternalWritten
                            ctx
                            (ctx.UseSiteAt diagKey)
                            (WrittenArity.Exact targs.Length)
                            (fun _ shape -> ExternalSymbols.intrinsicClassOf shape)
                            Qualifier.Bare
                            name
                    with
                    | ValueSome(struct (id, surface)) when surface.Members |> EqArray.exists (fun m -> m.Name = ".ctor") ->
                        ValueSome(BaseParentG.PrimitiveCanon(NominalG.ofConst id.Canon (EqArray.ofList targs)))
                    | _ -> ValueNone

                // A heritable base's platform type id → its external `TyClass`. A sentinel id
                // (`"!Vesper.Attribute"`) denotes no external type, so a base with declared
                // ctors falls back to inheriting by canon, and only a base with neither gets
                // the "did not resolve" diagnostic.
                let typeIdToExternalBase (typeId: PlatformTypeId) =
                    match ExternalSymbols.tryMetaTypeAt ctx.Provider typeId.Value targs.Length with
                    | ValueSome(struct (extKey, _)) ->
                        ValueSome(BaseParentG.Class(NominalG.ofClass extKey (EqArray.ofList targs)))
                    | ValueNone ->
                        match tryCtorBearingCanon () with
                        | ValueSome t -> ValueSome t
                        | ValueNone ->
                            diagnose
                                nameTok
                                (Kind.Message(
                                    sprintf
                                        "Cannot inherit from external base '%s': its representation '%s' did not resolve to a known external type (is a package dependency missing?)"
                                        name
                                        typeId.Value
                                ))

                            ValueNone

                // The name is not a project-local class: a class published by a prior file or a
                // reference, or a heritable primitive published by a provider (`exn`, or a
                // prior file's `(# class … #)` base like `Attribute`).
                let resolveThroughProvider () =
                    // `inherit X` is a name WRITTEN AT A SITE, so it resolves through the same
                    // opens-aware engine as any written type name.
                    match
                        tryPickExternalWritten
                            ctx
                            (ctx.UseSiteAt diagKey)
                            (WrittenArity.Exact targs.Length)
                            (providerBaseOf targs.Length)
                            Qualifier.Bare
                            name
                    with
                    | ValueSome(ProviderBase.Class key) ->
                        ValueSome(BaseParentG.Class(NominalG.ofClass key (EqArray.ofList targs)))
                    | ValueSome(ProviderBase.HeritableCanon id) ->
                        ValueSome(BaseParentG.PrimitiveCanon(NominalG.ofConst id.Canon (EqArray.ofList targs)))
                    | ValueSome(ProviderBase.HeritablePlatform id) ->
                        match id.Platform with
                        | IntrinsicPlatform.Bound typeId -> typeIdToExternalBase typeId
                        | IntrinsicPlatform.Unsupported target ->
                            diagnose nameTok (Kind.UnsupportedOnTarget(name, target))
                            ValueNone
                    | ValueSome(ProviderBase.Interface key) -> reject (BaseVerdict.Interface key)
                    | ValueNone ->
                        // A name the name table knows at any arity is a project-local type of
                        // some other kind; one it does not know is unknown *here*, which
                        // includes a type declared below this group.
                        match TypeRegistry.tryTypeClaimAnyArity ctx.Types (ctx.UseSiteAt diagKey) name with
                        | ValueSome claim -> reject (BaseVerdict.NotAClass claim.Key)
                        | ValueNone -> reject (BaseVerdict.UnknownName name)

                match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info when info.IsInterface -> reject (BaseVerdict.Interface info.TypeKey)
                | ValueSome info -> ValueSome(BaseParentG.Class(NominalG.ofClass info.TypeKey (EqArray.ofList targs)))
                | ValueNone ->
                    // Heritable-local arm: a `(# class … #)` intrinsic of THIS file. One read
                    // yields both the type id and the `class`-tag verdict. An `inherit` parent
                    // is an ARBITRARY written name (a record, a typo, a provider class), so the
                    // name → key step must be allowed to miss here.
                    let heritableLocalTypeId =
                        match TypeRegistry.tryIntrinsicKeyOf ctx.Types name with
                        | ValueNone -> ValueNone
                        | ValueSome canon ->
                            match ctx.Types.IntrinsicBindings.TryGetValue canon with
                            | true, binding when binding.Heritable -> ValueSome binding.TypeId
                            | _ -> ValueNone

                    match heritableLocalTypeId with
                    // The EXTERNAL type the id denotes, not the opaque value `TyConst`.
                    | ValueSome typeId -> typeIdToExternalBase typeId
                    | ValueNone -> resolveThroughProvider ()
