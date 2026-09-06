namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// SEMANTIC typar-COUNT and typar-ORDER conformance between a `.fsi` and its `.fs`.

module ConformanceTypars =

    /// A binding whose `.fs`-inferred generic scheme disagrees with its `.fsi`-declared
    /// one: a different typar COUNT or a different typar ORDER.
    [<NoEquality; NoComparison>]
    type TyparMismatch =
        {
            /// The binding key's qualified rendering, for the message alone
            /// (`Vesper.ListModule.fold`).
            Name: string
            Declared: FrozenType
            Inferred: FrozenType
        }

    /// One binding's disagreement as a compiler message.
    let describe (m: TyparMismatch) : string =
        sprintf
            "%s: declared %s, but the implementation infers %s"
            m.Name
            (Conformance.describeType m.Declared)
            (Conformance.describeType m.Inferred)

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes agree WITH typar order. Both
    /// are written under the binding's `ModuleFunction` scope, so a `.fs` `<'b,'a>` against a
    /// `.fsi` `<'a,'b>` fails.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = declared = inferred

    /// Check every generic module binding of a frozen implementation file against the contract
    /// `provider`, in source-declaration order. A binding the provider does not publish, or a
    /// monomorphic one, has no typar order to compare and is skipped.
    ///
    /// `inline` included: a binding's typars are the slots a splice fills, so a body folding
    /// `^T1 -> ^T2 -> ^T3` into one `^T` binds `y` at `x`'s type.
    let checkFile (provider: IExternalSymbolProvider) (pools: FrozenPools) : TyparMismatch list =
        let pool = TastPoolBuilder.openOver pools

        let check (boundVar: BoundVarId) (ty: FrozenType) : TyparMismatch voption =
            let key = (TastPoolBuilder.moduleMemberOf pool boundVar).BindingKey

            match provider.TryLookupByKey key with
            | ValueSome sym when sym.TyparArity > 0 && not (schemesAgree sym.Scheme ty) ->
                ValueSome
                    {
                        Name = SymbolKeyOps.qualifiedBindingName key
                        Declared = sym.Scheme
                        Inferred = ty
                    }
            | _ -> ValueNone

        [
            for m in TastAccessor.rootBindings pool do
                match m.Pattern with
                | TastAccessor.PNamed boundVar ->
                    match check boundVar m.Ty with
                    | ValueSome mismatch -> yield mismatch
                    | ValueNone -> ()
                | _ -> ()
        ]

    let private tupledParams (ps: EqArray<BoundVarKeyG<'id> * FrozenType>) : FrozenType =
        ExternalSignature.tupledParams (EqArray.map snd ps)

    /// The single-`FrozenType` shape both the `.fs` member and the `.fsi` overload fold
    /// to, so a `=` is the conformance check.
    let private memberSigOf (isProperty: bool) (parameters: FrozenType) (ret: FrozenType) : FrozenType =
        if isProperty then ret else FTFun(parameters, ret)

    /// The published half folded to the ONE .NET parameter slot it compiles to, curried groups
    /// and all: a `.fs` binding flattens its curried patterns to one parameter vector, so
    /// grouping is not what this pass compares. Typar ORDER is.
    let private extractedSigOf (m: ExternalMember) : FrozenType =
        memberSigOf m.IsValueMember (ExternalSignature.tupledParameters m.Signature) m.Signature.Return

    /// A generic implementation member's own scope, name, arity and folded signature.
    [<NoEquality; NoComparison>]
    type private ImplMember =
        {
            Scope: TyparScope
            Name: string
            MethodTyparArity: int
            Inferred: FrozenType
        }

    let private implMemberOf (declKey: TypeKey) (m: TastAccessor.TypeMember) : ImplMember =
        let isProperty = (m.Kind = TMemberKind.Property)

        {
            Scope = TyparScope.Member(declKey, m.Ordinal)
            Name = m.Name
            MethodTyparArity = m.MethodTypeParams.Length
            Inferred = memberSigOf isProperty (tupledParams m.Params) m.ReturnTy
        }

    /// Every generic member of the implementation, by declaring type, in declaration order.
    let private implGenericMembers (pools: FrozenPools) : Dictionary<TypeKey, ImplMember list> =
        let pool = TastPoolBuilder.openOver pools
        let byType = Dictionary<TypeKey, ImplMember list>(HashIdentity.Structural)

        for decl in TastAccessor.roots pool do
            match TastAccessor.declKind decl with
            | DeclShape.Type ->
                let td = TastAccessor.declType decl

                byType.[td.TypeKey] <-
                    [
                        for m in TTypeKindG.members td.Kind do
                            if m.MethodTypeParams.Length > 0 then
                                implMemberOf td.TypeKey m
                    ]
            | _ -> ()

        byType

    /// Whether the published overload `em` agrees with the implementation member `m` on
    /// shape and typar order. A signature file and its implementation number their members
    /// independently, so `em`'s own typars are read under `m`'s scope.
    let private conforms (em: ExternalMember) (m: ImplMember) : bool =
        em.Name = m.Name
        && em.Signature.MethodTyparArity = m.MethodTyparArity
        && FrozenType.rescopeMemberTypars m.Scope (extractedSigOf em) = m.Inferred

    /// `em` with its own typars rewritten under `scope`, its key included.
    let private rescopeMember (scope: TyparScope) (em: ExternalMember) : ExternalMember =
        let f = FrozenType.rescopeMemberTypars scope
        let s = em.Signature

        { em with
            Signature =
                { s with
                    MethodTypars = EqArray.map (ValueOption.map f) s.MethodTypars
                    ArgGroups = EqArray.map f s.ArgGroups
                    Return = f s.Return
                }
            Key =
                { em.Key with
                    ArgSig = EqArray.map f em.Key.ArgSig
                }
        }

    /// A signature file's surface with each generic member's own typars re-scoped onto the
    /// conforming implementation member, whose numbering the emitted members and later files'
    /// calls carry. A member with no conforming implementation keeps the signature's scope.
    let rescopeToImplementation (surface: PublishedSurface) (pools: FrozenPools) : PublishedSurface =
        let byType = implGenericMembers pools

        let rescope (declKey: TypeKey) (em: ExternalMember) : ExternalMember =
            if em.Signature.MethodTyparArity = 0 then
                em
            else
                match byType.TryGetValue declKey with
                | true, members ->
                    match members |> List.tryFind (conforms em) with
                    | Some m -> rescopeMember m.Scope em
                    | None -> em
                | _ -> em

        let rescopeShape (declKey: TypeKey) (shape: ExternalTypeShape) : ExternalTypeShape =
            match shape with
            | ExternalTypeShape.Class c ->
                ExternalTypeShape.Class
                    { c with
                        Members = EqArray.map (rescope declKey) c.Members
                    }
            | ExternalTypeShape.Intrinsic({ Class = ValueSome cs } as i) ->
                ExternalTypeShape.Intrinsic
                    { i with
                        Class =
                            ValueSome
                                { cs with
                                    Members = EqArray.map (rescope declKey) cs.Members
                                }
                    }
            | other -> other

        { surface with
            ShapesByKey =
                surface.ShapesByKey
                |> EqArray.map (fun e ->
                    { e with
                        Value = rescopeShape e.Key e.Value
                    }
                )
            MembersByKey =
                surface.MembersByKey
                |> EqArray.map (fun e ->
                    { e with
                        Value = EqArray.map (rescope e.Key) e.Value
                    }
                )
        }
