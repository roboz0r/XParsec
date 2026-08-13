namespace XParsec.FSharp.SemanticAnalysis

// SEMANTIC typar-COUNT and typar-ORDER conformance between a `.fsi` and its `.fs`.

module ConformanceTypars =

    /// A binding whose `.fs`-inferred generic scheme disagrees with its `.fsi`-declared
    /// one: a different typar COUNT or a different typar ORDER.
    [<NoEquality; NoComparison>]
    type TyparMismatch =
        {
            /// The provider name the binding resolved under (e.g. `ListModule.fold`).
            Name: string
            Declared: FrozenType
            Inferred: FrozenType
        }

    /// Rewrite every `FTTypar` onto `axis`, index kept: a free value/function has exactly
    /// ONE axis, so `FTTypar(Declaring, i)` and `FTTypar(Method, i)` denote the same slot.
    let rec normAxisTo (axis: TyparAxis) (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(_, i) -> FTTypar(axis, i)
        | t -> FrozenType.mapChildren (normAxisTo axis) t

    let normAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Method t

    /// Land a FREE value/function's single typar axis on `Declaring`, the axis a provider
    /// scheme speaks, because instantiating one throws on a `Method` typar.
    let toDeclaringAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Declaring t

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes are α-equivalent WITH typar
    /// order: `FTTypar`'s index IS the quantification order, so axis-normalized structural
    /// equality fails exactly when a `.fs` `<'b,'a>` meets a `.fsi` `<'a,'b>`.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = normAxis declared = normAxis inferred

    /// The lookup names to try, most-specific first: the qualified compiled name
    /// (`ListModule.fold`), then the bare one, which covers a top-level binding.
    let private lookupNames (info: ModuleBindingInfo option) (name: string) : string list =
        match info with
        | Some mi ->
            match mi.DeclaringModule with
            | ValueSome m -> [ m.Name + "." + mi.Name; mi.Name ] |> List.distinct
            | ValueNone -> [ mi.Name ]
        | None -> [ name ]

    /// Check every NON-inline generic module binding of a frozen `.fs` file against the
    /// `.fsi` contract `provider`, in source-declaration order. A binding the provider does
    /// not publish, or a monomorphic one, has no typar order to compare and is skipped.
    let checkFile (provider: IExternalSymbolProvider) (pools: FrozenPools) : TyparMismatch list =
        let pool = TastPoolBuilder.openOver pools
        let moduleMembers = DenseTable.index pools.ModuleMembers

        [
            for decl in TastAccessor.roots pool do
                match decl with
                // An `inline` binding IS in `Decls`, and is excluded here: its body is
                // SRTP-solved per call site, so its typar order drives nothing.
                | TastAccessor.DLet {
                                        Pattern = TastAccessor.PNamed boundVar
                                        IsInline = false
                                        Ty = ty
                                    } ->
                    let info =
                        match moduleMembers.TryGetValue boundVar with
                        | true, mi -> Some mi
                        | _ -> None

                    // A binding inside a named module publishes under its COMPILED name
                    // (`[<CompiledName>]`), which only the member table knows.
                    let nameOpt =
                        match info with
                        | Some mi -> Some mi.Name
                        | None ->
                            match TastPoolBuilder.boundVarNaming pool boundVar with
                            | BoundVarNaming.Source n -> Some n
                            | BoundVarNaming.Minted _ -> None

                    match nameOpt with
                    | None -> ()
                    | Some name ->
                        let resolved =
                            lookupNames info name
                            |> List.tryPick (fun n ->
                                match provider.TryLookup n with
                                | ValueSome s -> Some(n, s)
                                | ValueNone -> None
                            )

                        match resolved with
                        | Some(n, sym) when sym.TyparArity > 0 ->
                            if not (schemesAgree sym.Scheme ty) then
                                yield
                                    {
                                        Name = n
                                        Declared = normAxis sym.Scheme
                                        Inferred = normAxis ty
                                    }
                        | _ -> ()
                | _ -> ()
        ]

    /// A generic type MEMBER whose `.fs`-inferred signature disagrees with every published
    /// `.fsi` overload of matching method arity.
    [<NoEquality; NoComparison>]
    type MemberMismatch =
        {
            /// The declaring type's compiled name (`Vesper.Formatter`).
            TypeName: string
            MemberName: string
            /// Always `> 0`: this pass only checks generic members.
            MethodTyparArity: int
            Inferred: FrozenType
            /// The matching-arity published overloads' signatures.
            Published: FrozenType list
        }

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

    /// Check every generic (method-owned-typar) MEMBER of a frozen `.fs` file against its
    /// `.fsi` contract `provider`: conformance holds when one published overload of the
    /// same name + method arity equals the inferred signature.
    let checkMembers (provider: IExternalSymbolProvider) (pools: FrozenPools) : MemberMismatch list =
        let pool = TastPoolBuilder.openOver pools

        [
            for decl in TastAccessor.roots pool do
                match TastAccessor.declKind decl with
                | DeclShape.Type ->
                    let td = TastAccessor.declType decl
                    let typeName = SymbolKeyOps.qualifiedName td.Key

                    for m in TTypeKindG.members td.Kind do
                        let arity = m.MethodTypeParams.Length

                        if arity > 0 then
                            let isProperty = (m.Kind = TMemberKind.Property)
                            let inferred = memberSigOf isProperty (tupledParams m.Params) m.ReturnTy
                            let overloads = provider.TryLookupMembers(td.Key, m.Name)
                            // A different-arity overload is a different generic member.
                            let candidates = overloads |> EqArray.filter (fun em -> em.MethodTyparArity = arity)

                            // No matching-arity overload at all is member PRESENCE, not a
                            // typar-order disagreement, so it is skipped.
                            if
                                candidates.Length > 0
                                && not (candidates |> EqArray.exists (fun em -> extractedSigOf em = inferred))
                            then
                                yield
                                    {
                                        TypeName = typeName
                                        MemberName = m.Name
                                        MethodTyparArity = arity
                                        Inferred = inferred
                                        Published = [ for em in candidates -> extractedSigOf em ]
                                    }
                | _ -> ()
        ]
