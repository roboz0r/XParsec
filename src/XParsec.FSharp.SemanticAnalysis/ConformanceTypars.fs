namespace XParsec.FSharp.SemanticAnalysis

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

    /// Rewrite every `FTTypar` onto `axis`, index kept: a free value/function has exactly
    /// ONE axis, so `FTTypar(Declaring, i)` and `FTTypar(Method, i)` denote the same slot.
    let rec normAxisTo (axis: TyparAxis) (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(_, i) -> FTTypar(axis, i)
        | t -> FrozenType.mapChildren (normAxisTo axis) t

    let normAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Method t

    /// Land a FREE value/function's single typar axis on `Declaring`, the axis a provider
    /// scheme uses; instantiating one throws on a `Method` typar.
    let toDeclaringAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Declaring t

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes are α-equivalent WITH typar
    /// order: `FTTypar`'s index IS the quantification order, so axis-normalized structural
    /// equality fails exactly when a `.fs` `<'b,'a>` meets a `.fsi` `<'a,'b>`.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = normAxis declared = normAxis inferred

    /// Check every generic module binding of a frozen implementation file against the contract
    /// `provider`, in source-declaration order. A binding the provider does not publish, or a
    /// monomorphic one, has no typar order to compare and is skipped.
    ///
    /// `inline` included: a binding's typars are the slots a splice fills, so a body folding
    /// `^T1 -> ^T2 -> ^T3` into one `^T` binds `y` at `x`'s type.
    let checkFile (provider: IExternalSymbolProvider) (pools: FrozenPools) : TyparMismatch list =
        let pool = TastPoolBuilder.openOver pools

        [
            for decl in TastAccessor.roots pool do
                match decl with
                | TastAccessor.DLet {
                                        Pattern = TastAccessor.PNamed boundVar
                                        Ty = ty
                                    } ->
                    let key = (TastPoolBuilder.moduleMemberOf pool boundVar).BindingKey

                    match provider.TryLookupByKey key with
                    | ValueSome sym when sym.TyparArity > 0 ->
                        if not (schemesAgree sym.Scheme ty) then
                            yield
                                {
                                    Name = SymbolKeyOps.qualifiedBindingName key
                                    Declared = normAxis sym.Scheme
                                    Inferred = normAxis ty
                                }
                    | _ -> ()
                | _ -> ()
        ]
